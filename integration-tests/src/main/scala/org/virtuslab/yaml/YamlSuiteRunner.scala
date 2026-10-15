package org.virtuslab.yaml

import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.control.NonFatal

import org.virtuslab.yaml.internal.load.parse.EventKind
import org.virtuslab.yaml.internal.load.parse.ParserImpl
import org.virtuslab.yaml.internal.load.reader.Tokenizer

sealed abstract class CaseStatus(val name: String)
object CaseStatus {

  /** Events match, or the case expects an error and the parser rejected the input. */
  case object Pass extends CaseStatus("pass")

  /** The parser accepted valid input but produced different events. */
  case object EventsDiffer extends CaseStatus("events-differ")

  /** The parser returned a YamlError for valid input. */
  case object WronglyRejected extends CaseStatus("wrongly-rejected")

  /** The case expects an error but the parser accepted the input. */
  case object WronglyAccepted extends CaseStatus("wrongly-accepted")

  /** The parser threw something other than a YamlError, ran away, or timed out. */
  case object Crash extends CaseStatus("crash")
}

final case class CaseResult(id: String, tags: List[String], status: CaseStatus, detail: String)

object YamlSuiteRunner {

  private val maxEvents = 100000

  private sealed trait Outcome
  private final case class Accepted(events: List[String])                   extends Outcome
  private final case class Rejected(events: List[String], error: YamlError) extends Outcome
  private final case class Crashed(reason: String)                          extends Outcome

  // Daemon threads, so that a parser stuck in an infinite loop can't keep the JVM alive.
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(
    Executors.newCachedThreadPool(new ThreadFactory {
      def newThread(r: Runnable): Thread = {
        val t = new Thread(r, "yaml-suite-runner")
        t.setDaemon(true)
        t
      }
    })
  )

  def run(c: YamlSuiteCase, timeout: FiniteDuration = 10.seconds): CaseResult = {
    val outcome =
      try Await.result(Future(parse(c.inYaml)), timeout)
      catch { case _: TimeoutException => Crashed(s"timed out after $timeout") }

    def result(status: CaseStatus, detail: String) = CaseResult(c.id, c.tags, status, detail)

    outcome match {
      case Crashed(reason) => result(CaseStatus.Crash, reason)
      case Rejected(_, error) =>
        if (c.isError) result(CaseStatus.Pass, oneLine(error.msg))
        else result(CaseStatus.WronglyRejected, oneLine(error.msg))
      case Accepted(_) if c.isError =>
        result(CaseStatus.WronglyAccepted, "parser accepted input that should be rejected")
      case Accepted(events) =>
        firstDifference(c.expectedEvents, events) match {
          case None       => result(CaseStatus.Pass, "")
          case Some(diff) => result(CaseStatus.EventsDiffer, diff)
        }
    }
  }

  private def parse(yaml: String): Outcome =
    try {
      val parser = ParserImpl(Tokenizer.make(yaml))
      val acc    = new mutable.ListBuffer[String]

      @tailrec
      def loop(): Outcome =
        if (acc.size >= maxEvents) Crashed(s"runaway: more than $maxEvents events")
        else
          parser.getNextEvent() match {
            case Right(event) =>
              acc += TestRunnerUtils.formatEvent(event.kind)
              if (event.kind == EventKind.StreamEnd) Accepted(acc.toList) else loop()
            case Left(error) => Rejected(acc.toList, error)
          }
      loop()
    } catch {
      case e: StackOverflowError => Crashed("StackOverflowError")
      case NonFatal(e)           => Crashed(oneLine(s"${e.getClass.getName}: ${e.getMessage}"))
    }

  private def firstDifference(expected: List[String], obtained: List[String]): Option[String] = {
    val n = math.max(expected.size, obtained.size)
    (0 until n).collectFirst {
      case i if expected.lift(i) != obtained.lift(i) =>
        val exp = expected.lift(i).getOrElse("<end>")
        val got = obtained.lift(i).getOrElse("<end>")
        s"event ${i + 1}: expected `$exp`, got `$got`"
    }
  }

  private def oneLine(s: String): String = {
    val flat = String.valueOf(s).split("\\s*[\\r\\n]+\\s*").filter(_.nonEmpty).mkString(" ")
    if (flat.length <= 300) flat else flat.take(297) + "..."
  }

  def toJson(results: Seq[CaseResult]): String = {
    def str(s: String): String = {
      val sb = new StringBuilder("\"")
      s.foreach {
        case '"'          => sb.append("\\\"")
        case '\\'         => sb.append("\\\\")
        case '\n'         => sb.append("\\n")
        case '\r'         => sb.append("\\r")
        case '\t'         => sb.append("\\t")
        case c if c < ' ' => sb.append(f"\\u${c.toInt}%04x")
        case c            => sb.append(c)
      }
      sb.append('"').toString
    }
    results
      .map { r =>
        s"""  {"id": ${str(r.id)}, "tags": [${r.tags.map(str).mkString(", ")}], """ +
          s""""status": ${str(r.status.name)}, "detail": ${str(r.detail)}}"""
      }
      .mkString("[\n", ",\n", "\n]\n")
  }
}
