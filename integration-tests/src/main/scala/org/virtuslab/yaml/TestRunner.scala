package org.virtuslab.yaml

import scala.annotation.tailrec
import scala.collection.mutable

import org.virtuslab.yaml.Tag
import org.virtuslab.yaml.internal.load.parse.Anchor
import org.virtuslab.yaml.internal.load.parse.EventKind
import org.virtuslab.yaml.internal.load.parse.EventKind._
import org.virtuslab.yaml.internal.load.parse.NodeEventMetadata
import org.virtuslab.yaml.internal.load.parse.ParserImpl
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.Tokenizer

trait TestRunner {
  def inYaml: String
  def expectedEvents: String

  def run(): RunnerResult = {
    val reader = Tokenizer.make(inYaml)
    val parser = ParserImpl(reader)
    val acc    = new mutable.ArrayDeque[EventKind]()

    @tailrec
    def loop(): RunnerResult = {
      parser.getNextEvent() match {
        case Right(event) =>
          acc.append(event.kind)
          if (event.kind != EventKind.StreamEnd) loop()
          else RunnerResult(acc.toList, expectedEvents)
        case Left(error) =>
          RunnerResult(acc.toList, expectedEvents, error)
      }
    }
    loop()
  }

}

object TestRunnerUtils {

  private def anchorAsString(anchor: Option[Anchor]): String =
    anchor.map(a => s" &${a.anchor}").getOrElse("")

  private def tagAsString(tag: Option[Tag]): String =
    tag.map(t => s" <${t.value}>").getOrElse("")

  private def metadataAsString(metadata: NodeEventMetadata): String =
    anchorAsString(metadata.anchor) + tagAsString(metadata.tag)

  /** Escapes a scalar value the way yaml-test-suite's test.event files do. */
  def escapeValue(value: String): String = {
    val sb = new java.lang.StringBuilder(value.length)
    value.foreach {
      case '\\' => sb.append("\\\\")
      case '\b' => sb.append("\\b")
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case c    => sb.append(c)
    }
    sb.toString
  }

  /** Formats one event as a line of yaml-test-suite's test.event format. */
  def formatEvent(event: EventKind): String = event match {
    case StreamStart             => "+STR"
    case StreamEnd               => "-STR"
    case DocumentStart(explicit) => if (explicit) "+DOC ---" else "+DOC"
    case DocumentEnd(explicit)   => if (explicit) "-DOC ..." else "-DOC"
    case SequenceStart(data, flow) =>
      s"+SEQ${if (flow) " []" else ""}${metadataAsString(data)}"
    case SequenceEnd => "-SEQ"
    case MappingStart(data, flow) =>
      s"+MAP${if (flow) " {}" else ""}${metadataAsString(data)}"
    case MappingEnd   => "-MAP"
    case Alias(alias) => s"=ALI *${alias.anchor}"
    case Scalar(value, style, data) =>
      val indicator = style match {
        case ScalarStyle.Plain        => ":"
        case ScalarStyle.DoubleQuoted => "\""
        case ScalarStyle.SingleQuoted => "'"
        case ScalarStyle.Folded       => ">"
        case ScalarStyle.Literal      => "|"
      }
      s"=VAL${metadataAsString(data)} $indicator${escapeValue(value)}"
  }

  def convertEventToYamlTestSuiteFormat(events: Seq[EventKind]): String =
    events.map(formatEvent).mkString("\n")

}

case class K8sYamlTestRunner(yamlPath: os.Path, libYaml: os.Path) extends TestRunner {
  override val inYaml = os.read(yamlPath)
  override val expectedEvents = os
    .proc(libYaml, yamlPath)
    .call(cwd = os.pwd)
    .out
    .text()
    .trim

  override def run(): RunnerResult = {
    println(yamlPath)
    super.run()
  }

}
