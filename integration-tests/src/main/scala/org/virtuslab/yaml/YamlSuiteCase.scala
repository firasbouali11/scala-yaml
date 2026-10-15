package org.virtuslab.yaml

/**
 * One case of the vendored yaml-test-suite data release (integration-tests/yaml-test-suite).
 *
 * A case is a directory holding `in.yaml` and `test.event`, either at the top level (`229Q`) or
 * one level down for releases that split a test into subcases (`2G84/00`). A case with an
 * `error` file must be rejected by the parser.
 */
final case class YamlSuiteCase(
    id: String,
    tags: List[String],
    inYaml: String,
    expectedEvents: List[String],
    isError: Boolean
)

object YamlSuiteCase {

  def loadAll(suiteDir: os.Path): List[YamlSuiteCase] = {
    val tags = loadTags(suiteDir / "tags.txt")
    caseDirs(suiteDir).map { case (id, dir) =>
      YamlSuiteCase(
        id = id,
        tags = tags.getOrElse(id, Nil),
        inYaml = os.read(dir / "in.yaml"),
        expectedEvents = os.read.lines(dir / "test.event").toList.filter(_.nonEmpty),
        isError = os.exists(dir / "error")
      )
    }
  }

  private def isCaseDir(dir: os.Path): Boolean = os.isFile(dir / "in.yaml")

  private def caseDirs(suiteDir: os.Path): List[(String, os.Path)] =
    os.list(suiteDir)
      .filter(os.isDir)
      .sortBy(_.last)
      .toList
      .flatMap { dir =>
        if (isCaseDir(dir)) List(dir.last -> dir)
        else
          os.list(dir)
            .filter(sub => os.isDir(sub) && isCaseDir(sub))
            .sortBy(_.last)
            .map(sub => s"${dir.last}/${sub.last}" -> sub)
      }

  /** tags.txt holds one line per case: the id followed by its space-separated tags. */
  private def loadTags(file: os.Path): Map[String, List[String]] =
    os.read
      .lines(file)
      .map(_.trim)
      .filter(_.nonEmpty)
      .map { line =>
        val parts = line.split(' ').toList
        parts.head -> parts.tail
      }
      .toMap
}
