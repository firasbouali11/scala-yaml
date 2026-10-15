package org.virtuslab.yaml

/**
 * Runs the vendored yaml-test-suite release and enforces a ratchet: every id listed in
 * loop/ratchet.txt must keep passing. Cases outside the ratchet may fail.
 *
 * Results for every case are written to integration-tests/target/yaml-suite-results.json.
 * Run with UPDATE_RATCHET=1 to add newly passing ids to the ratchet. Ids are never removed
 * automatically; dropping one is a deliberate, manual edit.
 */
class OfficialTestSuiteSpec extends munit.FunSuite {

  private val repoRoot: os.Path =
    Iterator
      .iterate(os.pwd)(_ / os.up)
      .take(os.pwd.segmentCount + 1)
      .find(dir => os.isFile(dir / "integration-tests" / "yaml-test-suite" / "VERSION"))
      .getOrElse(sys.error(s"Cannot find integration-tests/yaml-test-suite above ${os.pwd}"))

  private val suiteDir    = repoRoot / "integration-tests" / "yaml-test-suite"
  private val ratchetFile = repoRoot / "loop" / "ratchet.txt"
  private val resultsFile = repoRoot / "integration-tests" / "target" / "yaml-suite-results.json"

  private lazy val results: List[CaseResult] =
    YamlSuiteCase.loadAll(suiteDir).map(c => YamlSuiteRunner.run(c))

  private def readRatchet(): List[String] =
    if (os.exists(ratchetFile)) os.read.lines(ratchetFile).map(_.trim).filter(_.nonEmpty).toList
    else Nil

  test("write yaml-suite-results.json") {
    os.write.over(resultsFile, YamlSuiteRunner.toJson(results), createFolders = true)

    val byStatus = results.groupBy(_.status.name).map { case (k, v) => k -> v.size }
    val summary = List("pass", "events-differ", "wrongly-rejected", "wrongly-accepted", "crash")
      .map(s => f"  $s%-17s ${byStatus.getOrElse(s, 0)}")
      .mkString("\n")
    println(s"\nyaml-test-suite: ${results.size} cases\n$summary\nresults: $resultsFile\n")
  }

  test("ratchet: every id in loop/ratchet.txt still passes") {
    val ratchet = readRatchet()
    val passing = results.filter(_.status == CaseStatus.Pass).map(_.id).toSet
    val known   = results.map(r => r.id -> r).toMap

    val updating = sys.env.get("UPDATE_RATCHET").exists(_.nonEmpty)
    if (updating) {
      val updated = (ratchet.toSet ++ passing).toList.sorted
      os.write.over(ratchetFile, updated.mkString("", "\n", "\n"), createFolders = true)
      println(s"ratchet: ${updated.size - ratchet.toSet.size} id(s) added, ${updated.size} total")
    }

    // String ordering on these ASCII ids matches `LC_ALL=C sort`.
    assertEquals(ratchet, ratchet.distinct.sorted, s"$ratchetFile must be sorted and unique")

    val unknown = ratchet.filterNot(known.contains)
    assert(
      unknown.isEmpty,
      s"ids in $ratchetFile that are not in the suite: ${unknown.mkString(", ")}"
    )

    val regressions = ratchet.filterNot(passing).map { id =>
      val r = known(id)
      s"  $id: ${r.status.name}: ${r.detail}"
    }
    assert(
      regressions.isEmpty,
      s"${regressions.size} ratcheted case(s) no longer pass:\n${regressions.mkString("\n")}"
    )

    val newlyPassing = (passing -- ratchet).toList.sorted
    if (newlyPassing.nonEmpty && !updating)
      println(
        s"${newlyPassing.size} passing case(s) not in the ratchet (UPDATE_RATCHET=1 adds them): " +
          newlyPassing.mkString(" ")
      )
  }
}
