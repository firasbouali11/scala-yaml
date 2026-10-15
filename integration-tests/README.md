### Test suite

The [yaml-test-suite](https://github.com/yaml/yaml-test-suite) is used in integration testing.
A data release is vendored in `yaml-test-suite/` (see `yaml-test-suite/VERSION` for the tag and
commit). The release's `tags/` symlink tree is flattened into `yaml-test-suite/tags.txt`.

`sbt "integration/testOnly org.virtuslab.yaml.OfficialTestSuiteSpec"` runs every case and writes
`target/yaml-suite-results.json`, giving each case one status: `pass`, `events-differ`,
`wrongly-rejected`, `wrongly-accepted` or `crash`. A case with an `error` file passes when the
parser rejects it.

`loop/ratchet.txt` (repo root) lists the ids that pass, sorted with `LC_ALL=C`. The spec fails if
any of them stops passing. Run with `UPDATE_RATCHET=1` to add newly passing ids.
