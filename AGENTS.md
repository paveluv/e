# Repository rules

- Before every commit, run `tools/scheme-format -i` on all `*.e` files.
- Temporary scripts (file surgery, probes, one-off drivers) are
  written in Scheme, not Python or other languages. Committed test
  fixtures may shell out where a fixture genuinely needs another
  runtime, but reach for Scheme first.
