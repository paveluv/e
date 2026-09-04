# Repository rules

- Before every commit, run `tools/scheme-format -i` on all `*.e` files.
- Temporary scripts (file surgery, probes, one-off drivers) are
  written in Scheme, not Python or other languages. Committed test
  fixtures may shell out where a fixture genuinely needs another
  runtime, but reach for Scheme first.
- Seam modules are named in the singular (`style`, `file`, `mode`,
  `string`, `actor`, `doc`), are imported with their own prefix, and
  their exported names never repeat the module's stem: `style:set!`,
  `log:add!`, `mode:register!` -- never `styles:set-style!`.  Rename in
  the export list (`(rename (internal external))`) if the definition
  keeps a longer name.
