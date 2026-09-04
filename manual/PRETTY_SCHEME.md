# Pretty Scheme display

Pretty Scheme changes how delimiters look without changing buffer text or the
saved file. It is presentation, not source transformation.

## Structural clusters

`pretty-scheme-clusters!` assigns Unicode delimiter pairs by construct:

```scheme
｢define (twice x)
  ⟨let ([y (* x 2)])
    ⦅if (odd? y) y ⟦begin x⟧⦆⟩｣
```

Definitions, lambdas, binding forms, conditionals, control forms, iteration,
syntax, modules, quotation, mutation, and delay each have a distinct pair.
Applications and clause brackets remain plain, keeping structure visible
without turning every delimiter into decoration.

The display follows edits immediately. Completing or changing an operator can
change its cluster, while the underlying ASCII parentheses remain untouched.
Typing `)` or `]` closes the innermost construct using the source delimiter
that opened it. The status line reports the actual source character beneath
point.

The cluster palette is literal data near the top of `lib/pretty-scheme.e`.
Editing and saving it hot-reloads the module and restyles visible buffers.

## Depth variants

`pretty-scheme-depth!` assigns delimiter pairs by nesting depth, cycling after
the palette is exhausted.

`pretty-scheme-rainbow!` keeps ordinary delimiter characters but colors each
depth through a seven-color sequence. Closing delimiters match their opener.

Invoking the active variant again returns the buffer to normal Scheme display.

## Semantic symbol styling

Scheme buffers also distinguish symbol provenance. Names unknown to the
standard language are italic, covering locals, program-defined names, and
possible typos. In editor evaluation contexts, published e symbols use the
editor face. Thus `M-x` presents standard Scheme plainly, editor API names in
the editor style, and incomplete or unknown names in italics.

These faces are customizable through the [style DSL](STYLES.md).

