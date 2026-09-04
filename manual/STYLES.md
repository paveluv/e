# Styles

e renders semantic face names such as `comment`, `keyword`, and `selection`.
Modes assign those names to text; `style:set!` controls how a terminal displays
each name. Put overrides in `config.e` so they are reapplied at startup and
after module reloads.

## Style expressions

The preferred form is a quoted list of attributes and color clauses:

```scheme
(style:set! 'comment '((foreground bright-black) italic))
(style:set! 'keyword '(bold (foreground cyan)))
(style:set! 'selection '((background 24) (foreground white)))
(style:set! 'editor '((foreground (rgb 170 110 220)) bold))
```

Order is preserved when the expression is compiled to terminal SGR parameters.
Most styles contain at most one foreground, one background, and any desired
attributes.

### Attributes

The available attribute symbols are:

- `reset`
- `bold`
- `dim`
- `italic`
- `underline`
- `blink`
- `reverse`
- `hidden`
- `strike`
- `double-underline`
- `curly-underline`, `dotted-underline`, `dashed-underline`
- `overline`
- `framed`, `encircled`
- `superscript`, `subscript`

Each attribute has a cancellation for overlay faces that must remove
what the underlying style set: `normal-intensity` (clears bold and dim),
`no-italic`, `no-underline`, `no-blink`, `no-reverse`, `no-hidden`,
`no-strike`, `no-frame`, and `no-overline`.

Terminal support varies. In particular, some terminals render `bold` as a
brighter color, ignore `blink`, or do not expose italic and strike-through.
The underline variants and `underline-color` need a terminal with styled
underlines (kitty, foot, WezTerm, recent VTE); elsewhere they degrade to
a plain underline or to nothing. `framed` and `encircled` (boxes around
the cells) are the rarest: most terminals ignore them.

### Colors

Use `(foreground color)` and `(background color)`. The shorter names `fg` and
`bg` are equivalent.

The eight named colors are `black`, `red`, `green`, `yellow`, `blue`,
`magenta`, `cyan`, and `white`. Prefix any with `bright-`, for example
`bright-black` or `bright-cyan`.

An integer from 0 through 255 selects the terminal's 256-color palette:

```scheme
(style:set! 'delimiter '((foreground 245)))
```

The symbol `default` restores the terminal's own foreground or
background. `(underline-color color)` takes the same color forms and
sets the underline's color separately from the text, with
`(underline-color default)` restoring it.

An RGB color contains three integers from 0 through 255:

```scheme
(style:set! 'string '((foreground (rgb 100 210 130))))
(style:set! 'selection '((background (rgb 30 55 90))))
```

RGB requires true-color terminal support. The actual appearance of named and
palette colors is controlled by the terminal's theme.

## Faces

The built-in faces available to `style:set!` are:

| Face | Default expression | Used for |
| --- | --- | --- |
| `plain` | `(reset)` | Ordinary text and fallback rendering |
| `chrome` | `((foreground bright-black))` | Prompt labels, ghost text, log prefixes, and quiet UI furniture |
| `comment` | `((foreground bright-black))` | Source comments and Markdown block quotes |
| `string` | `((foreground green))` | Strings and Markdown code |
| `keyword` | `(bold (foreground cyan))` | Language keywords and Markdown headings |
| `number` | `((foreground magenta))` | Numeric syntax |
| `literal` | `(bold (foreground magenta))` | Self-evaluating and constant syntax |
| `delimiter` | `((foreground 245))` | Parentheses and other structural delimiters |
| `editor` | `((foreground 135))` | e-specific symbols in eval and completion contexts |
| `quote` | `((foreground cyan))` | Quote syntax |
| `bold` | `(bold)` | Markdown bold and generic emphasis |
| `italic` | `(italic)` | Markdown italics and incomplete symbols |
| `rainbow1` … `rainbow7` | Foregrounds `196`, `208`, `220`, `40`, `33`, `57`, `129` | Pretty-Scheme nesting colors |
| `mark` | `(underline)` | Generic highlighted ranges, including matching delimiters |
| `selection` | `((background blue))` | The active selected region |
| `active` | `((background 24))` | The active row in an app buffer |
| `active-shadow` | `((background 31))` | A lighter blue app row tracking the focused buffer while the app is inactive |
| `choice` | `(bold (foreground 135))` | The initial letters of choices in focused dialog prompts |
| `match` | `((background cyan) (foreground black))` | Incremental-search matches |
| `match-point` | `((background yellow) (foreground black))` | The current incremental-search match |

A mode or extension may use additional face symbols. Unknown faces fall back
to `plain`; they can still be assigned an override with `style:set!` before or
after the extension is loaded.

## Compilation and compatibility

`style:compile` validates a style expression and returns its raw SGR parameter
string without the escape introducer:

```scheme
(style:compile '((foreground 244) italic))
;; => "38;5;244;3"
```

Invalid attributes, malformed clauses, and color components outside 0…255
raise an error. An empty expression compiles to reset (`"0"`).

For compatibility, `style:set!` still accepts a number as a 256-color
foreground or a raw SGR parameter string:

```scheme
(style:set! 'comment 244)
(style:set! 'comment "38;5;244;3")
```

New configuration should use style expressions; they are validated and do not
require memorizing SGR numeric codes.

## Configuration lifecycle

Style overrides participate in e's owned registration system. A call from
`config.e` belongs to that configuration load. Removing the call and reloading
the configuration restores the built-in style instead of leaving a stale
override behind.

To experiment for the current session, evaluate a `style:set!` call with M-x.
Run `(main:load-config!)` afterward to restore the choices in `config.e`.

`style:set!` changes future rendering immediately; the next redraw applies it
to buffers, prompts, log views, selections, and search highlights.
