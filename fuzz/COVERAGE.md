# Invariants and their properties

Each invariant an interface states, and the Alcobar property that checks it
(`suite / test`). `dune build @fuzz/runtest` runs them briefly; `dune build
@fuzz --profile=afl` explores.

| Interface | Invariant | Property |
|---|---|---|
| `Span` | a span only holds printable text | span / every span prints |
| `Span.width` | `width (s0 ++ s1) <= width s0 + width s1` | span / width is subadditive |
| `Span.width` | a style boundary adds no cell | span / styles add no cell |
| `Span.truncate` | fits `w`, identity when it fits, cut lines keep a prefix and end with an ellipsis, idempotent | span / truncate fits, keeps a prefix, ends with an ellipsis |
| `Span.truncate`, `Span.pp` | every style turned on is turned off | span / truncate turns styles off |
| `Span.wrap` | never empty, every line fits, non-space text kept | span / wrap fits and keeps the text |
| `Span.wrap` | refuses a non-positive width; any bytes | span / wrap refuses a non-positive width; wrap takes any bytes |
| `Span.link` | refuses an unsafe URI; prints its label alone | span / link |
| `Span.concat` | `sep` between each two | span / concat |
| `Width` (private) | the width is the column a terminal reaches | width / matches the terminal |
| `Console_text.sanitize`, `strip` | printable result, idempotent, `strip` leaves no escape | console_text / sanitize and strip |
| `Console_text.sanitize_styles` | leaves no colour on after it | console_text / sanitize_styles leaves nothing on |
| `Color.rgb`, `Color.palette` | accept exactly `[0;255]` | color / rgb range; palette range |
| `Color.of_hex`, `hex`, `pp` | agree; `pp` of RGB reads back | color / hex |
| `Color.blend`, `scale` | stay in range, saturate, blend's ends | color / blend and scale stay in range |
| `Color.downsample` | identity at true colour, palette or named result, idempotent | color / downsample |
| `Color.depth`, `set_depth` | the depth last set | color / formatter depth |
| `Style.( + )` | unit, associative, right colour wins | style / composition laws |
| `Border`, `Guide` | accessors give back what was given | border / accessors; guide / accessors |
| `Spinner.frame` | one of its frames, the first before time; refusals | spinner / frame |
| `Theme.v`, `Theme.mark`, `Theme.of_string` | parts kept, the muted colour among them, headers set apart, palette never empty, a mark is one line; `of_string` reads back exactly the names of `Theme.all` | theme / parts and marks |
| every widget, `Display` | on a formatter that is not UTF-8, ASCII alone in every theme, for any ASCII text and margin | theme / ASCII when not UTF-8 |
| `Bar.v` | exactly `width` cells, two more with brackets | bar / width |
| `Rule.pp` | exactly the margin; refused glyphs | rule / as wide as the margin |
| `Panel.pp` | fits the margin; a fixed panel is a rectangle | panel / fits the margin; refuses a box too small |
| `Table.pp` | fits the margin, framed lines of one width, styles closed | table / fits the margin |
| `Table` | right-aligned cells line up; row shape | table / right-aligned cells line up; row shape |
| `Tree.pp` | fits the margin | tree / fits the margin |
| `Input.feed` | whole or in pieces alike, no escape in the echo, masked keeps no history | input / chunking and masking |
| `Display` | header on screen exactly once after every write (no blank, half-drawn or doubled region, nothing left after a resize, never taller than the screen); a redraw changes only the region's rows; every line written fits; styles closed per line; an idle redraw writes nothing; cursor visible at the start of a line once closed | display / screen guarantees |
| `Display` | without a terminal: no escape, header last | display / plain output |
| `Display` | on a terminal, each write is one synchronized update, opened and closed once at its ends | display / screen guarantees |
| `Display` | on a terminal wider than 100 columns, no row nor line of a task is wider than 100 | display / rows keep to 100 columns on a wide terminal |
| `Display.set_scene`, `Theme.motion` | in every frame of a table or a panel in any theme, at any width and time, only border glyphs differ from the theme without motion, each from one as wide; without motion every frame is the one `Fmt` prints | display / a scene's frames move only its border |
| `Display.set_scene`, `Theme.still` | a scene is never drawn without a terminal; a widget printed on a formatter is the bytes of its theme without motion | display / a scene is still off a live display |

The display's column alignment with wide labels, its narrowing clear and its
marks are unit tests in `test/test_display.ml`; its teardown on return,
exception, cancellation and `exit` is `test/eio/test_console_eio.ml`, on a
pseudo-terminal.
