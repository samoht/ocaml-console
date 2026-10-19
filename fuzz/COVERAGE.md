# Public API property coverage

Inventory: module aliases exported by `lib/console.mli`, plus `lib/text/console_text.mli`. Each interface stops at its first `(**/**)`. Constants count as values. No exported pure value is uncovered.

Formatting properties write to memory buffers. Display operations also have buffered properties, although the request permits excluding operations that write output. `Display.ctx` and `Display.run` are hidden test harnesses, not entries in the inventory.

Private modules from `lib/dune` are not public API: `ansi`, `paint`, `render`, `widget`, `width`, `inked`, and `display_*`. The extra `api.width` property covers `Width.atoms`, `shorten`, `elide`, `wrap`, `fold`, `string_width`, `pad_left`, and `pad_right`; existing properties cover `truncate`. Values after hide markers (including Display.Event, Activity, history, activity) are excluded as requested.

Generators retain arbitrary bytes, machine integers and floats. Dimensions that allocate proportional storage are bounded in properties; invalid signed dimensions and documented constructor refusals are still exercised. Only documented exception types are caught.

| Public value | Property name(s) |
| --- | --- |
| `Anim.all` | api.anim / animation sampling laws |
| `Anim.const` | api.anim / animation sampling laws |
| `Anim.frame` | api.anim / animation sampling laws |
| `Anim.map` | api.anim / animation sampling laws |
| `Anim.v` | api.anim / animation sampling laws |
| `Bar.spread` | api.bar / bar variants conserve width |
| `Bar.stacked` | api.bar / bar variants conserve width |
| `Bar.v` | api.bar / bar variants conserve width |
| `Border.ascii` | api.border / border accessors and restyling |
| `Border.chars` | api.border / border accessors and restyling |
| `Border.double` | api.border / border accessors and restyling |
| `Border.heavy` | api.border / border accessors and restyling |
| `Border.hidden` | api.border / border accessors and restyling |
| `Border.none` | api.border / border accessors and restyling |
| `Border.pp` | api.border / border accessors and restyling |
| `Border.rounded` | api.border / border accessors and restyling |
| `Border.single` | api.border / border accessors and restyling |
| `Border.style` | api.border / border accessors and restyling |
| `Border.v` | api.border / border accessors and restyling |
| `Border.with_style` | api.border / border accessors and restyling |
| `Color.ansi` | api.color / colour constructors and laws |
| `Color.black` | api.color / colour constructors and laws |
| `Color.blend` | api.color / colour constructors and laws |
| `Color.blue` | api.color / colour constructors and laws |
| `Color.bright_black` | api.color / colour constructors and laws |
| `Color.bright_blue` | api.color / colour constructors and laws |
| `Color.bright_cyan` | api.color / colour constructors and laws |
| `Color.bright_green` | api.color / colour constructors and laws |
| `Color.bright_magenta` | api.color / colour constructors and laws |
| `Color.bright_red` | api.color / colour constructors and laws |
| `Color.bright_white` | api.color / colour constructors and laws |
| `Color.bright_yellow` | api.color / colour constructors and laws |
| `Color.cyan` | api.color / colour constructors and laws |
| `Color.depth_of_env` | api.color / colour constructors and laws |
| `Color.downsample` | api.color / colour constructors and laws |
| `Color.equal` | api.color / colour constructors and laws |
| `Color.green` | api.color / colour constructors and laws |
| `Color.hex` | api.color / colour constructors and laws |
| `Color.magenta` | api.color / colour constructors and laws |
| `Color.of_hex` | api.color / colour constructors and laws |
| `Color.palette` | api.color / colour constructors and laws |
| `Color.pp` | api.color / colour constructors and laws |
| `Color.red` | api.color / colour constructors and laws |
| `Color.rgb` | api.color / colour constructors and laws |
| `Color.scale` | api.color / colour constructors and laws |
| `Color.to_rgb` | api.color / colour constructors and laws |
| `Color.white` | api.color / colour constructors and laws |
| `Color.yellow` | api.color / colour constructors and laws |
| `Console_text.sanitize` | api.console_text / text sanitation laws |
| `Console_text.sanitize_styles` | api.console_text / text sanitation laws |
| `Console_text.strip` | api.console_text / text sanitation laws |
| `Display.Line.accent` | api.display_line / row layout validation |
| `Display.Line.bar` | api.display_line / row layout validation |
| `Display.Line.count` | api.display_line / row layout validation |
| `Display.Line.default` | api.display_line / row layout validation |
| `Display.Line.elapsed` | api.display_line / row layout validation |
| `Display.Line.label` | api.display_line / row layout validation |
| `Display.Line.percent` | api.display_line / row layout validation |
| `Display.Line.rate` | api.display_line / row layout validation |
| `Display.Line.spacer` | api.display_line / row layout validation |
| `Display.Line.spinner` | api.display_line / row layout validation |
| `Display.Line.text` | api.display_line / row layout validation |
| `Display.Line.v` | api.display_line / row layout validation |
| `Display.cancel` | api.display / display public operations on memory formatter |
| `Display.fail` | api.display / display public operations on memory formatter |
| `Display.log` | api.display / display public operations on memory formatter |
| `Display.pp` | api.display / display public operations on memory formatter |
| `Display.set_bytes` | api.display / display public operations on memory formatter |
| `Display.set_count` | api.display / display public operations on memory formatter |
| `Display.set_label` | api.display / display public operations on memory formatter |
| `Display.set_result` | api.display / display public operations on memory formatter |
| `Display.succeed` | api.display / display public operations on memory formatter |
| `Display.task` | api.display / display public operations on memory formatter |
| `Display.with_output` | api.display / display public operations on memory formatter |
| `Gradient.at` | api.gradient / gradient coordinate and mapping laws |
| `Gradient.cell` | api.gradient / gradient coordinate and mapping laws |
| `Gradient.equal` | api.gradient / gradient coordinate and mapping laws |
| `Gradient.map` | api.gradient / gradient coordinate and mapping laws |
| `Gradient.pp` | api.gradient / gradient coordinate and mapping laws |
| `Gradient.shift` | api.gradient / gradient coordinate and mapping laws |
| `Gradient.stops` | api.gradient / gradient coordinate and mapping laws |
| `Gradient.v` | api.gradient / gradient coordinate and mapping laws |
| `Guide.ascii` | api.guide / guide accessors and restyling |
| `Guide.branch` | api.guide / guide accessors and restyling |
| `Guide.last` | api.guide / guide accessors and restyling |
| `Guide.pipe` | api.guide / guide accessors and restyling |
| `Guide.pp` | api.guide / guide accessors and restyling |
| `Guide.space` | api.guide / guide accessors and restyling |
| `Guide.style` | api.guide / guide accessors and restyling |
| `Guide.unicode` | api.guide / guide accessors and restyling |
| `Guide.v` | api.guide / guide accessors and restyling |
| `Guide.with_style` | api.guide / guide accessors and restyling |
| `Input.crlf` | api.input / input chunking and masking laws |
| `Input.feed` | api.input / input chunking and masking laws |
| `Input.history` | api.input / input chunking and masking laws |
| `Input.pending` | api.input / input chunking and masking laws |
| `Input.v` | api.input / input chunking and masking laws |
| `Layout.hcat` | api.layout / layout arbitrary blocks and gutters |
| `Layout.vcat` | api.layout / layout arbitrary blocks and gutters |
| `Panel.content_width` | api.panel / panel content room and rendering |
| `Panel.pp` | api.panel / panel content room and rendering |
| `Panel.to_string` | api.panel / panel content room and rendering |
| `Panel.v` | api.panel / panel content room and rendering |
| `Rule.pp` | api.rule / rule accepts arbitrary glyphs and labels |
| `Rule.v` | api.rule / rule accepts arbitrary glyphs and labels |
| `Span.( ++ )` | api.span / span conversion and sanitation laws |
| `Span.concat` | api.span / span conversion and sanitation laws |
| `Span.empty` | api.span / span conversion and sanitation laws |
| `Span.link` | api.span / span conversion and sanitation laws |
| `Span.pp` | api.span / span conversion and sanitation laws |
| `Span.sanitize` | api.span / span conversion and sanitation laws |
| `Span.split_lines` | api.span / span conversion and sanitation laws |
| `Span.styled` | api.span / span conversion and sanitation laws |
| `Span.text` | api.span / span conversion and sanitation laws |
| `Span.to_string` | api.span / span conversion and sanitation laws |
| `Span.truncate` | api.span / span conversion and sanitation laws |
| `Span.width` | api.span / span conversion and sanitation laws |
| `Span.with_style` | api.span / span conversion and sanitation laws |
| `Span.wrap` | api.span_bounds / wrapped spans fit width; console / Span.wrap accepts arbitrary bytes; console / wrapping preserves non-space text |
| `Spinner.anim` | api.spinner / predefined spinner sampling |
| `Spinner.ascii` | api.spinner / predefined spinner sampling |
| `Spinner.braille` | api.spinner / predefined spinner sampling |
| `Spinner.frames` | api.spinner / predefined spinner sampling |
| `Spinner.pp` | api.spinner / predefined spinner sampling |
| `Spinner.v` | api.spinner / predefined spinner sampling |
| `Style.( + )` | api.style / style composition and cell laws |
| `Style.at` | api.style / style composition and cell laws |
| `Style.background` | api.style / style composition and cell laws |
| `Style.bg` | api.style / style composition and cell laws |
| `Style.bg_gradient` | api.style / style composition and cell laws |
| `Style.blink` | api.style / style composition and cell laws |
| `Style.bold` | api.style / style composition and cell laws |
| `Style.equal` | api.style / style composition and cell laws |
| `Style.faint` | api.style / style composition and cell laws |
| `Style.fg` | api.style / style composition and cell laws |
| `Style.fg_gradient` | api.style / style composition and cell laws |
| `Style.foreground` | api.style / style composition and cell laws |
| `Style.has_gradient` | api.style / style composition and cell laws |
| `Style.is_none` | api.style / style composition and cell laws |
| `Style.italic` | api.style / style composition and cell laws |
| `Style.map_color` | api.style / style composition and cell laws |
| `Style.none` | api.style / style composition and cell laws |
| `Style.pp` | api.style / style composition and cell laws |
| `Style.reverse` | api.style / style composition and cell laws |
| `Style.shows_on_space` | api.style / style composition and cell laws |
| `Style.strikethrough` | api.style / style composition and cell laws |
| `Style.underline` | api.style / style composition and cell laws |
| `Table.column` | api.table / table column and row contracts |
| `Table.of_rows` | api.table / table column and row contracts |
| `Table.pp` | api.table / table column and row contracts |
| `Table.to_string` | api.table / table column and row contracts |
| `Theme.accent` | api.theme / theme overrides and defaults |
| `Theme.border` | api.theme / theme overrides and defaults |
| `Theme.dos` | api.theme / theme overrides and defaults |
| `Theme.guide` | api.theme / theme overrides and defaults |
| `Theme.matrix` | api.theme / theme overrides and defaults |
| `Theme.palette` | api.theme / theme overrides and defaults |
| `Theme.pp` | api.theme / theme overrides and defaults |
| `Theme.rainbow` | api.theme / theme overrides and defaults |
| `Theme.spinner` | api.theme / theme overrides and defaults |
| `Theme.unicode` | api.theme / theme overrides and defaults |
| `Theme.v` | api.theme / theme overrides and defaults |
| `Tree.pp` | api.tree / tree printers accept arbitrary labels |
| `Tree.to_string` | api.tree / tree printers accept arbitrary labels |
| `Tree.v` | api.tree / tree printers accept arbitrary labels |

165 exported values listed.

# Eio coverage

| Public function | Mock test(s) |
| --- | --- |
| `Console_eio.run` | run with mock clock; run failure releases display; nested child fiber shares display |
| `Console_eio.setup` | setup under mock backend |
| `Prompt.v` | bytewise CRLF and buffer reuse; nonterminal prompts never read |
| `Prompt.interactive` | bytewise CRLF and buffer reuse; nonterminal prompts never read |
| `Prompt.line` | bytewise CRLF and buffer reuse; split UTF-8 and buffered second line; partial line at EOF is refused; read fault propagates after flush; answer limit |
| `Prompt.confirm` | confirm and expect scripted answers; nonterminal prompts never read |
| `Prompt.expect` | confirm and expect scripted answers; nonterminal prompts never read |
| `Prompt.secret` | nonterminal prompts never read; existing secret leaves the domain running (real PTY) |

All new Eio tests run under `Eio_mock.Backend.run`; input is scripted with `Eio_mock.Flow.on_read`. Timed runs use `Eio_mock.Clock`. A real PTY, in the existing test, is the smallest stand-in for password success: mock flows have no Unix descriptor or termios state. `setup` necessarily probes the real standard descriptors with isatty; its explicit-renderer branches work under the mock backend. The public API needs no process manager.

# Validation

`dune build @test/runtest @test/eio/runtest @fuzz/runtest` using the preinstalled switch; no installation or dependency changes. Failures from the first pass are retained. No library or binary implementation changes.

The final run compiles successfully and executes 63 fuzz properties (10 fail),
245 core tests (11 fail), 47 Eio tests (2 fail), and 3 text tests (1 fails).
The previous pass accounts for all failures except these four new tests:

| New failing test | Observation |
| --- | --- |
| `api.bar / bar variants conserve width` | `Bar.spread` emits one cell for width zero with a positive value. |
| `api.color / colour constructors and laws` | `Color.scale` with a very large positive finite factor fails to saturate RGB components at 255. |
| `api.rule / rule accepts arbitrary glyphs and labels` | A glyph measuring one cell but containing a control byte raises an undocumented `Invalid_argument`. |
| `mock / partial line at EOF is refused` | An unterminated line is returned as `Ok` when input ends. |

All four new failures have separate `test:` commits. The property failures in
bar and rule also expose inconsistencies between documented validation and
behavior; these tests intentionally follow the published interface contracts.
