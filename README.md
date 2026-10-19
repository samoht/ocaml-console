# console

Styled text, tables, trees and progress bars for OCaml command-line tools.

console prints into the normal scrollback, the way `make` or `ls` do. It
never switches to the alternate screen and runs no event loop of its own, so
for a full-screen program you want [notty](https://github.com/pqwy/notty) or
[lambda-term](https://github.com/ocaml-community/lambda-term) instead. On a
terminal, its progress rows are redrawn in place. When the output goes to a
pipe or a file, the same program writes plain lines with no escape codes, so
CI logs stay readable.

It has:

- styles that compose with `+`, over the 16 ANSI colours, the 256-colour
  palette and RGB;
- spans of styled text and OSC 8 hyperlinks, which keep their style when
  wrapped or truncated;
- tables, trees, panels and rules, with five border styles and shared themes;
- live progress rows, spinners and bars;
- widths counted in terminal cells, so CJK characters and emoji line up.

## Installation

<!-- $MDX skip -->
```sh
$ opam install console
```

Until console is in opam-repository, pin it from GitHub:

<!-- $MDX skip -->
```sh
$ opam pin add https://github.com/samoht/ocaml-console.git
```

## Usage

Styles compose with `+` and apply through a `Fmt` combinator:

```ocaml
open Console

let success = Style.(bold + fg Color.green)
let error = Style.(bold + fg Color.red)

let () =
  Fmt.pr "%a@." (Style.styled success Fmt.string) "✓ Build passed";
  Fmt.pr "%a@." (Style.styled error Fmt.string) "✗ Tests failed"
```

Widgets take their text as a `Span.t`. `Span.pp` writes styles and links
only when the formatter allows ANSI (see `Fmt.set_style_renderer`), and
`Span.to_string` always returns plain text.

```ocaml
open Console

let docs =
  Span.(
    styled Style.bold "Read "
    ++ link ~style:Style.underline ~uri:"https://ocaml.org/docs" "the manual")

let () = Fmt.pr "%a@." Span.pp docs
```

Tables, trees and panels are `Fmt` printers. They take the width their
content needs, or `~width` when you know the limit:

```ocaml
open Console

let print_files () =
  let table = Table.(
    of_rows ~border:Border.rounded
      [ column "Name"; column ~align:`Right "Size"; column "Modified" ]
      [
        [ Span.text "README.md"; Span.text "2.4K"; Span.text "2 hours ago" ];
        [ Span.text "src/"; Span.text "4.0K"; Span.text "yesterday" ];
        [ Span.text "Makefile"; Span.text "892"; Span.text "3 days ago" ];
      ]
  ) in
  Fmt.pr "%a@." Table.pp table

let main env =
  Console_eio.run ~clock:(Eio.Stdenv.clock env) @@ fun _display ->
  print_files ()
```

```
╭───────────┬──────┬─────────────╮
│ Name      │ Size │ Modified    │
├───────────┼──────┼─────────────┤
│ README.md │ 2.4K │ 2 hours ago │
│ src/      │ 4.0K │ yesterday   │
│ Makefile  │  892 │ 3 days ago  │
╰───────────┴──────┴─────────────╯
```

Every widget takes an optional `~theme`. A `Theme.t` sets the accent colour,
the border, the tree guide, the spinner and the bar style in one place, and
an explicit `~border` or `~guide` still overrides it. The presets are
`Theme.unicode` (the default), `Theme.dos`, `Theme.matrix` and
`Theme.rainbow`.

`Rule` draws a horizontal line with an optional label, and `Layout` puts
rendered strings side by side or one under the other:

```ocaml
open Console

let section =
  Rule.v ~theme:Theme.unicode ~label:(Span.text "Results") ~width:40 ()

let report =
  Layout.vcat ~gutter:1
    [ Rule.to_string section; Panel.to_string (Panel.v (Span.text "All green")) ]
```

### Progress

A `Display` shows one row per running task. On a terminal, those rows are
redrawn in place, and log lines and finished tasks go once into the
scrollback. In a pipe or a log file, the same calls print one plain line per
event.

```ocaml
open Console

let build ~clock =
  Console_eio.run ~clock ~theme:Theme.unicode ~header:"Building" @@ fun display ->
  let step = Display.task display "compile" in
  Display.set_count step ~cur:2 ~total:2;
  Display.log step "compiled parser.ml";
  Display.succeed step
```

A task shows a spinner until it knows its total, then a bar with a count, a
rate and the elapsed time. A task still running when the callback returns is
marked as succeeded. If the callback raises, it is marked as cancelled. A
nested `Console_eio.run` reuses the outer display, and a task created with
`~id` is found again by that id, which helps when its events arrive twice or
out of order. `Display.Line` builds other row layouts.

### Animation

![Matrix ASCII rain demo](examples/matrix/matrix.gif)

An `Anim.t` is a value that changes over time. Spinners, bars and the Matrix
rain of `Panel.rain` and `Table.rain` are all animations, and `Anim.map2`,
`Anim.all`, `Layout.hcat_anim` and `Layout.vcat_anim` combine them. The demo
in [`examples/matrix`](examples/matrix) runs with:

<!-- $MDX skip -->
```sh
$ dune exec examples/matrix/main.exe
```

## API

| Module | Description |
|---|---|
| `Console.Color` | ANSI, RGB, hex, and 256-palette colours |
| `Console.Style` | Composable text styles |
| `Console.Gradient` | Linear colour gradients laid over terminal cells |
| `Console.Span` | Styled text, hyperlinks, wrapping and truncation |
| `Console.Width` | Unicode-, CSI-, and OSC-aware cell measurement |
| `Console.Border` | Custom and predefined box-drawing borders |
| `Console.Guide` | Shared guide glyphs for nested views |
| `Console.Theme` | One visual theme for every widget |
| `Console.Table` | Styled tables with alignment and overflow policies |
| `Console.Tree` | Tree rendering with shared guides |
| `Console.Panel` | Bordered panels with edge labels |
| `Console.Rule` | Fixed-width horizontal rules with styled labels |
| `Console.Layout` | Horizontal and vertical static/animated composition |
| `Console.Canvas` | Grids of styled cells, for pixel art and scenes |
| `Console.Spinner` | Animated spinners for indeterminate work |
| `Console.Bar` | Determinate and moving horizontal progress bars |
| `Console.Anim` | Composable time-varying values |
| `Console.Display` | Live nested progress rows with plain fallback |
| `Console.Input` | Line editing with history and completion |
| `Console_eio` | Eio driver: terminal detection, geometry, the refresh fiber |
| `Console_eio.Prompt` | Questions over Eio flows, and a masked secret from the terminal |
| `Console_text` | Sanitizing and stripping untrusted text, without the rest of `console` |
| `Console_vte` | The screen a terminal recording put up, frame by frame |

## Related work

- [Rich](https://github.com/Textualize/rich), the Python library console
  is modelled on.
- [lipgloss](https://github.com/charmbracelet/lipgloss), whose composable
  styles gave the shape of `Style`.
- [printbox](https://github.com/c-cube/printbox) prints nested boxes, tables
  and trees in OCaml.
- [progress](https://github.com/CraigFe/progress) draws progress bars in
  OCaml.
- [notty](https://github.com/pqwy/notty) does declarative terminal graphics
  for full-screen OCaml programs.
- [down](https://erratique.ch/software/down), Daniel Bünzli's toplevel line
  editor, is the model for the key bindings, history and completion of
  `Console.Input`.

## License

ISC. See [LICENSE.md](LICENSE.md).
