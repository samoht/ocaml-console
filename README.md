# console

Terminal styling and layout widgets for OCaml CLI applications.

```
╭──────────────────────────────────────────────────────────────────╮
│                                                                  │
│    ██████╗ ██████╗ ███╗   ██╗███████╗ ██████╗ ██╗     ███████╗   │
│   ██╔════╝██╔═══██╗████╗  ██║██╔════╝██╔═══██╗██║     ██╔════╝   │
│   ██║     ██║   ██║██╔██╗ ██║███████╗██║   ██║██║     █████╗     │
│   ██║     ██║   ██║██║╚██╗██║╚════██║██║   ██║██║     ██╔══╝     │
│   ╚██████╗╚██████╔╝██║ ╚████║███████║╚██████╔╝███████╗███████╗   │
│    ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝ ╚═════╝ ╚══════╝╚══════╝   │
│                                                                  │
│   Type-safe terminal styling and layout widgets for OCaml        │
│                                                                  │
╰──────────────────────────────────────────────────────────────────╯
```

A command-line tool that prints a table, a tree or a progress bar has to
measure every string in terminal cells, keep ANSI styling out of redirected
output, and redraw live rows without scrambling the scrollback. A terminal reads
commands inside the text it prints, as escape sequences: a CSI sequence (ESC `[`
...) moves the cursor or sets colours, and an OSC sequence (ESC `]` ...) passes
a command to the terminal, such as OSC 8, which makes the text it wraps a
clickable link to a URI. `console` does that work once. It renders *inline*
output (styled text, tables, trees, panels and live progress rows) into the
normal scrollback, and falls back to plain append-only lines when standard
output is not a terminal. It is not a TUI framework: it never switches to the
alternate screen (the second screen buffer that full-screen programs such as
`vim` draw on, leaving the scrollback untouched), never takes over the whole
window and never runs an event loop that owns the terminal. For full-screen
applications, use [notty](https://github.com/pqwy/notty) or
[lambda-term](https://github.com/ocaml-community/lambda-term).

- **Composable styles**: `Style.(bold + fg Color.red + underline)`, over ANSI
  16, the 256-colour palette and true colour (RGB/hex)
- **Rich spans**: styled text, safe OSC-8 hyperlinks, style-preserving wrapping
  and truncation, with clean plain-text fallbacks
- **Widgets**: tables with headers and alignment, directory-style trees,
  bordered panels, horizontal rules, and five border styles
- **Themes**: one `Theme.t` styles every widget consistently (`dos`,
  `unicode`, `matrix`, `rainbow`)
- **Live progress**: a `Display` of rows that update in place, degrading to
  one tidy line per step with no terminal
- **Animation and layout**: spinners, progress bars and Matrix rain driven by a
  single `Anim.t`, composed horizontally or vertically with `Layout`
- **Unicode-aware**: proper width calculation for CJK and emoji

## Contents

- [Installation](#installation)
- [Usage](#usage)
- [Progress](#progress)
- [Animation and layout](#animation-and-layout)
- [API](#api)

## Installation

Install with opam:

<!-- $MDX skip -->
```sh
$ opam install console
```

If opam cannot find the package, it may not yet be released in the public
`opam-repository`. Add the overlay repository, then install it:

<!-- $MDX skip -->
```sh
$ opam repo add samoht https://tangled.org/gazagnaire.org/opam-overlay.git
$ opam update
$ opam install console
```

## Usage

Styles compose with `+` and apply through a `Fmt.t` combinator:

```ocaml
open Console

let success = Style.(bold + fg Color.green)
let error = Style.(bold + fg Color.red)

let () =
  Fmt.pr "%a@." (Style.styled success Fmt.string) "✓ Build passed";
  Fmt.pr "%a@." (Style.styled error Fmt.string) "✗ Tests failed"
```

`Span` is the composable text value used by every widget. Styling and terminal
hyperlinks follow the destination formatter: `Span.pp` emits them only when
the formatter's `Fmt.style_renderer` allows ANSI, while `Span.to_string` is
always plain text. This keeps redirected output and logs clean.

```ocaml
open Console

let docs =
  Span.(
    styled Style.bold "Read "
    ++ link ~style:Style.underline ~uri:"https://ocaml.org/docs" "the manual")

let () = Fmt.pr "%a@." Span.pp docs
```

Static widgets are ordinary formatter printers. They render at their natural
width by default; pass `~width` only when a known boundary matters, such as a
remote PTY:

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

`Tree.pp` and `Panel.pp` render the other two structural widgets the same way.
A `Theme.t` bundles the visual choices they share (accent colour and palette,
border, tree guide, spinner, bar style and markers), so passing the same
`~theme` to every widget gives the output one look; an explicit `~border` or
`~guide` still takes precedence. The presets are `Theme.unicode` (the default),
`Theme.dos` (an amber ASCII terminal), `Theme.matrix` and `Theme.rainbow`, and
`Theme.v` builds your own.

`Rule` and `Layout` fill the small composition gaps without introducing a
second box model:

```ocaml
open Console

let section =
  Rule.v ~theme:Theme.unicode ~label:(Span.text "Results") ~width:40 ()

let report =
  Layout.vcat ~gutter:1
    [ Rule.to_string section; Panel.to_string (Panel.v (Span.text "All green")) ]
```

### Progress

A `Display` has two projections: append-only event history and an immutable
snapshot of current activity. On a terminal, only the bounded activity snapshot
is redrawn; logs and finished tasks stay in scrollback. A pipe or captured log
gets the same permanent history without cursor movement.

```ocaml
open Console

let build ~clock =
  Console_eio.run ~clock ~theme:Theme.unicode ~header:"Building" @@ fun display ->
  let step = Display.task display "compile" in
  Display.set_count step ~cur:2 ~total:2;
  Display.log step "compiled parser.ml";
  Display.succeed step
```

Most programs need only this: one `Console_eio.run`, a `Display.task` per step,
progress updates and an outcome. The default row shows a spinner until a total
is known, then a bar, a count, a rate and the elapsed time. Returning normally completes forgotten tasks; raising cancels them;
nested `Console_eio.run` calls join the existing display instead of claiming the
terminal again. Stable `~id` values deduplicate repeated or out-of-order task
events without a caller-side table.

A program that needs a different row composes one with `Display.Line` from
spinner, label, bar, percent, count, rate, elapsed and spacer pieces.

### Animation and layout

![Matrix ASCII rain demo](examples/matrix/matrix.gif)

An `Anim.t` is a value sampled at an elapsed time. `Spinner`, `Bar` and
`Panel.rain` / `Table.rain` expose one. A panel or table
made with `~theme:Theme.matrix` rains automatically. `Anim.map2`, `Anim.all`,
`Layout.hcat_anim` and `Layout.vcat_anim` combine independently animated
components while sampling them at the same elapsed time. A runnable demo lives in
[`examples/matrix`](examples/matrix):

<!-- $MDX skip -->
```sh
$ dune exec examples/matrix/main.exe
```

## API

| Module | Description |
|---|---|
| `Console.Color` | ANSI, RGB, hex, and 256-palette colours |
| `Console.Style` | Composable text styles |
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
| `Console.Spinner` | Animated spinners for indeterminate work |
| `Console.Bar` | Determinate and moving horizontal progress bars |
| `Console.Anim` | Composable time-varying values |
| `Console.Display` | Live nested progress rows with plain fallback |
| `Console.Input` | Line editing with history and completion |
| `Console_eio` | Eio driver: terminal detection, geometry, the refresh fiber |
| `Console_vte` | The screen a terminal recording put up, frame by frame |

## Related Work

- [Rich](https://github.com/Textualize/rich) - Python library for rich text and beautiful formatting. Primary inspiration.
- [lipgloss](https://github.com/charmbracelet/lipgloss) - Go library for terminal styling. Inspiration for composable style API.
- [printbox](https://github.com/c-cube/printbox) - OCaml library for printing nested boxes, tables and trees.
- [progress](https://github.com/CraigFe/progress) - Terminal progress bars for OCaml.
- [notty](https://github.com/pqwy/notty) - OCaml library for declarative terminal graphics.
- [down](https://erratique.ch/software/down) - OCaml toplevel line editor by Daniel Buenzli; model for `Console.Input`'s key bindings, history and completion.

## License

ISC. See [LICENSE.md](LICENSE.md).
