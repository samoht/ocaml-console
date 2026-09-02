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

console renders *inline* output: styled text, tables, trees, panels and
live progress rows written into the normal scroll-back, degrading to
plain append-only lines when stdout is not a terminal. It is not a TUI
framework -- it never switches to the alternate screen, takes over the
full window, or runs an event loop owning the terminal. For full-screen
terminal applications, see [notty](https://github.com/pqwy/notty) or
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
A `Theme.t` bundles every visual choice they share -- accent colour and palette,
border, tree guide, spinner, bar style and markers -- so passing `~theme` to any
widget styles the whole output consistently; an explicit `~border` or `~guide`
still wins. The presets are `Theme.dos` (amber ASCII CRT, the default),
`Theme.unicode`, `Theme.matrix` and `Theme.rainbow`, and `Theme.v` builds your
own.

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

That is the ordinary API: one `Console_eio.run`, `Display.task`, semantic
updates and an outcome. The default line automatically chooses an indeterminate
spinner or determinate progress, count, rate and elapsed time from the updates
it receives. Returning normally completes forgotten tasks; raising cancels them;
nested `Console_eio.run` calls join the existing display instead of claiming the
terminal again. Stable `~id` values deduplicate repeated or out-of-order task
events without a caller-side table.

For applications with genuinely unusual information density, the advanced
`Display.Line` API can compose spinner, label, bar, percent, count, rate,
elapsed and spacer pieces. Most callers should never need it.

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
$ dune exec ocaml-console/examples/matrix/main.exe
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

## Licence

ISC licence. See [LICENSE.md](LICENSE.md) for details.
