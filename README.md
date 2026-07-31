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
- **Widgets**: tables with headers and alignment, directory-style trees,
  bordered panels, in five border styles
- **Themes**: one `Theme.t` styles every widget consistently (`dos`,
  `unicode`, `matrix`, `rainbow`)
- **Live progress**: a `Display` of rows that update in place, degrading to
  one tidy line per step with no terminal
- **Animation**: spinners, progress bars and a Matrix ASCII rain border, all
  driven by a single `Anim.t` abstraction
- **Unicode-aware**: proper width calculation for CJK and emoji

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

Widgets render through a `ctx`, which carries the terminal capabilities and the
theme. A library takes one as an argument; the top-level binary builds one with
`Console_eio.run` and threads it down:

```ocaml
open Console

let print_files ~ctx =
  let table = Table.(
    of_rows ~border:Border.rounded
      [ column "Name"; column ~align:`Right "Size"; column "Modified" ]
      [
        [ Span.text "README.md"; Span.text "2.4K"; Span.text "2 hours ago" ];
        [ Span.text "src/"; Span.text "4.0K"; Span.text "yesterday" ];
        [ Span.text "Makefile"; Span.text "892"; Span.text "3 days ago" ];
      ]
  ) in
  Fmt.pr "%a@." (Table.pp ~ctx) table

let main env =
  Console_eio.run ~clock:(Eio.Stdenv.clock env) @@ fun ctx -> print_files ~ctx
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

### Progress

A `Display` owns a live region of rows that update in place. On a terminal it
redraws with the cursor; with none -- a pipe, a captured log, or `~mode:`Plain`
-- the very same calls emit one tidy append-only line per finished step, so
logs stay clean.

```ocaml
open Console

let build ~ctx =
  let d = Display.v ~ctx ~mode:`Plain ~theme:Theme.unicode ~header:"Building" () in
  let step = Display.row d "compile" in
  Display.set_count step ~cur:2 ~total:2;
  Display.finish step;
  Display.finish_all d
```

Bars are composed from pieces (`Display.Line`): spinner, label, `bar n`,
percent, count, rate, elapsed, spacer. Under Eio, `Console_eio.Display.run` owns
a display for the duration of a function, animating it on a tick.

### Animation

![Matrix ASCII rain demo](examples/matrix/matrix.gif)

An `Anim.t` is a value sampled at an elapsed time. `Spinner`, `Bar` and
`Panel.rain` / `Table.rain` expose one; `Display.set_banner` and
`Console_eio.Anim.run` drive it live, redrawing each tick. A panel or table
built with `~theme:Theme.matrix` rains automatically. A runnable demo lives in
[`examples/matrix`](examples/matrix):

<!-- $MDX skip -->
```sh
$ dune exec ocaml-console/examples/matrix/main.exe
```

## API

| Module       | Description                                    |
|--------------|------------------------------------------------|
| `Console.Color`  | ANSI, RGB, hex, and 256-palette colors         |
| `Console.Style`  | Composable text styles                         |
| `Console.Span`   | Styled text spans                              |
| `Console.Border` | Border styles (ascii, single, double, rounded) |
| `Console.Theme`  | Shared visual theme styling every widget       |
| `Console.Table`  | Tables with headers and alignment              |
| `Console.Tree`   | Tree rendering with guides                     |
| `Console.Panel`  | Bordered panels with titles                    |
| `Console.Width`  | Unicode-aware string width calculation         |
| `Console.Spinner` | Animated spinners for indeterminate work      |
| `Console.Bar`    | Horizontal progress bars, determinate or moving |
| `Console.Anim`   | Time-varying values shared by animatable components |
| `Console.Display` | Live progress display of rows that update in place |
| `Console.Input`  | Cooked input: line editor with history and tab completion |

## Related Work

- [Rich](https://github.com/Textualize/rich) - Python library for rich text and beautiful formatting. Primary inspiration.
- [lipgloss](https://github.com/charmbracelet/lipgloss) - Go library for terminal styling. Inspiration for composable style API.
- [printbox](https://github.com/c-cube/printbox) - OCaml library for printing nested boxes, tables and trees.
- [progress](https://github.com/CraigFe/progress) - Terminal progress bars for OCaml.
- [notty](https://github.com/pqwy/notty) - OCaml library for declarative terminal graphics.
- [down](https://erratique.ch/software/down) - OCaml toplevel line editor by Daniel Buenzli; model for `Console.Input`'s key bindings, history and completion.

## Licence

ISC licence. See [LICENSE.md](LICENSE.md) for details.
