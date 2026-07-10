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

## What it looks like

### Tables

```
╭──────────┬────────┬─────────┬──────────┬─────────┬───────────╮
│ Target   │ Inst.  │ Execs   │ Paths    │ Crashes │ Exec/s    │
├──────────┼────────┼─────────┼──────────┼─────────┼───────────┤
│ cbor     │      3 │    1.2M │      847 │       0 │     4,521 │
│ yamlrw   │      3 │    890K │    1,203 │       2 │     3,102 │
│ jwt      │      2 │    2.1M │      412 │       0 │     6,847 │
╰──────────┴────────┴─────────┴──────────┴─────────┴───────────╯
```

### Trees

```
src
├── lib
│   ├── console.ml
│   ├── style.ml
│   └── table.ml
├── bin
│   └── main.ml
└── test
    └── test_console.ml
```

### Panels

```
╭─────────────────────── Status ───────────────────────╮
│                                                      │
│  ✓ All systems operational                           │
│  ✓ Database connected                                │
│  ✓ Cache warmed                                      │
│                                                      │
╰──────────────────────────────────────────────────────╯
```

### Border Styles

```
ASCII:            Single:           Rounded:          Heavy:
+-------+         ┌───────┐         ╭───────╮         ┏━━━━━━━┓
| hello |         │ hello │         │ hello │         ┃ hello ┃
+-------+         └───────┘         ╰───────╯         ┗━━━━━━━┛

Double:
╔═══════╗
║ hello ║
╚═══════╝
```

## Features

- **Composable styles**: `Style.(bold + fg Color.red + underline)`
- **Full color support**: ANSI 16, 256-color palette, true color (RGB/hex)
- **Tables**: Headers, alignment, multiple border styles
- **Trees**: Directory-style tree rendering with guides
- **Panels**: Bordered boxes with titles
- **Themes**: one `Theme.t` styles every widget consistently (`dos`,
  `unicode`, `matrix`, `rainbow`)
- **Live progress**: a `Display` of rows that update in place, degrading to
  one tidy line per step with no terminal
- **Animation**: spinners, progress bars and a Matrix ASCII rain border, all
  driven by a single `Anim.t` abstraction
- **Unicode-aware**: Proper width calculation for CJK and emoji

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

## Quick Start

### Styled Text

```ocaml
open Console

(* Compose styles with + *)
let success = Style.(bold + fg Color.green)
let error = Style.(bold + fg Color.red)
let warning = Style.(fg Color.yellow)

let () =
  Fmt.pr "%a@." (Style.styled success Fmt.string) "✓ Build passed";
  Fmt.pr "%a@." (Style.styled error Fmt.string) "✗ Tests failed";
  Fmt.pr "%a@." (Style.styled warning Fmt.string) "⚠ Deprecation warning"
```

### Tables

```ocaml
open Console

(* A library renders through a [ctx]; the top-level binary builds one with
   [Console_eio.run] and threads it down. *)
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

Output:
```
╭───────────┬──────┬─────────────╮
│ Name      │ Size │ Modified    │
├───────────┼──────┼─────────────┤
│ README.md │ 2.4K │ 2 hours ago │
│ src/      │ 4.0K │ yesterday   │
│ Makefile  │  892 │ 3 days ago  │
╰───────────┴──────┴─────────────╯
```

### Trees

```ocaml
open Console

let () =
  let tree = Tree.of_tree (
    Node (Span.text "myproject", [
      Node (Span.text "src", [
        Node (Span.text "main.ml", []);
        Node (Span.text "lib.ml", []);
      ]);
      Node (Span.text "test", [
        Node (Span.text "test_main.ml", []);
      ]);
      Node (Span.text "dune-project", []);
    ])
  ) in
  Tree.pp Format.std_formatter tree;
  Format.pp_print_flush Format.std_formatter ()
```

Output:
```
myproject
├── src
│   ├── main.ml
│   └── lib.ml
├── test
│   └── test_main.ml
└── dune-project
```

### Panels

```ocaml
open Console

let () =
  let panel = Panel.v
    ~border:Border.rounded
    ~title:(Span.text "Summary")
    (Span.text "Processed 1,234 files\nFound 42 issues\nFixed 38 automatically")
  in
  Panel.pp Format.std_formatter panel;
  Format.pp_print_flush Format.std_formatter ()
```

Output:
```
╭─────────────────────── Summary ────────────────────────╮
│ Processed 1,234 files                                  │
│ Found 42 issues                                        │
│ Fixed 38 automatically                                 │
╰────────────────────────────────────────────────────────╯
```

## Themes

A `Theme.t` bundles every visual choice the widgets share -- the accent colour
and palette, the border, the tree guide, and the live display's spinner, bar
style and markers -- so one value styles tables, trees, panels and the progress
display consistently. Pass `~theme` to any widget; an explicit `~border` or
`~guide` still wins.

```ocaml
open Console

let print_tests ~ctx =
  let table =
    Table.of_string_rows ~theme:Theme.unicode
      [ Table.column "Module"; Table.column ~align:`Right "Tests" ]
      [ [ "console"; "113" ]; [ "console-eio"; "10" ] ]
  in
  Fmt.pr "%a@." (Table.pp ~ctx) table
```

The presets are `Theme.dos` (amber ASCII CRT, the default), `Theme.unicode`,
`Theme.matrix` (green) and `Theme.rainbow`; build your own with `Theme.v`.

## Progress

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
percent, count, rate, elapsed, spacer. Under Eio, `Console_eio.Display.run` owns a
display for the duration of a function, animating it on a tick.

## Animation & the Matrix

![Matrix ASCII rain demo](examples/matrix/matrix.gif)

An `Anim.t` is a value sampled at an elapsed time. `Spinner`, `Bar` and
`Panel.rain` / `Table.rain` expose one; `Display.set_banner` and
`Console_eio.Anim.run` drive it live, redrawing each tick.

```ocaml
open Console

let () =
  (* A progress bar filled to 60%, and a spinner frame, sampled directly. *)
  print_string (Bar.render ~style:`Smooth ~width:20 ~pct:60 ());
  print_newline ();
  print_string (Spinner.at Spinner.braille ~elapsed:0.3);
  print_newline ()
```

`Panel.rain` frames a panel as Matrix-style ASCII digital rain: a bold green
head sweeps the border, trailing dimmer green, every cell flickering through an
ASCII glyph. A panel or table built with `~theme:Theme.matrix` rains
automatically from `Panel.anim` / `Table.anim` (the theme sets
`animated_border`); `Panel.rain` applies the effect to any panel. A runnable
demo lives in [`examples/matrix`](examples/matrix):

<!-- $MDX skip -->
```sh
$ dune exec ocaml-console/examples/matrix/main.exe
```

## Colors

```ocaml
open Console

(* ANSI 16 colors *)
let red = Color.red
let bright_blue = Color.bright_blue

(* RGB colors *)
let coral = Color.rgb 255 127 80
let teal = Color.rgb 0 128 128

(* Hex colors *)
let purple = Color.hex "#8B5CF6"
let orange = Color.hex "F97316"

(* 256-color palette *)
let pink = Color.palette 213

(* Use in styles *)
let gradient_style = Style.(fg coral + bg (Color.rgb 30 30 30))
```

## Styles

Available style attributes:

| Attribute       | Example                          |
|-----------------|----------------------------------|
| `bold`          | **bold text**                    |
| `faint`         | dimmed text                      |
| `italic`        | *italic text*                    |
| `underline`     | underlined text                  |
| `blink`         | blinking text                    |
| `reverse`       | inverted colors                  |
| `strikethrough` | ~~strikethrough~~                |
| `fg color`      | coloured foreground               |
| `bg color`      | coloured background               |

```ocaml
(* Combine any attributes *)
let fancy = Style.(bold + italic + underline + fg Color.cyan + bg Color.black)
```

## API Reference

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
