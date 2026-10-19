# console

Colours, tables, trees and progress bars for OCaml command-line tools.

console prints the way `make` or `ls` do. Each line goes below the previous
one, and everything is still there when the program exits. It is not meant
for full-screen programs such as `vim` or `htop`; for those, use
[notty](https://github.com/pqwy/notty) or
[lambda-term](https://github.com/ocaml-community/lambda-term).

Progress bars update in place while the work runs. When the output goes to a
file or to another program, as in a CI log, console prints the same
information as plain lines. No colour codes or progress-bar updates end up
mixed into the text.

It has:

- text styles (bold, underline, colours) that you combine with `+`, from the
  standard terminal colours up to any RGB colour;
- clickable links, in the terminals that support them;
- tables, trees, framed panels and horizontal lines, in several frame styles;
- progress bars and spinners that update in place;
- correct alignment for wide characters, such as Chinese, Japanese, Korean
  and emoji, which take two columns on screen.

## Installation

<!-- $MDX skip -->
```sh
$ opam install console
```

Until console is in the opam repository, install it from GitHub:

<!-- $MDX skip -->
```sh
$ opam pin add https://github.com/samoht/ocaml-console.git
```

## Usage

Styles combine with `+` and are applied by a
[`Fmt`](https://erratique.ch/software/fmt) printer:

```ocaml
open Console

let success = Style.(bold + fg Color.green)
let error = Style.(bold + fg Color.red)

let () =
  Fmt.pr "%a@." (Style.styled success Fmt.string) "✓ Build passed";
  Fmt.pr "%a@." (Style.styled error Fmt.string) "✗ Tests failed"
```

Tables and other widgets take their text as a `Span.t`, a piece of text with
its style and, optionally, a link. `Span.pp` prints the colours and links
only when the output is a terminal that can show them, and
`Span.to_string` always returns the plain text.

```ocaml
open Console

let docs =
  Span.(
    styled Style.bold "Read "
    ++ link ~style:Style.underline ~uri:"https://ocaml.org/docs" "the manual")

let () = Fmt.pr "%a@." Span.pp docs
```

A table is as wide as its content needs, unless you give it a `~width`:

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

```ocaml
# print_files ();;
╭───────────┬──────┬─────────────╮
│ Name      │ Size │ Modified    │
├───────────┼──────┼─────────────┤
│ README.md │ 2.4K │ 2 hours ago │
│ src/      │ 4.0K │ yesterday   │
│ Makefile  │  892 │ 3 days ago  │
╰───────────┴──────┴─────────────╯

- : unit = ()
```

Trees and framed panels print the same way:

```ocaml
# open Console;;
# let file name = Tree.Node (Span.text name, []) in
  Fmt.pr "%a@." Tree.pp
    (Tree.of_tree
       (Node
          ( Span.text "src",
            [
              Node (Span.text "lib", [ file "console.ml"; file "table.ml" ]);
              Node (Span.text "test", [ file "test.ml" ]);
            ] )));;
src
├── lib
│   ├── console.ml
│   └── table.ml
└── test
    └── test.ml
- : unit = ()
# Fmt.pr "%a@." Panel.pp
    (Panel.v ~title:(Span.text "Status") (Span.text "All tests passed"));;
╭───── Status ─────╮
│ All tests passed │
╰──────────────────╯
- : unit = ()
```

To give every widget the same look, pass them the same `~theme`. A theme sets the
main colour, the frame style, the lines that connect tree branches, the
spinner and the progress bar. The built-in themes are `Theme.unicode` (the
default), `Theme.dos`, `Theme.matrix` and `Theme.rainbow`.

`Rule` draws a horizontal line with an optional title, and `Layout` places
blocks of text next to each other or one above the other:

```ocaml
open Console

let section =
  Rule.v ~theme:Theme.unicode ~label:(Span.text "Results") ~width:40 ()

let report =
  Layout.vcat ~gutter:1
    [ Rule.to_string section; Panel.to_string (Panel.v (Span.text "All green")) ]
```

```ocaml
# print_endline report;;
─────────────── Results ────────────────

╭───────────╮
│ All green │
╰───────────╯
- : unit = ()
```

### Progress

A `Display` shows one line per task that is still running, and updates
those lines in place. Log messages and finished tasks are printed once, above
them, and stay on screen. When the output is not a terminal, the same calls
print one plain line per event instead.

```ocaml
open Console

let build ~clock =
  Console_eio.run ~clock ~theme:Theme.unicode ~header:"Building" @@ fun display ->
  let step = Display.task display "compile" in
  Display.set_count step ~cur:2 ~total:2;
  Display.log step "compiled parser.ml";
  Display.succeed step
```

When the output is not a terminal, the same calls print plain lines.
`Display.dumb` stands in for that case here, with a clock that does not move:

```ocaml
# Display.run ~ctx:(Display.dumb ()) ~header:"Building" (fun display ->
    let step = Display.task display "compile" in
    Display.set_count step ~cur:2 ~total:2;
    Display.log step "compiled parser.ml";
    Display.succeed step);;
  # compiled parser.ml
=> compile  2/2  0.0s
[+] Building 0.0s (1/1)
- : unit = ()
```

`Bar.render` draws a bar on its own, here 65% full and twenty columns wide:

```ocaml
# print_endline (Bar.render ~styled:false ~width:20 ~pct:65 ());;
▕█████████████       ▏
- : unit = ()
```

A task shows a spinner until it knows how much work it has, then a progress
bar with a count, a speed and the time spent so far. If your function returns
while a task is still running, the task is marked as done. If your function
raises an exception, it is marked as cancelled. Calling `Console_eio.run`
inside another `Console_eio.run` reuses the same display.

### Animation

![Matrix ASCII rain demo](examples/matrix/matrix.gif)

Spinners, progress bars and the falling characters of `Theme.matrix` are
animations: values of type `Anim.t` that change over time. `Anim.all` and
`Layout.hcat_anim` combine several of them into one. To see them together,
run the demo in [`examples/matrix`](examples/matrix):

<!-- $MDX skip -->
```sh
$ dune exec examples/matrix/main.exe
```

## API

| Module | What it does |
|---|---|
| `Console.Color` | Terminal colours, by name, by palette number or as RGB and hex |
| `Console.Style` | Bold, italic, underline and colours, combined with `+` |
| `Console.Gradient` | Colour gradients across a line of text |
| `Console.Span` | Styled text and links, with wrapping and truncation |
| `Console.Width` | How many columns a string takes on screen |
| `Console.Border` | Frame styles for tables and panels |
| `Console.Guide` | The lines that connect the branches of a tree |
| `Console.Theme` | One look for every widget |
| `Console.Table` | Tables with headers and aligned columns |
| `Console.Tree` | Trees, drawn like the output of `tree` |
| `Console.Panel` | Text in a frame, with an optional title |
| `Console.Rule` | Horizontal lines with an optional title |
| `Console.Layout` | Blocks of text side by side or stacked |
| `Console.Canvas` | Grids of coloured characters, for pictures |
| `Console.Spinner` | Spinners for work of unknown length |
| `Console.Bar` | Progress bars |
| `Console.Anim` | Values that change over time, for animations |
| `Console.Display` | Progress lines that update in place, or plain lines outside a terminal |
| `Console.Input` | Reading a line from the keyboard, with history and completion |
| `Console_eio` | Runs a `Display` with [Eio](https://github.com/ocaml-multicore/eio): finds the terminal and its size, and redraws |
| `Console_eio.Prompt` | Asking the user a question, or a password without echoing it |
| `Console_text` | Removing colour codes and control characters from text you do not trust |
| `Console_vte` | Replaying a recorded terminal session, to test what it showed |

## Related work

- [Rich](https://github.com/Textualize/rich), the Python library console is
  modelled on.
- [lipgloss](https://github.com/charmbracelet/lipgloss), a Go library whose way
  of combining styles `Style` follows.
- [printbox](https://github.com/c-cube/printbox), which prints boxes, tables
  and trees in OCaml.
- [progress](https://github.com/CraigFe/progress), progress bars in OCaml.
- [notty](https://github.com/pqwy/notty), for full-screen OCaml programs.
- [down](https://erratique.ch/software/down), Daniel Bünzli's line editor
  for the OCaml toplevel, which `Console.Input` follows for its keys, history
  and completion.

## License

ISC. See [LICENSE.md](LICENSE.md).
