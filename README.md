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

Styles combine with `+`, and text in a style is a `Span.t`, printed by a
[`Fmt`](https://erratique.ch/software/fmt) printer. `Console_eio.setup`
turns colour on when the output is a terminal; without it, everything prints
plain:

```ocaml
open Console

let success = Style.(bold + fg Color.green)
let error = Style.(bold + fg Color.red)

let () =
  Console_eio.setup ();
  Fmt.pr "%a@." Span.pp (Span.styled success "✓ Build passed");
  Fmt.pr "%a@." Span.pp (Span.styled error "✗ Tests failed")
```

Tables and other widgets take their text as a `Span.t`, a piece of text with
its style and, optionally, a link. `Span.pp` prints the colours and links
when its formatter is set to colour, as `Console_eio.setup` sets standard
output on a terminal, and `Span.to_string` returns the plain text.

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
# let file name = Tree.v (Span.text name) [] in
  Fmt.pr "%a@." Tree.pp
    (Tree.v (Span.text "src")
       [
         Tree.v (Span.text "lib") [ file "console.ml"; file "table.ml" ];
         Tree.v (Span.text "test") [ file "test.ml" ];
       ]);;
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

`Rule` draws a horizontal line as wide as the output, with an optional title:

```ocaml
# Format.set_margin 40;;
- : unit = ()
# Fmt.pr "%a@." Console.Rule.pp
    (Console.Rule.v ~label:(Console.Span.text "Results") ());;
─────────────── Results ────────────────
- : unit = ()
```

`Layout` places blocks of text next to each other or one above the other:

```ocaml
open Console

let report =
  Layout.hcat ~gutter:2
    [ Fmt.const Panel.pp (Panel.v (Span.text "All green"));
      Fmt.const Panel.pp (Panel.v (Span.text "No warnings")) ]
```

```ocaml
# Fmt.pr "%a@." report ();;
╭───────────╮  ╭─────────────╮
│ All green │  │ No warnings │
╰───────────╯  ╰─────────────╯
- : unit = ()
```

### Progress

A `Display` shows one line per task that is still running, and updates
those lines in place. Log messages and finished tasks are printed once, above
them, and stay on screen. `Console_eio.run` opens a display and passes it to
your function:

```ocaml
open Console

let build display =
  let step = Display.task display "compile" in
  Display.set_count step ~cur:2 ~total:2;
  Display.log step "compiled parser.ml";
  Display.succeed step
```

```ocaml
# Eio_main.run @@ fun env ->
  Console_eio.run ~clock:(Eio.Stdenv.clock env) ~header:"Building" build;;
  # compiled parser.ml
=> compile  2/2  0.0s
[+] Building 0.0s (1/1)
- : unit = ()
```

The output here is not a terminal, so nothing is redrawn: these are the plain
lines a pipe or a CI log gets. Each line starts with a mark that says what it
is:

- `=>`, `!!` and `--` start the line of a task that succeeded, failed or was
  cancelled, followed by its count and the time it took;
- `#`, `!` and `x` start a log line that informs, warns or reports an error;
  the lines of a task are indented under it;
- the header line starts with `[-]` while tasks run, then `[+]` when all
  succeeded, `[x]` when one failed or `[~]` when one was cancelled, and ends
  with the time so far and the tasks that ended out of those started, here
  `(1/1)`. On a terminal it stays above the rows; in a pipe it is printed
  last, as a summary.

While a display runs, print through `Display.with_output`, which takes the
rows off the screen for the time it prints, rather than with `Fmt.pr`, which
would print in the middle of them.

`Bar.v` draws a bar on its own, here 65% full and twenty columns wide:

```ocaml
# print_endline (Span.to_string (Bar.v ~brackets:true ~width:20 0.65));;
▕█████████████░░░░░░░▏
- : unit = ()
```

A task shows a spinner until it knows how much work it has, then a progress
bar with a count, a speed and the time spent so far. If your function returns
while a task is still running, the task is marked as done. If your function
raises an exception, it is marked as cancelled. Calling `Console_eio.run`
inside another `Console_eio.run` reuses the same display.

### Animation

![Matrix ASCII rain demo](examples/matrix/matrix.gif)

Spinners are animations: values of type `Anim.t` that change over time.
`Anim.all` samples several of them at once, and `Layout.hcat` joins the frames
side by side. To see a display in the `Theme.matrix` colours, run the demo in
[`examples/matrix`](examples/matrix):

<!-- $MDX skip -->
```sh
$ dune exec examples/matrix/main.exe
```

## API

The `console` library:

| Module | What it does |
|---|---|
| `Console.Color` | Terminal colours, by name, by palette number or as RGB and hex |
| `Console.Style` | Bold, italic, underline and colours, combined with `+` |
| `Console.Gradient` | Colours that change from one cell to the next |
| `Console.Span` | Text in a style, with links, wrapping and truncation |
| `Console.Border` | Frame styles for tables and panels |
| `Console.Guide` | The lines that connect the branches of a tree |
| `Console.Theme` | One look for every widget |
| `Console.Table` | Tables with headers and aligned columns |
| `Console.Tree` | Trees, drawn like the output of `tree` |
| `Console.Panel` | Text in a frame, with an optional title |
| `Console.Rule` | Horizontal lines with an optional title |
| `Console.Layout` | Widgets side by side or one above the other |
| `Console.Spinner` | Spinners for work of unknown length |
| `Console.Bar` | Horizontal bars, for progress and for figures |
| `Console.Anim` | Values that change over time, for animations |
| `Console.Display` | Progress rows that update in place, or plain lines outside a terminal |
| `Console.Input` | Reading a line from the keyboard, with history and completion |

The `console-eio` library:

| Module | What it does |
|---|---|
| `Console_eio` | Runs a `Display` with [Eio](https://github.com/ocaml-multicore/eio): finds the terminal and its size, and redraws |
| `Console_eio.Prompt` | Asking the user a question, or a password without showing it |

Two small libraries that come with `console`:

| Library | Module | What it does |
|---|---|---|
| `console.text` | `Console_text` | Removing colour codes and control characters from text you do not trust |
| `console.vte` | `Console_vte` | Replaying a recorded terminal session, to test what it showed |

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
