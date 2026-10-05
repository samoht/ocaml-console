# Fitting a row to the terminal's width

A summary row is one line: what does not fit is left out, and the row says
how much it left out and where the whole value can be read. These cases are
the contract of `Width.elide` and of a `Table` fitted to a width.

```ocaml
open Console

let paths =
  String.concat ", "
    (List.init 4000 (fun i -> Printf.sprintf "_build/p%d" (i + 1)))

let changed = "changed paths: " ^ paths
let refused = "check could not run: consumer /tmp/c has changed paths: " ^ paths
```

## A list keeps its leading items and counts the others

A value that fits is itself.

```ocaml
# Width.elide 20 "a, b"
- : string = "a, b"
```

Items are separated by `", "`; the leading ones that fit stay whole, then
`and N more` for the N left out.

```ocaml
# Width.elide 40 changed
- : string = "changed paths: _build/p1 and 3999 more"
```

`see` follows the count and names where the value is read whole; the room
it takes is the items'.

```ocaml
# Width.elide ~see:" (mq status x)" 60 changed
- : string = "changed paths: _build/p1 and 3999 more (mq status x)"
# Width.elide ~see:" (mq status x)" 80 changed
- : string =
"changed paths: _build/p1, _build/p2, _build/p3 and 3997 more (mq status x)"
```

A separator inside brackets does not end an item.

```ocaml
# Width.elide 33 "checks failed (build, test), lint skipped, fmt skipped"
- : string = "checks failed... and 2 more"
```

## An item too wide alone keeps its leading words

When the first item does not fit, its leading words that do are kept
(`Width.shorten`), then `...` marks the cut, then the count and `see`.

```ocaml
# Width.elide ~see:" (mq status x)" 50 refused
- : string = "check could not... and 3999 more (mq status x)"
```

A value that is not a list is the same case with no count.

```ocaml
# let queue = "1 queued · checking docs-mq/workflow (check, 11m15s) · overdue since 16:16"
val queue : string =
  "1 queued · checking docs-mq/workflow (check, 11m15s) · overdue since 16:16"
# Width.elide 30 queue
- : string = "1 queued · checking..."
```

What is kept ends on a word, never on the `·` that joined it to the next.

```ocaml
# Width.elide 20 queue
- : string = "1 queued..."
# Width.shorten 12 queue
- : string = "1 queued"
```

## When no word fits, the pointer alone

There is never a count without the items it counts: with no word of the
first item in the room, `see` alone is left, without its leading spaces,
and with no `see` the mark alone, each cut to the width.

```ocaml
# Width.elide ~see:" (mq status x)" 20 refused
- : string = "(mq status x)"
# Width.elide ~see:" (mq status x)" 5 refused
- : string = "(mq s"
# Width.elide 2 refused
- : string = ".."
```

Widths are terminal cells: a wide glyph takes two.

```ocaml
# Width.elide ~see:" (mq status 修正)" 44 refused
- : string = "check... and 3999 more (mq status 修正)"
```

## A table row fitted to a width

A row with fewer cells than the table has columns ends in a cell that spans
the columns left: it is measured against none of them, and in a table
without edges it runs to the end of the line. A one-cell row spans every
column, as a line under a row does. In a table without edges a line ends at
its last cell that is not empty.

```ocaml
# let col = Table.column ""
val col : Table.column = <abstr>
# print_string
    (Table.to_string
       (Table.of_string_rows ~border:Border.none [ col; col; col ]
          [
            [ "fix"; "stuck"; "/pool/fix.3" ];
            [ "staging"; "1 queued · overdue since 16:16" ];
            [ "  14:05 queued fix for staging, with a line longer than the table" ];
            [ "main"; ""; "" ];
          ]))
fix      stuck  /pool/fix.3
staging  1 queued · overdue since 16:16
  14:05 queued fix for staging, with a line longer than the table
main
- : unit = ()
```

An `` `Elide `` column fits its cells with `Width.elide`, the row's `see`
after a cell it cut. A spanning cell takes the overflow of the column it
starts in. A column that is not shrinkable keeps its width while another
yields.

```ocaml
# let fixed = Table.column ~shrinkable:false ~overflow:`Elide ""
val fixed : Table.column = <abstr>
# let reason = Table.column ~overflow:`Elide ""
val reason : Table.column = <abstr>
# let table =
    Table.v ~border:Border.none [ fixed; fixed; reason; fixed ]
    |> Table.add_row ~see:" (mq status fix)"
         (List.map Span.text [ "fix"; "stuck"; changed; "/pool/fix.3" ])
    |> Table.add_row (List.map Span.text [ "staging"; queue ])
    |> Table.add_row [ Span.text "  14:05 queued fix for staging" ]
val table : Table.t = <abstr>
# print_string (Table.to_string ~width:70 table)
fix      stuck  changed... and 3999 more (mq status fix)   /pool/fix.3
staging  1 queued · checking docs-mq/workflow (check, 11m15s)...
  14:05 queued fix for staging
- : unit = ()
# print_string (Table.to_string ~width:60 table)
fix      stuck  (mq status fix)                  /pool/fix.3
staging  1 queued · checking docs-mq/workflow...
  14:05 queued fix for staging
- : unit = ()
```

`Table.lines` is the same lines as styled spans, for a live screen to draw.

```ocaml
# List.map Span.to_string (Table.lines ~width:70 table)
- : string list =
["fix      stuck  changed... and 3999 more (mq status fix)   /pool/fix.3";
 "staging  1 queued · checking docs-mq/workflow (check, 11m15s)...";
 "  14:05 queued fix for staging"]
```

A cut keeps the style of what it kept.

```ocaml
# let bold = Table.v ~border:Border.none [ reason ] |> Table.add_row [ Span.styled Style.bold changed ]
val bold : Table.t = <abstr>
# List.map (Span.to_ansi_string) (Table.lines ~width:40 bold)
- : string list = ["\027[1mchanged paths: _build/p1\027[0m and 3999 more"]
```
