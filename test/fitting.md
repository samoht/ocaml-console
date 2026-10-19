# Fitting a row to the terminal's width

A summary row is one line: what does not fit is left out, and the row says
how much it left out. These cases are the contract of a `Table` fitted to a
width.

```ocaml
open Console

let paths =
  String.concat ", "
    (List.init 4000 (fun i -> Printf.sprintf "_build/p%d" (i + 1)))

let changed = "changed paths: " ^ paths
let queue =
  "1 queued · checking docs-mq/workflow (check, 11m15s) · overdue since 16:16"
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
       (Table.of_rows ~border:Border.none [ col; col; col ]
          @@ List.map (List.map Span.text)
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

An `` `Elide `` column keeps the leading items of a list that fit, then says
how many it left out. A spanning cell takes the overflow of the column it
starts in. A column that is not shrinkable keeps its width while another
yields.

```ocaml
# let fixed = Table.column ~shrinkable:false ~overflow:`Elide ""
val fixed : Table.column = <abstr>
# let reason = Table.column ~overflow:`Elide ""
val reason : Table.column = <abstr>
# let table =
    Table.of_rows ~border:Border.none [ fixed; fixed; reason; fixed ]
      [
        List.map Span.text [ "fix"; "stuck"; changed; "/pool/fix.3" ];
        List.map Span.text [ "staging"; queue ];
        [ Span.text "  14:05 queued fix for staging" ];
      ]
val table : Table.t = <abstr>
# print_string (Table.to_string ~width:70 table)
fix      stuck  changed paths: _build/p1 and 3999 more     /pool/fix.3
staging  1 queued and 2 more
  14:05 queued fix for staging
- : unit = ()
# print_string (Table.to_string ~width:60 table)
fix      stuck  changed paths... and 3999 more   /pool/fix.3
staging  1 queued and 2 more
  14:05 queued fix for staging
- : unit = ()
```

`Table.lines` is the same lines as styled spans, for a live screen to draw.

```ocaml
# List.map Span.to_string (Table.lines ~width:70 table)
- : string list =
["fix      stuck  changed paths: _build/p1 and 3999 more     /pool/fix.3";
 "staging  1 queued and 2 more"; "  14:05 queued fix for staging"]
```

A cut keeps the style of what it kept.

```ocaml
# let bold = Table.of_rows ~border:Border.none [ reason ] [ [ Span.styled Style.bold changed ] ]
val bold : Table.t = <abstr>
# List.map (Span.to_string ~styled:true) (Table.lines ~width:40 bold)
- : string list = ["\027[1mchanged paths: _build/p1\027[0m and 3999 more"]
```
