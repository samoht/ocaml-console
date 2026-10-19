open Alcobar
open Console

let test a b c d =
  let g = Guide.v ~branch:a ~last:b ~pipe:c ~space:d in
  check
    (Guide.branch g = a
    && Guide.last g = b
    && Guide.pipe g = c
    && Guide.space g = d);
  List.iter
    (fun g ->
      let styled = Guide.with_style Style.bold g in
      check (Style.equal (Guide.style styled) Style.bold);
      check
        (Guide.branch styled = Guide.branch g
        && Guide.last styled = Guide.last g
        && Guide.pipe styled = Guide.pipe g
        && Guide.space styled = Guide.space g);
      ignore (Fmt.str "%a" Guide.pp g))
    [ g; Guide.ascii; Guide.unicode ]

let suite =
  ( "api.guide",
    [
      test_case "guide accessors and restyling"
        [ bytes; bytes; bytes; bytes ]
        test;
    ] )
