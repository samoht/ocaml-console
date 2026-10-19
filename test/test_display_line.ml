(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let test_large_percentage_does_not_overflow () =
  let count_ok = ref false and bytes_ok = ref false in
  let bar =
    Display.Line.(
      v [ label; spacer; percent; text " "; count; text " "; elapsed ])
  in
  let _vte =
    terminal_run ~height:5 ~width:72 (fun display _ sync vte ->
        let count = Display.task ~bar display "count" in
        Display.set_count count ~cur:max_int ~total:max_int;
        sync ();
        count_ok := contains "100%" (Vte.to_string vte);
        Display.succeed count;
        let bytes = Display.task ~bar display "bytes" in
        Display.set_bytes bytes ~cur:Int64.max_int ~total:Int64.max_int;
        sync ();
        bytes_ok := contains "100%" (Vte.to_string vte);
        Display.succeed bytes)
  in
  Alcotest.(check bool) "max_int percentage" true !count_ok;
  Alcotest.(check bool) "int64 percentage" true !bytes_ok

(* The row sits under an open group so that it stays drawn once it has
   finished, which is where its outcome must replace the marker. *)
let test_a_marker_moves_with_the_ticks () =
  let bar =
    Display.Line.(
      v
        [
          marker (fun ~frame -> Fmt.kstr Span.text "<%d>" frame);
          text " ";
          label;
        ])
  in
  let screens = ref [] in
  let _vte =
    terminal_run ~header:"BUILD" (fun display _advance sync vte ->
        let group = Display.task ~group:true display "entry" in
        let task = Display.task ~parent:group ~bar display "compile" in
        let shot () =
          sync ();
          screens := screen_rows vte :: !screens
        in
        shot ();
        shot ();
        Display.succeed task;
        shot ();
        Display.succeed group)
  in
  match List.rev !screens with
  | [ first; second; finished ] ->
      let row screen =
        match List.find_opt (contains "compile") screen with
        | Some row -> String.trim row
        | None -> Alcotest.fail "no compile row"
      in
      Alcotest.(check (list string))
        "the marker at frames 1 and 2"
        [ "└── <1> compile"; "└── <2> compile" ]
        [ row first; row second ];
      Alcotest.(check bool)
        "a finished row draws its outcome, not the marker" false
        (contains "<" (row finished))
  | screens -> Alcotest.failf "%d screens, expected 3" (List.length screens)

let suite =
  ( "display_line",
    [
      Alcotest.test_case "large percentage" `Quick
        test_large_percentage_does_not_overflow;
      Alcotest.test_case "a marker moves with the ticks" `Quick
        test_a_marker_moves_with_the_ticks;
    ] )
