(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console
open Display_harness

let test_large_percentage_does_not_overflow () =
  let count_ok = ref false and bytes_ok = ref false in
  let layout =
    Display.Line.(
      v [ label; spacer; percent; text " "; count; text " "; elapsed ])
  in
  let _vte =
    terminal_run ~height:5 ~width:72 ~layout (fun display _ sync vte ->
        let count = Display.task display "count" in
        Display.set_count count ~cur:max_int ~total:max_int;
        sync ();
        count_ok := contains "100%" (Vte.to_string vte);
        Display.succeed count;
        let bytes = Display.task display "bytes" in
        Display.set_bytes bytes ~cur:Int64.max_int ~total:Int64.max_int;
        sync ();
        bytes_ok := contains "100%" (Vte.to_string vte);
        Display.succeed bytes)
  in
  Alcotest.(check bool) "max_int percentage" true !count_ok;
  Alcotest.(check bool) "int64 percentage" true !bytes_ok

let suite =
  ( "display_line",
    [
      Alcotest.test_case "large percentage" `Quick
        test_large_percentage_does_not_overflow;
    ] )
