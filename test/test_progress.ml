(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let contains affix s = Astring.String.is_infix ~affix s

let test_names () =
  Alcotest.(check (list string))
    "names"
    [ "auto"; "tty"; "plain"; "quiet"; "json" ]
    (List.map fst Progress.all);
  List.iter
    (fun (name, p) ->
      Alcotest.(check bool)
        ("of_string " ^ name) true
        (match Progress.of_string name with
        | Ok p' -> p' = p
        | Error _ -> false))
    Progress.all;
  match Progress.of_string "silent" with
  | Ok _ -> Alcotest.fail "unknown name accepted"
  | Error (`Msg m) ->
      Alcotest.(check bool) "names the choices" true (contains "quiet" m)

let suite = ("progress", [ Alcotest.test_case "names" `Quick test_names ])
