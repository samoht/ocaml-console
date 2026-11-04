(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let json = Alcotest.testable Jsont.pp_json Jsont.Json.equal

let parse line =
  if not (String.is_valid_utf_8 line) then
    Alcotest.failf "not valid UTF-8: %S" line;
  match Jsont_bytesrw.decode_string Jsont.json line with
  | Ok v -> v
  | Error e -> Alcotest.failf "not JSON: %S: %s" line e

type event = { time : float; code : string; data : string }

let event line =
  match parse line with
  | Jsont.Array ([ Number (time, _); String (code, _); String (data, _) ], _) ->
      { time; code; data }
  | _ -> Alcotest.failf "not an event: %S" line

let events = function [] -> [] | _header :: lines -> List.map event lines
