(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

(* [s] as valid UTF-8: each byte that is not part of a character is
   U+FFFD. *)
let repaired s =
  let buf = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then begin
      let d = String.get_utf_8_uchar s i in
      Buffer.add_utf_8_uchar buf
        (if Uchar.utf_decode_is_valid d then Uchar.utf_decode_uchar d
         else Uchar.rep);
      go (i + Uchar.utf_decode_length d)
    end
  in
  go 0;
  Buffer.contents buf

(* No byte of a JSON string is a control character, nor is a character
   of it C1. *)
let no_control s =
  String.for_all (fun c -> Char.code c >= 0x20 && c <> '\127') s
  && not
       (String.fold_left
          (fun (prev, found) c ->
            ( c,
              found
              || (prev = '\xc2' && Char.code c >= 0x80 && Char.code c <= 0x9f)
            ))
          ('\000', false) s
       |> snd)

(* Any bytes make a JSON string on one line, valid UTF-8 without a control
   character, that a JSON parser reads back as the bytes, each one not part
   of a character being U+FFFD. *)
let test_string s =
  let buf = Buffer.create 64 in
  Display_json.add_string buf s;
  let json = Buffer.contents buf in
  check (String.is_valid_utf_8 json);
  check (no_control json);
  match Yojson.Safe.from_string json with
  | `String back -> check_eq ~pp:Fmt.Dump.string (repaired s) back
  | v -> failf "not a string: %s" (Yojson.Safe.to_string v)
  | exception Yojson.Json_error e -> failf "%s does not parse: %s" json e

(* A number is a JSON number, the value to the thousandth. *)
let test_number x =
  let buf = Buffer.create 16 in
  Display_json.add_number buf x;
  match Yojson.Safe.from_string (Buffer.contents buf) with
  | `Float f ->
      check (Float.is_finite x);
      check (Float.abs (f -. x) <= 0.0005 *. Float.max 1. (Float.abs x))
  | `Int 0 -> check (not (Float.is_finite x))
  | `Int n -> failf "an integer %d for %h" n x
  | v -> failf "not a number: %s" (Yojson.Safe.to_string v)
  | exception Yojson.Json_error e -> failf "does not parse: %s" e

let suite =
  ( "display_json",
    [
      test_case "a string reads back" [ bytes ] test_string;
      test_case "a number reads back" [ float ] test_number;
    ] )
