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

(* Check every documented event field through the public Display API,
   including arbitrary bytes in labels, end messages and result text. *)
let public_schema_records k raw =
  let open Console in
  let clean s = Console_text.sanitize ~keep_newlines:false s in
  let now = ref 42. and out = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer out in
  let d = Display.v ~progress:`Json ~now:(fun () -> !now) ppf in
  let parent = Display.task d raw in
  now := 42.125;
  let task = Display.task ~parent ~transient:true d (raw ^ "/child") in
  now := 42.375;
  Display.set_label parent (raw ^ "/new");
  let count, total, unit_name =
    if k land 1 = 0 then begin
      Display.set_count task ~cur:max_int ~total:max_int;
      (`Int max_int, `Int max_int, "items")
    end
    else begin
      Display.set_bytes task ~cur:Int64.max_int ~total:Int64.max_int;
      let json = Yojson.Safe.from_string (Int64.to_string Int64.max_int) in
      (json, json, "bytes")
    end
  in
  Display.log task "line";
  let ending, status =
    match k mod 3 with
    | 0 -> (Display.succeed, "succeeded")
    | 1 -> (Display.fail, "failed")
    | _ -> (Display.cancel, "cancelled")
  in
  ending ~message:(raw ^ "/end") task;
  Display.succeed parent;
  Display.log (Display.root d) "root";
  let detail = clean raw ^ "\ndetail" in
  Display.set_result ~detail d (if k land 2 = 0 then Ok raw else Error raw);
  Display.close d;
  Format.pp_print_flush ppf ();
  let records =
    String.split_on_char '\n' (Buffer.contents out)
    |> List.filter (( <> ) "")
    |> List.map (fun line ->
        check (String.is_valid_utf_8 line);
        check (String.for_all (fun c -> Char.code c >= 0x20) line);
        match Yojson.Safe.from_string line with
        | `Assoc fields -> fields
        | _ -> fail "not an object")
  in
  (records, (count, total, unit_name, status, detail))

let json_path labels = `List (List.map (fun s -> `String s) labels)

let public_schema_expected k raw (count, total, unit_name, status, detail) =
  let clean s = Console_text.sanitize ~keep_newlines:false s in
  let label = clean raw and child = clean (raw ^ "/child") in
  let renamed = clean (raw ^ "/new") and message = clean (raw ^ "/end") in
  let event time name fields =
    ("time", `Float time) :: ("event", `String name) :: fields
  in
  let own id labels = [ ("id", `Int id); ("task", json_path labels) ] in
  [
    event 0. "start" (own 0 [ label ]);
    event 0.125 "start" (own 1 [ label; child ] @ [ ("parent", `Int 0) ]);
    event 0.375 "label" (own 0 [ renamed ] @ [ ("text", `String renamed) ]);
    event 0.375 "progress"
      (own 1 [ renamed; child ]
      @ [ ("count", count); ("total", total); ("unit", `String unit_name) ]);
    event 0.375 "output"
      (own 1 [ renamed; child ] @ [ ("text", `String "line") ]);
    event 0.375 "end"
      (own 1 [ renamed; child ]
      @ [
          ("status", `String status);
          ("elapsed", `Float 0.25);
          ("text", `String message);
        ]);
    event 0.375 "end"
      (own 0 [ renamed ]
      @ [ ("status", `String "succeeded"); ("elapsed", `Float 0.375) ]);
    event 0.375 "output" [ ("task", json_path []); ("text", `String "root") ];
    event 0.375 "result"
      [
        ("status", `String (if k land 2 = 0 then "ok" else "error"));
        ("text", `String label);
        ("detail", `String detail);
      ];
  ]

let test_public_schema k text =
  let raw = if k mod 16 = 0 then String.make 65537 'x' ^ text else text in
  let records, values = public_schema_records k raw in
  let expected = public_schema_expected k raw values in
  check_eq ~pp:Fmt.int (List.length expected) (List.length records);
  List.iter2
    (fun expected actual ->
      List.iter
        (fun (key, value) ->
          if
            not
              (Option.equal Yojson.Safe.equal
                 (List.assoc_opt key actual)
                 (Some value))
          then
            failf "wrong JSON field %s on event %s" key
              (Yojson.Safe.to_string (List.assoc "event" actual)))
        expected)
    expected records;
  List.iter
    (fun fields ->
      let event = List.assoc "event" fields in
      if Yojson.Safe.equal event (`String "output") then
        check (not (List.mem_assoc "level" fields));
      if
        Option.equal Yojson.Safe.equal
          (List.assoc_opt "task" fields)
          (Some (json_path []))
        || Yojson.Safe.equal event (`String "result")
      then check (not (List.mem_assoc "id" fields)))
    records

let suite =
  ( "display_json",
    [
      test_case "public JSON schema keeps every field and renamed ancestry"
        [ uint8; bytes ] test_public_schema;
      test_case "a string reads back" [ bytes ] test_string;
      test_case "a number reads back" [ float ] test_number;
    ] )
