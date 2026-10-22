(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
module Event = Console.Progress.Event

let known_events = [ "start"; "progress"; "label"; "output"; "end"; "result" ]

(* Strings that are, often, a listed value of a member. *)
let value listed = choose (bytes :: List.map const listed)

let open_value listed =
  map
    [ value (List.map fst listed) ]
    (fun s ->
      match List.assoc_opt s listed with Some v -> v | None -> `Other s)

let count_unit = open_value [ ("items", `Items); ("bytes", `Bytes) ]

let level =
  open_value
    [
      ("app", `App);
      ("error", `Error);
      ("warning", `Warning);
      ("info", `Info);
      ("debug", `Debug);
    ]

let status =
  open_value
    [
      ("succeeded", `Succeeded); ("failed", `Failed); ("cancelled", `Cancelled);
    ]

let outcome = open_value [ ("ok", `Ok); ("error", `Error) ]
let task = pair int (list bytes)

(* Any event: [`Other] names an event that is not listed. *)
let kind =
  choose
    [
      map [ task; option int ] (fun t p -> `Start (t, p));
      map [ task; int64; int64; count_unit ] (fun t c n u ->
          `Progress (t, c, n, u));
      map [ task; bytes ] (fun t s -> `Label (t, s));
      map [ option task; bytes; option level ] (fun t s l -> `Output (t, s, l));
      map
        [ task; status; float; option bytes ]
        (fun t s x m -> `End (t, s, x, m));
      map [ outcome; bytes; option bytes ] (fun o s d -> `Result (o, s, d));
      map [ bytes ] (fun s ->
          if List.mem s known_events then bad_test () else `Other s);
    ]

(* No byte of a line is a control character, nor is a character of it
   C1. *)
let no_control s =
  String.for_all (fun c -> Char.code c >= 0x20 && c <> '\127') s
  &&
  let rec no_c1 i =
    i >= String.length s
    ||
    let d = String.get_utf_8_uchar s i in
    let c = Uchar.to_int (Uchar.utf_decode_uchar d) in
    (c < 0x80 || c > 0x9f) && no_c1 (i + Uchar.utf_decode_length d)
  in
  no_c1 0

let decode line =
  match Event.of_string line with
  | Ok e -> e
  | Error (`Msg e) -> failf "%S does not decode: %s" line e

(* An event's line decodes, and its event is written as the line is: valid
   UTF-8 on one line, without a control character. *)
let test_round_trip time kind =
  let line = Event.to_string (Event.make ~time kind) in
  check (String.is_valid_utf_8 line);
  check (no_control line);
  check_eq ~pp:Fmt.Dump.string line (Event.to_string (decode line))

(* [s] as an event holds it: each byte that is not part of a character and
   each C1 control is U+FFFD. *)
let cleaned s =
  let buf = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s then begin
      let d = String.get_utf_8_uchar s i in
      let u = Uchar.utf_decode_uchar d in
      let c = Uchar.to_int u in
      Buffer.add_utf_8_uchar buf
        (if Uchar.utf_decode_is_valid d && (c < 0x80 || c > 0x9f) then u
         else Uchar.rep);
      go (i + Uchar.utf_decode_length d)
    end
  in
  go 0;
  Buffer.contents buf

(* Any bytes read back as valid UTF-8 without a C1 control, the bytes of
   each other character kept. *)
let test_string s =
  match
    Event.kind
      (decode (Event.to_string (Event.make ~time:0. (`Label ((0, [ s ]), s)))))
  with
  | `Label ((0, [ l ]), t) ->
      check_eq ~pp:Fmt.Dump.string (cleaned s) l;
      check_eq ~pp:Fmt.Dump.string (cleaned s) t
  | _ -> fail "not the label event"

(* A time reads back rounded to the millisecond, held in [-10^12, 10^12]
   seconds, and is zero if it is not finite. *)
let test_time x =
  let back =
    Event.time (decode (Event.to_string (Event.make ~time:x (`Other "x"))))
  in
  if not (Float.is_finite x) then check_eq ~pp:Fmt.float 0. back
  else
    let x = Float.min 1e12 (Float.max (-1e12) x) in
    check_eq ~pp:Fmt.float ((Float.round (x *. 1000.) /. 1000.) +. 0.) back

let contains ~affix s =
  let n = String.length affix in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = affix || go (i + 1))
  in
  go 0

(* A count reads back as it was, a JSON number below 10^15 in magnitude and
   a string of decimal digits from there. *)
let test_count c n =
  let line =
    Event.to_string (Event.make ~time:0. (`Progress ((0, []), c, n, `Bytes)))
  in
  let exact = Int64.compare (Int64.abs c) 1_000_000_000_000_000L < 0 in
  let exact = exact && not (Int64.equal c Int64.min_int) in
  check
    (contains
       ~affix:
         (if exact then Fmt.str {|"count":%Ld,|} c
          else Fmt.str {|"count":"%Ld",|} c)
       line);
  match Event.kind (decode line) with
  | `Progress (_, c', n', `Bytes) ->
      check (Int64.equal c c');
      check (Int64.equal n n')
  | _ -> fail "not the progress event"

(* Check every documented event field through the public Display API,
   including arbitrary bytes in labels, end messages and result text: each
   line decodes with Progress.Event.jsont, is written back as it was, and
   is the event expected. *)
let public_schema_events k raw =
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
  let count =
    if k land 1 = 0 then begin
      Display.set_count task ~cur:max_int ~total:max_int;
      (Int64.of_int max_int, `Items)
    end
    else begin
      Display.set_bytes task ~cur:Int64.max_int ~total:Int64.max_int;
      (Int64.max_int, `Bytes)
    end
  in
  Display.log task "line";
  let ending, status =
    match k mod 3 with
    | 0 -> (Display.succeed, `Succeeded)
    | 1 -> (Display.fail, `Failed)
    | _ -> (Display.cancel, `Cancelled)
  in
  ending ~message:(raw ^ "/end") task;
  Display.succeed parent;
  Display.log (Display.root d) "root";
  let detail = clean raw ^ "\ndetail" in
  Display.set_result ~detail d (if k land 2 = 0 then Ok raw else Error raw);
  Display.close d;
  Format.pp_print_flush ppf ();
  let events =
    String.split_on_char '\n' (Buffer.contents out)
    |> List.filter (( <> ) "")
    |> List.map (fun line ->
        check (String.is_valid_utf_8 line);
        check (no_control line);
        match Progress.Event.of_string line with
        | Ok e ->
            check_eq ~pp:Fmt.Dump.string line (Progress.Event.to_string e);
            (Progress.Event.time e, Progress.Event.kind e)
        | Error (`Msg e) -> failf "%s does not decode: %s" line e)
  in
  (events, (count, status, detail))

let public_schema_expected k raw ((count, unit), status, detail) =
  let clean s = Console_text.sanitize ~keep_newlines:false s in
  let label = clean raw and child = clean (raw ^ "/child") in
  let renamed = clean (raw ^ "/new") and message = clean (raw ^ "/end") in
  let outcome = if k land 2 = 0 then `Ok else `Error in
  [
    (0., `Start ((0, [ label ]), None));
    (0.125, `Start ((1, [ label; child ]), Some 0));
    (0.375, `Label ((0, [ renamed ]), renamed));
    (0.375, `Progress ((1, [ renamed; child ]), count, count, unit));
    (0.375, `Output (Some (1, [ renamed; child ]), "line", None));
    (0.375, `End ((1, [ renamed; child ]), status, 0.25, Some message));
    (0.375, `End ((0, [ renamed ]), `Succeeded, 0.375, None));
    (0.375, `Output (None, "root", None));
    (0.375, `Result (outcome, label, Some detail));
  ]

let test_public_schema k text =
  let raw = if k mod 16 = 0 then String.make 65537 'x' ^ text else text in
  let events, values = public_schema_events k raw in
  let expected = public_schema_expected k raw values in
  check_eq ~pp:Fmt.int (List.length expected) (List.length events);
  List.iteri
    (fun i (expected, actual) ->
      if expected <> actual then failf "event %d is not the one expected" i)
    (List.combine expected events)

(* Include the finite limits and all IEEE-754 bit patterns, rather than
   only the ordinary magnitudes produced by the float generator. *)
let extreme_seconds =
  choose
    [
      const Float.max_float;
      const (-.Float.max_float);
      map [ int64 ] Int64.float_of_bits;
    ]

let test_extreme_seconds time =
  test_round_trip time (`End ((0, [ "work" ]), `Succeeded, time, None))

let suite =
  ( "progress",
    [
      test_case "event times read back at every finite magnitude"
        [ extreme_seconds ] test_extreme_seconds;
      test_case "an event reads back" [ float; kind ] test_round_trip;
      test_case "a string reads back" [ bytes ] test_string;
      test_case "a time reads back to the millisecond" [ float ] test_time;
      test_case "a count reads back" [ int64; int64 ] test_count;
      test_case "public JSON schema keeps every field and renamed ancestry"
        [ uint8; bytes ] test_public_schema;
    ] )
