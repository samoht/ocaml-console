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
let task = list bytes

(* Any event, but for its time: [other] names an event that is not
   listed. *)
let kind =
  choose
    [
      map
        [ int; task; option int ]
        (fun id t parent -> Event.start ?parent ~id t);
      map [ int; task; int64; int64; count_unit ] (fun id t count total unit ->
          Event.progress ~id ~count ~total ~unit t);
      map [ int; task; bytes ] (fun id t text -> Event.label ~id ~text t);
      map
        [ option int; option task; bytes; option level ]
        (fun id task s level -> Event.output ?id ?task ?level s);
      map
        [ int; task; status; float; option bytes ]
        (fun id t status elapsed text ->
          Event.end' ?text ~id ~status ~elapsed t);
      map
        [ outcome; bytes; option bytes ]
        (fun outcome s detail -> Event.result ?detail ~outcome s);
      map [ bytes ] (fun s ->
          if List.mem s known_events then bad_test () else Event.other s);
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
  let e = kind ~time in
  let line = Event.to_string e in
  check (String.is_valid_utf_8 line);
  check (no_control line);
  let back = decode line in
  check_eq ~pp:Fmt.Dump.string line (Event.to_string back);
  check (back = e)

(* [`Other s] is refused iff [s] is a listed name, and reads back as it
   is otherwise, for an event's name and for the values of its members. *)
let test_other s =
  let is_listed names = List.mem s names in
  let try_ listed make =
    match make () with
    | e ->
        if listed then failf "%S, a listed name, taken as other" s;
        check (decode (Event.to_string e) = e)
    | exception Invalid_argument _ ->
        if not listed then failf "%S refused as other" s
  in
  try_ (is_listed known_events) (fun () -> Event.other ~time:0. s);
  try_
    (is_listed [ "items"; "bytes" ])
    (fun () ->
      Event.progress ~time:0. ~id:0 ~count:0L ~total:0L ~unit:(`Other s) []);
  try_
    (is_listed [ "app"; "error"; "warning"; "info"; "debug" ])
    (fun () -> Event.output ~level:(`Other s) ~time:0. "");
  try_
    (is_listed [ "succeeded"; "failed"; "cancelled" ])
    (fun () -> Event.end' ~time:0. ~id:0 ~status:(`Other s) ~elapsed:0. []);
  try_
    (is_listed [ "ok"; "error" ])
    (fun () -> Event.result ~time:0. ~outcome:(`Other s) "")

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
  let e = decode (Event.to_string (Event.label ~time:0. ~id:0 ~text:s [ s ])) in
  match (Event.task e, Event.text e) with
  | [ l ], Some t ->
      check_eq ~pp:Fmt.Dump.string (cleaned s) l;
      check_eq ~pp:Fmt.Dump.string (cleaned s) t
  | _ -> fail "not the label event"

(* A time reads back rounded to the millisecond, held in [-10^12, 10^12]
   seconds, and is zero if it is not finite. *)
let test_time x =
  let back = Event.time (decode (Event.to_string (Event.other ~time:x "x"))) in
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
    Event.to_string
      (Event.progress ~time:0. ~id:0 ~count:c ~total:n ~unit:`Bytes [])
  in
  let exact = Int64.compare (Int64.abs c) 1_000_000_000_000_000L < 0 in
  let exact = exact && not (Int64.equal c Int64.min_int) in
  check
    (contains
       ~affix:
         (if exact then Fmt.str {|"count":%Ld,|} c
          else Fmt.str {|"count":"%Ld",|} c)
       line);
  let e = decode line in
  check (Event.count e = Some c);
  check (Event.total e = Some n)

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
            e
        | Error (`Msg e) -> failf "%s does not decode: %s" line e)
  in
  (events, (count, status, detail))

(* A display keeps the first 4 KiB of a label or a message, cut between
   two characters and ended with three dots. *)
let kept s =
  if String.length s <= 4096 then s
  else
    let head = String.sub s 0 (4096 - 3) in
    String.sub head 0 (Console_text.boundary head) ^ "..."

let public_schema_expected k raw ((count, unit), status, detail) =
  let clean s = Console_text.sanitize ~keep_newlines:false s in
  let label = kept (clean raw) and child = kept (clean (raw ^ "/child")) in
  let renamed = kept (clean (raw ^ "/new"))
  and message = kept (clean (raw ^ "/end")) in
  let outcome = if k land 2 = 0 then `Ok else `Error in
  let child_task = [ renamed; child ] in
  [
    Event.start ~time:0. ~id:0 [ label ];
    Event.start ~parent:0 ~time:0.125 ~id:1 [ label; child ];
    Event.label ~time:0.375 ~id:0 ~text:renamed [ renamed ];
    Event.progress ~time:0.375 ~id:1 ~count ~total:count ~unit child_task;
    Event.output ~id:1 ~task:child_task ~time:0.375 "line";
    Event.end' ~text:message ~time:0.375 ~id:1 ~status ~elapsed:0.25 child_task;
    Event.end' ~time:0.375 ~id:0 ~status:`Succeeded ~elapsed:0.375 [ renamed ];
    Event.output ~time:0.375 "root";
    Event.result ~detail ~time:0.375 ~outcome (clean raw);
  ]

let test_public_schema k text =
  let raw = if k mod 16 = 0 then String.make 65537 'x' ^ text else text in
  let events, values = public_schema_events k raw in
  let expected = public_schema_expected k raw values in
  check_eq ~pp:Fmt.int (List.length expected) (List.length events);
  List.iteri
    (fun i (expected, actual) ->
      if expected <> actual then
        failf "event %d is %a, not %a" i Event.pp actual Event.pp expected)
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
  test_round_trip time (fun ~time:t ->
      Event.end' ~time:t ~id:0 ~status:`Succeeded ~elapsed:time [ "work" ])

let suite =
  ( "progress",
    [
      test_case "event times read back at every finite magnitude"
        [ extreme_seconds ] test_extreme_seconds;
      test_case "an event reads back" [ float; kind ] test_round_trip;
      test_case "other is refused for a listed name"
        [
          value
            (known_events
            @ [ "items"; "bytes"; "app"; "error"; "warning"; "info"; "debug" ]
            @ [ "succeeded"; "failed"; "cancelled"; "ok" ]);
        ]
        test_other;
      test_case "a string reads back" [ bytes ] test_string;
      test_case "a time reads back to the millisecond" [ float ] test_time;
      test_case "a count reads back" [ int64; int64 ] test_count;
      test_case "public JSON schema keeps every field and renamed ancestry"
        [ uint8; bytes ] test_public_schema;
    ] )
