(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

(* A logging transport can refuse any record. The caller still completes,
   with the record handed once to the reporter installed before the run. *)
(* A formatter whose first write raises the exception [k] picks. *)
let refusing k =
  let refused = ref false in
  Format.make_formatter
    (fun _ _ _ ->
      if not !refused then begin
        refused := true;
        match k mod 3 with
        | 0 -> failwith "transport refused"
        | 1 -> invalid_arg "transport refused"
        | _ -> raise End_of_file
      end)
    (fun () -> ())

let test_refused_record s k =
  Eio_mock.Backend.run @@ fun () ->
  let text = String.sub s 0 (min 64 (String.length s)) in
  let kept = ref [] in
  let report _ _ ~over continue msgf =
    msgf (fun ?header:_ ?tags:_ fmt ->
        Fmt.kstr
          (fun message ->
            kept := message :: !kept;
            over ();
            continue ())
          fmt)
  in
  let before = Logs.reporter () in
  Logs.set_reporter { Logs.report };
  Fun.protect ~finally:(fun () -> Logs.set_reporter before) @@ fun () ->
  let previous = Logs.reporter () in
  let ppf = refusing k in
  let over_count = ref 0 and continued = ref 0 in
  let clock = Eio_mock.Clock.Mono.make () in
  let failure =
    try
      Console_eio.run ~clock ~ppf (fun _ ->
          let reporter = Logs.reporter () in
          reporter.Logs.report Logs.default Logs.Info
            ~over:(fun () -> incr over_count)
            (fun () -> incr continued)
            (fun m -> m "%s" text));
      None
    with (Failure _ | Invalid_argument _ | End_of_file) as ex -> Some ex
  in
  check (Logs.reporter () == previous);
  (match failure with
  | None -> ()
  | Some ex -> failf "a Logs call raised %s" (Printexc.to_string ex));
  (* An empty message writes nothing, so nothing refuses it. *)
  let expected = if text = "" then [] else [ text ] in
  check_eq ~pp:Fmt.(Dump.list Dump.string) expected (List.rev !kept);
  check_eq ~pp:Fmt.int 1 !over_count;
  check_eq ~pp:Fmt.int 1 !continued

(* A joined sibling retains the display while its first callback unwinds,
   so its later task output still has a live destination and attribution. *)
let test_sibling_lifetime k failing =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer out in
  Fmt.set_utf_8 ppf true;
  let first = ref None and body_ended = ref false and returned = ref false in
  let entered, enter = Eio.Promise.create () in
  let count = 1 + (k mod 8) in
  Eio.Fiber.both
    (fun () ->
      let raised =
        try
          Console_eio.run ~clock ~ppf (fun d ->
              first := Some d;
              Eio.Promise.await entered;
              body_ended := true;
              if failing then failwith "first callback failed");
          false
        with Failure message when message = "first callback failed" -> true
      in
      returned := true;
      check_eq ~pp:Fmt.bool failing raised)
    (fun () ->
      Console_eio.run ~clock ~ppf (fun d ->
          check (Option.fold ~none:false ~some:(( == ) d) !first);
          Eio.Promise.resolve enter ();
          Eio.Fiber.yield ();
          check !body_ended;
          check (not !returned);
          Console.Display.with_task d "sibling" (fun task ->
              for i = 0 to count - 1 do
                if !returned then
                  fail "the first run returned while its sibling still logs";
                Console.Display.log task (Fmt.str "record-%d" i);
                Eio.Fiber.yield ()
              done)));
  Format.pp_print_flush ppf ();
  let raw = Buffer.contents out in
  check (not (String.contains raw '\027'));
  let records =
    String.split_on_char '\n' raw
    |> List.filter (String.starts_with ~prefix:"  ")
  in
  check_eq
    ~pp:Fmt.(Dump.list Dump.string)
    (List.init count (fun i -> Fmt.str "  \u{203a} sibling: record-%d" i))
    records;
  check !returned

(* A context inherited by a fiber is only a hint to a live display. Once
   that run ended, a later call opens a fresh display or joins the current one. *)
let test_surviving_fiber k =
  let failing = k land 8 <> 0 and active = k land 16 <> 0 in
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer out in
  Fmt.set_utf_8 ppf true;
  let old = ref None and current = ref None and actual = ref None in
  let count = 1 + (k mod 8) in
  let before = Logs.reporter () in
  Eio.Switch.run (fun sw ->
      let ended, finish = Eio.Promise.create () in
      let completed, complete = Eio.Promise.create () in
      (try
         Console_eio.run ~clock ~ppf (fun d ->
             old := Some d;
             Eio.Fiber.fork ~sw (fun () ->
                 Eio.Promise.await ended;
                 Console_eio.run ~clock ~ppf (fun d ->
                     actual := Some d;
                     Console.Display.with_task d "later" (fun task ->
                         for i = 0 to count - 1 do
                           Console.Display.log task (Fmt.str "record-%d" i)
                         done));
                 Eio.Promise.resolve complete ());
             if failing then failwith "first callback failed")
       with Failure message when message = "first callback failed" -> ());
      if active then
        Console_eio.run ~clock ~ppf (fun d ->
            current := Some d;
            Eio.Promise.resolve finish ();
            Eio.Promise.await completed)
      else Eio.Promise.resolve finish ());
  check (Logs.reporter () == before);
  let same a b = match (a, b) with Some a, Some b -> a == b | _ -> false in
  if same !actual !old then fail "a surviving fiber reused its closed display";
  if active then check (same !actual !current);
  Format.pp_print_flush ppf ();
  let raw = Buffer.contents out in
  check (not (String.contains raw '\027'));
  let records =
    String.split_on_char '\n' raw
    |> List.filter (String.starts_with ~prefix:"  ")
  in
  check_eq
    ~pp:Fmt.(Dump.list Dump.string)
    (List.init count (fun i -> Fmt.str "  \u{203a} later: record-%d" i))
    records

(* With [`Json], each line the display writes is a JSON object with a time
   and an event, and the labels and lines of output read back as the
   display shows them, whatever bytes they were given. *)
(* The objects of the lines of [out], each checked to be valid UTF-8 and
   to have a time. *)
let events out =
  String.split_on_char '\n' out
  |> List.filter (fun l -> l <> "")
  |> List.map (fun line ->
      check (String.is_valid_utf_8 line);
      match Yojson.Safe.from_string line with
      | `Assoc fields ->
          (match List.assoc_opt "time" fields with
          | Some (`Float _ | `Int _) -> ()
          | _ -> failf "no time in %s" line);
          fields
      | _ -> failf "not an object: %s" line
      | exception Yojson.Json_error e -> failf "%s does not parse: %s" line e)

let test_json_events label text =
  (* A tab goes to a tab stop, which a space does not need. *)
  let text = String.map (fun c -> if c = '\t' then ' ' else c) text in
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer out in
  Console_eio.run ~clock ~ppf ~progress:`Json (fun display ->
      Console.Display.with_task display label (fun task ->
          Console.Display.set_count task ~cur:1 ~total:2;
          Console.Display.log task text;
          Console.Display.set_label task label;
          Console.Display.fail task));
  Format.pp_print_flush ppf ();
  let events = events (Buffer.contents out) in
  let string name fields =
    match List.assoc_opt name fields with Some (`String s) -> s | _ -> ""
  in
  let of_event e = List.filter (fun f -> string "event" f = e) events in
  let shown = Console_text.sanitize ~keep_newlines:false label in
  List.iter
    (fun fields ->
      check_eq
        ~pp:Fmt.(Dump.option Dump.string)
        (Some shown)
        (match List.assoc_opt "task" fields with
        | Some (`List [ `String l ]) -> Some l
        | _ -> None))
    (of_event "start" @ of_event "end");
  let expected =
    match
      String.split_on_char '\n' (Console_text.sanitize text) |> List.rev
    with
    | "" :: rest -> List.rev rest
    | lines -> List.rev lines
  in
  let expected = if text = "" then [] else expected in
  check_eq
    ~pp:Fmt.(Dump.list Dump.string)
    expected
    (List.map (string "text") (of_event "output"));
  check_eq ~pp:Fmt.Dump.string "failed"
    (string "status" (List.hd (of_event "end")))

let suite =
  ( "console_eio",
    [
      test_case "JSON events read back" [ bytes; bytes ] test_json_events;
      test_case "a surviving fiber uses a live display" [ uint8 ]
        test_surviving_fiber;
      test_case "a refused record falls back and completes once"
        [ bytes; uint8 ] test_refused_record;
      test_case "a sibling keeps the display after the first callback ends"
        [ uint8; bool ] test_sibling_lifetime;
    ] )
