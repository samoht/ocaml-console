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

(* A formatter backed by an Eio flow may yield after any chunk. Records
   still form a complete event stream, regardless of the task interleaving. *)
let test_json_concurrent_records chunk tasks =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 256 in
  let chunk = 1 + (chunk mod 32) and tasks = 2 + (tasks mod 4) in
  let ppf =
    Format.make_formatter
      (fun text offset length ->
        let rec write offset length =
          if length > 0 then begin
            let n = min chunk length in
            Buffer.add_substring out text offset n;
            Eio.Fiber.yield ();
            write (offset + n) (length - n)
          end
        in
        write offset length)
      (fun () -> ())
  in
  Console_eio.run ~clock ~ppf ~progress:`Json (fun d ->
      Eio.Fiber.List.iter
        (fun i ->
          Console.Display.with_task d (string_of_int i) (fun t ->
              Console.Display.log t "line";
              Console.Display.set_count t ~cur:1 ~total:1))
        (List.init tasks Fun.id));
  Format.pp_print_flush ppf ();
  let records = events (Buffer.contents out) in
  check_eq ~pp:Fmt.int (tasks * 4) (List.length records);
  List.iter
    (fun i ->
      let own =
        List.filter
          (fun fields -> List.assoc_opt "id" fields = Some (`Int i))
          records
      in
      check_eq
        ~pp:Fmt.(Dump.list Dump.string)
        [ "start"; "output"; "progress"; "end" ]
        (List.map
           (fun fields ->
             match List.assoc "event" fields with `String s -> s | _ -> "")
           own))
    (List.init tasks Fun.id)

(* tty.exe replays the real terminal session through matrix.vte. The same
   capture rule applies to Display.log, Fmt on either output and Logs. *)
let test_quiet_terminal_records ending =
  let mode =
    match ending mod 3 with
    | 0 -> "quiet-ok"
    | 1 -> "quiet-fail"
    | _ -> "quiet-cancel"
  in
  let helper name =
    Filename.concat
      (Filename.dirname Sys.executable_name)
      ("../test/eio/" ^ name ^ ".exe")
  in
  let input =
    Unix.open_process_args_in (helper "tty")
      [| "tty.exe"; helper "teardown"; mode |]
  in
  let screen = In_channel.input_all input in
  check (Unix.close_process_in input = Unix.WEXITED 0);
  let lines =
    String.split_on_char '\n' screen
    |> List.filter (String.ends_with ~suffix:" line")
  in
  let expected =
    if ending mod 3 = 0 then [ "root line" ]
    else
      [
        "  \u{203a} display line";
        "  \u{203a} stdout line";
        "  \u{203a} stderr line";
        "  logs line";
        "root line";
      ]
  in
  check_eq ~pp:Fmt.(Dump.list Dump.string) expected lines

(* Consumers can pair every start with one end, follow a parent, and recover
   counts and log levels under interleaved success, failure and cancellation. *)
let test_json_lifecycle k =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer out in
  let levels =
    [|
      (Logs.App, "app");
      (Logs.Error, "error");
      (Logs.Warning, "warning");
      (Logs.Info, "info");
      (Logs.Debug, "debug");
    |]
  in
  let count = 1 + (k mod 8) in
  let expected = ref [] in
  Console_eio.run ~clock ~ppf ~progress:`Json (fun d ->
      Console.Display.with_task d "parent 界" (fun _ ->
          Eio.Fiber.List.iter
            (fun i ->
              let label = Fmt.str "worker-%d 👩‍💻" i in
              let ending = (k + i) mod 4 in
              let status =
                match ending with
                | 0 -> "succeeded"
                | 1 | 3 -> "failed"
                | _ -> "cancelled"
              in
              let level, level_name =
                levels.((k + i) mod Array.length levels)
              in
              expected := (label, status, level_name) :: !expected;
              let ready, announce = Eio.Promise.create () in
              let work () =
                Console.Display.with_task d label (fun task ->
                    Console.Display.set_bytes task ~cur:Int64.max_int
                      ~total:Int64.max_int;
                    Eio.Fiber.yield ();
                    let reporter = Logs.reporter () in
                    reporter.Logs.report Logs.default level ~over:ignore ignore
                      (fun m -> m "line-%d" i);
                    match ending with
                    | 1 -> Console.Display.fail task
                    | 2 ->
                        Eio.Promise.resolve announce ();
                        Eio.Fiber.await_cancel ()
                    | 3 -> failwith "worker failed"
                    | _ -> ())
              in
              try
                if ending = 2 then
                  Eio.Fiber.first work (fun () -> Eio.Promise.await ready)
                else work ()
              with Failure message when message = "worker failed" -> ())
            (List.init count Fun.id));
      ignore (Console.Display.task d "left running" : Console.Display.task);
      Console.Display.log (Console.Display.root d) "root line";
      Console.Display.set_result ~detail:"detail" d (Error "result"));
  Format.pp_print_flush ppf ();
  let records = events (Buffer.contents out) in
  let field name fields = List.assoc_opt name fields in
  let is_event event fields = field "event" fields = Some (`String event) in
  let starts = List.filter (is_event "start") records in
  let ends = List.filter (is_event "end") records in
  check_eq ~pp:Fmt.int (count + 2) (List.length starts);
  check_eq ~pp:Fmt.int (List.length starts) (List.length ends);
  List.iter
    (fun start ->
      let id = field "id" start in
      check (id <> None);
      let own = List.filter (fun record -> field "id" record = id) records in
      check_eq ~pp:Fmt.int 1 (List.length (List.filter (is_event "end") own));
      check (is_event "start" (List.hd own));
      check (is_event "end" (List.hd (List.rev own))))
    starts;
  let parent =
    List.find
      (fun f -> field "task" f = Some (`List [ `String "parent 界" ]))
      starts
  in
  List.iter
    (fun (label, status, level) ->
      let path = Some (`List [ `String "parent 界"; `String label ]) in
      let own = List.filter (fun f -> field "task" f = path) records in
      let start = List.find (is_event "start") own in
      check (field "parent" start = field "id" parent);
      let ending = List.find (is_event "end") own in
      check (field "status" ending = Some (`String status));
      let progress = List.find (is_event "progress") own in
      let integer = function
        | Some (`Int n) -> Int64.of_int n
        | Some (`Intlit n) -> Int64.of_string n
        | _ -> fail "missing integer count"
      in
      check (integer (field "count" progress) = Int64.max_int);
      check (integer (field "total" progress) = Int64.max_int);
      check (field "unit" progress = Some (`String "bytes"));
      let output = List.filter (is_event "output") own in
      check_eq ~pp:Fmt.int 1 (List.length output);
      check (field "level" (List.hd output) = Some (`String level)))
    !expected;
  let root =
    List.find (fun f -> field "text" f = Some (`String "root line")) records
  in
  check (field "task" root = Some (`List []));
  check (field "id" root = None);
  let abandoned =
    List.find
      (fun f ->
        is_event "end" f
        && field "task" f = Some (`List [ `String "left running" ]))
      records
  in
  check (field "status" abandoned = Some (`String "cancelled"));
  let result = List.hd (List.rev records) in
  check (is_event "result" result);
  check (field "status" result = Some (`String "error"));
  check (field "text" result = Some (`String "result"));
  check (field "detail" result = Some (`String "detail"))

let suite =
  ( "console_eio",
    [
      test_case "concurrent JSON records stay whole and complete"
        [ uint8; uint8 ] test_json_concurrent_records;
      test_case "quiet captures every terminal output source" [ uint8 ]
        test_quiet_terminal_records;
      test_case "JSON lifecycles keep counts parents levels and endings"
        [ uint8 ] test_json_lifecycle;
      test_case "JSON events read back" [ bytes; bytes ] test_json_events;
      test_case "a surviving fiber uses a live display" [ uint8 ]
        test_surviving_fiber;
      test_case "a refused record falls back and completes once"
        [ bytes; uint8 ] test_refused_record;
      test_case "a sibling keeps the display after the first callback ends"
        [ uint8; bool ] test_sibling_lifetime;
    ] )
