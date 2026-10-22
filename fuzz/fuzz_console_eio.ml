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

let json_field_is name value fields =
  Option.equal Yojson.Safe.equal (List.assoc_opt name fields) value

let json_event event = json_field_is "event" (Some (`String event))

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
      let own = List.filter (json_field_is "id" (Some (`Int i))) records in
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
  check
    (match Unix.close_process_in input with
    | Unix.WEXITED 0 -> true
    | _ -> false);
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
let lifecycle_levels =
  [|
    (Logs.App, "app");
    (Logs.Error, "error");
    (Logs.Warning, "warning");
    (Logs.Info, "info");
    (Logs.Debug, "debug");
  |]

let lifecycle_worker k d i =
  let label = Fmt.str "worker-%d 👩‍💻" i in
  let ending = (k + i) mod 4 in
  let status =
    match ending with 0 -> "succeeded" | 1 | 3 -> "failed" | _ -> "cancelled"
  in
  let level, level_name =
    lifecycle_levels.((k + i) mod Array.length lifecycle_levels)
  in
  let ready, announce = Eio.Promise.create () in
  let work () =
    Console.Display.with_task d label (fun task ->
        Console.Display.set_bytes task ~cur:Int64.max_int ~total:Int64.max_int;
        Eio.Fiber.yield ();
        let reporter = Logs.reporter () in
        reporter.Logs.report Logs.default level ~over:ignore ignore (fun m ->
            m "line-%d" i);
        match ending with
        | 1 -> Console.Display.fail task
        | 2 ->
            Eio.Promise.resolve announce ();
            Eio.Fiber.await_cancel ()
        | 3 -> failwith "worker failed"
        | _ -> ())
  in
  (try
     if ending = 2 then Eio.Fiber.first work (fun () -> Eio.Promise.await ready)
     else work ()
   with Failure message when message = "worker failed" -> ());
  (label, status, level_name)

let capture_lifecycles k count =
  Eio_mock.Backend.run @@ fun () ->
  let clock = Eio_mock.Clock.Mono.make () in
  let out = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer out in
  let expected = ref [] in
  Console_eio.run ~clock ~ppf ~progress:`Json (fun d ->
      Console.Display.with_task d "parent 界" (fun _ ->
          Eio.Fiber.List.iter
            (fun i ->
              let ending = lifecycle_worker k d i in
              expected := ending :: !expected)
            (List.init count Fun.id));
      ignore (Console.Display.task d "left running" : Console.Display.task);
      Console.Display.log (Console.Display.root d) "root line";
      Console.Display.set_result ~detail:"detail" d (Error "result"));
  Format.pp_print_flush ppf ();
  (events (Buffer.contents out), !expected)

let check_lifecycle_pairs count records =
  let starts = List.filter (json_event "start") records in
  let ends = List.filter (json_event "end") records in
  check_eq ~pp:Fmt.int (count + 2) (List.length starts);
  check_eq ~pp:Fmt.int (List.length starts) (List.length ends);
  List.iter
    (fun start ->
      let id = List.assoc_opt "id" start in
      check (Option.is_some id);
      let own = List.filter (json_field_is "id" id) records in
      check_eq ~pp:Fmt.int 1 (List.length (List.filter (json_event "end") own));
      check (json_event "start" (List.hd own));
      check (json_event "end" (List.hd (List.rev own))))
    starts;
  List.find (json_field_is "task" (Some (`List [ `String "parent 界" ]))) starts

let check_worker_events records parent (label, status, level) =
  let path = Some (`List [ `String "parent 界"; `String label ]) in
  let own = List.filter (json_field_is "task" path) records in
  let start = List.find (json_event "start") own in
  check (json_field_is "parent" (List.assoc_opt "id" parent) start);
  let ending = List.find (json_event "end") own in
  check (json_field_is "status" (Some (`String status)) ending);
  let progress = List.find (json_event "progress") own in
  let integer = function
    | Some (`Int n) -> Int64.of_int n
    | Some (`Intlit n) -> Int64.of_string n
    | _ -> fail "missing integer count"
  in
  check (Int64.equal (integer (List.assoc_opt "count" progress)) Int64.max_int);
  check (Int64.equal (integer (List.assoc_opt "total" progress)) Int64.max_int);
  check (json_field_is "unit" (Some (`String "bytes")) progress);
  let output = List.filter (json_event "output") own in
  check_eq ~pp:Fmt.int 1 (List.length output);
  check (json_field_is "level" (Some (`String level)) (List.hd output))

let test_json_lifecycle k =
  let count = 1 + (k mod 8) in
  let records, expected = capture_lifecycles k count in
  check_eq ~pp:Fmt.int count (List.length expected);
  let parent = check_lifecycle_pairs count records in
  List.iter (check_worker_events records parent) expected;
  let root =
    List.find (json_field_is "text" (Some (`String "root line"))) records
  in
  check (json_field_is "task" (Some (`List [])) root);
  check (json_field_is "id" None root);
  let abandoned =
    List.find
      (fun f ->
        json_event "end" f
        && json_field_is "task" (Some (`List [ `String "left running" ])) f)
      records
  in
  check (json_field_is "status" (Some (`String "cancelled")) abandoned);
  let result = List.hd (List.rev records) in
  check (json_event "result" result);
  check (json_field_is "status" (Some (`String "error")) result);
  check (json_field_is "text" (Some (`String "result")) result);
  check (json_field_is "detail" (Some (`String "detail")) result)

(* Forced tty mode must keep the display's screen guarantees even when the
   destination has no terminal, using COLUMNS and LINES for its geometry. *)
let with_dimensions width height f =
  let bindings =
    [ ("COLUMNS", string_of_int width); ("LINES", string_of_int height) ]
  in
  let saved =
    List.map (fun (name, _) -> (name, Sys.getenv_opt name)) bindings
  in
  List.iter (fun (name, value) -> Unix.putenv name value) bindings;
  Fun.protect f ~finally:(fun () ->
      List.iter
        (fun (name, value) -> Unix.putenv name (Option.value value ~default:""))
        saved)

let tty_destination width height k =
  let screen = Console_vte.v ~raw:true ~rows:height ~cols:width () in
  let written = Buffer.create 256 in
  let ppf =
    Format.make_formatter
      (fun text offset length ->
        let chunk = String.sub text offset length in
        check (String.is_valid_utf_8 chunk);
        String.split_on_char '\n' (Console_text.strip chunk)
        |> List.iter (fun line ->
            check (Console.Span.width (Console.Span.text line) <= width));
        Buffer.add_string written chunk;
        Console_vte.feed screen chunk)
      (fun () -> ())
  in
  Fmt.set_utf_8 ppf true;
  Fmt.set_style_renderer ppf `Ansi_tty;
  let _, theme =
    List.nth Console.Theme.all (k mod List.length Console.Theme.all)
  in
  Console.Theme.set ppf (Console.Theme.still theme);
  (screen, written, ppf)

let test_forced_tty_screen k label =
  let width, height = [| (1, 1); (80, 2); (300, 10); (20, 8) |].(k mod 4) in
  with_dimensions width height (fun () ->
      Eio_mock.Backend.run @@ fun () ->
      let clock = Eio_mock.Clock.Mono.make () in
      let screen, written, ppf = tty_destination width height k in
      let raises = k land 4 <> 0 in
      let raised =
        try
          Console_eio.run ~clock ~ppf ~progress:`Tty (fun d ->
              check (not (Console_vte.cursor_visible screen));
              for i = 0 to k mod 32 do
                let task = Console.Display.task d (string_of_int i ^ label) in
                Console.Display.set_count task ~cur:1 ~total:2
              done;
              Console.Display.redraw d;
              let before = Buffer.length written in
              Console.Display.redraw d;
              check_eq ~pp:Fmt.int before (Buffer.length written);
              if raises then failwith "body failed");
          false
        with Failure message when message = "body failed" -> true
      in
      check_eq ~pp:Fmt.bool raises raised;
      check (Console_vte.cursor_visible screen);
      check_eq ~pp:Fmt.int 0 (snd (Console_vte.cursor_pos screen));
      (* A subsequent ordinary write remains on the main screen in order. *)
      Console_vte.feed screen "Z\r\n";
      check
        (List.exists
           (fun line -> String.contains line 'Z')
           (Console_vte.screen ~scrollback:true screen)))

(* What [run] writes on {!Fmt.stdout} as [ppf], which it also takes the
   program's lines from under [progress]. *)
let capture_stdout progress f =
  Eio_mock.Backend.run @@ fun () ->
  Format.pp_print_flush Fmt.stdout ();
  let saved = Format.pp_get_formatter_out_functions Fmt.stdout () in
  let out = Buffer.create 256 in
  Format.pp_set_formatter_out_functions Fmt.stdout
    {
      saved with
      out_string = Buffer.add_substring out;
      out_flush = (fun () -> ());
    };
  Fun.protect
    ~finally:(fun () -> Format.pp_set_formatter_out_functions Fmt.stdout saved)
    (fun () ->
      Console_eio.run
        ~clock:(Eio_mock.Clock.Mono.make ())
        ~ppf:Fmt.stdout ~progress f;
      Format.pp_print_flush Fmt.stdout ());
  Buffer.contents out

(* Quiet, a line that a task leaves unended, flushed or not, is the output
   of no task: it goes out as it is when the display closes, after a run in
   which every task succeeded too, and the terminal is left usable. *)
let test_quiet_unended_line size flush =
  let text = String.make (1 + (size mod 64)) 'x' in
  let out =
    capture_stdout `Quiet (fun d ->
        Console.Display.with_task d "success" (fun _ ->
            Fmt.pr "%s" text;
            if flush then Format.pp_print_flush Fmt.stdout ();
            Eio.Fiber.yield ()))
  in
  check_eq ~pp:Fmt.Dump.string text out;
  let screen = Console_vte.v ~raw:true ~rows:8 ~cols:80 () in
  Console_vte.feed screen out;
  check (Console_vte.cursor_visible screen)

(* Two tasks that each begin a line and yield share the line under way: a
   line is the output of the task current where its line feed is written,
   inside that task's start and end, and a line never ended is one root
   output event after every end. No byte is lost or repeated. *)
let test_json_line_attribution size newline =
  let prefix i = string_of_int i ^ String.make (1 + (size mod 32)) 'x' in
  let out =
    capture_stdout `Json (fun d ->
        Eio.Fiber.List.iter
          (fun i ->
            Console.Display.with_task d (string_of_int i) (fun _ ->
                Fmt.pr "%s%!" (prefix i);
                Eio.Fiber.yield ();
                Fmt.pr "done";
                if newline then Fmt.pr "@."
                else Format.pp_print_flush Fmt.stdout ()))
          [ 0; 1 ])
  in
  let records = events out in
  let active = Hashtbl.create 2 and ended = ref 0 in
  let text = Buffer.create 64 in
  List.iter
    (fun fields ->
      let is event = json_event event fields in
      let id =
        match List.assoc_opt "id" fields with
        | Some (`Int i) -> Some i
        | _ -> None
      in
      let output () =
        match List.assoc_opt "text" fields with
        | Some (`String s) -> Buffer.add_string text s
        | _ -> fail "an output event with no text"
      in
      match id with
      | Some i when is "start" -> Hashtbl.replace active i ()
      | Some i when is "end" ->
          Hashtbl.remove active i;
          incr ended
      | Some i when is "output" ->
          check newline;
          check (Hashtbl.mem active i);
          (match List.assoc_opt "text" fields with
          | Some (`String s) -> check (String.ends_with ~suffix:"done" s)
          | _ -> ());
          output ()
      | None when is "output" ->
          check (not newline);
          check_eq ~pp:Fmt.int 2 !ended;
          output ()
      | _ -> ())
    records;
  let sorted s =
    String.to_seq s |> List.of_seq |> List.sort Char.compare |> List.to_seq
    |> String.of_seq
  in
  check_eq ~pp:Fmt.Dump.string
    (sorted (prefix 0 ^ prefix 1 ^ "donedone"))
    (sorted (Buffer.contents text))

(* The JSON representation of a printed line is independent of how the
   formatter chunks or flushes it, including at the 64 KiB capture bound. *)
let test_json_long_line size split =
  let length = if size land 1 = 0 then 65536 + size else 1 + size in
  let text = String.make length 'x' in
  let cut = min length (split * 257) in
  let out =
    capture_stdout `Json (fun d ->
        Console.Display.with_task d "large" (fun _ ->
            Fmt.pr "%s%!" (String.sub text 0 cut);
            Fmt.pr "%s@." (String.sub text cut (length - cut))))
  in
  let output = events out |> List.filter (json_event "output") in
  check_eq ~pp:Fmt.int 1 (List.length output);
  check (json_field_is "id" (Some (`Int 0)) (List.hd output));
  check (json_field_is "text" (Some (`String text)) (List.hd output))

(* What [run ppf] writes, with [ppf] a buffer's formatter or
   {!Format.str_formatter}, in [theme]. *)
let capture_progress_formatter ~theme ~use_str run =
  let buf = Buffer.create 256 in
  let ppf =
    if use_str then begin
      ignore (Format.flush_str_formatter ());
      Format.str_formatter
    end
    else Format.formatter_of_buffer buf
  in
  let old_theme = Console.Theme.get ppf in
  Console.Theme.set ppf theme;
  Fun.protect
    ~finally:(fun () -> Console.Theme.set ppf old_theme)
    (fun () ->
      run ppf;
      Format.pp_print_flush ppf ();
      if use_str then Format.flush_str_formatter () else Buffer.contents buf)

(* With output the program does not print itself, Display.v and run show
   each progress mode alike, on Format.str_formatter too and in every
   theme. *)
let test_direct_progress_parity k label use_str =
  let label = String.sub label 0 (min 128 (String.length label)) in
  let _, theme =
    List.nth Console.Theme.all (k mod List.length Console.Theme.all)
  in
  let program d =
    Console.Display.with_task d label (fun parent ->
        let child =
          Console.Display.task ~parent ~transient:(k land 1 = 0) d "child"
        in
        Console.Display.set_bytes child ~cur:3L ~total:5L;
        Console.Display.log child "child output";
        Console.Display.cancel child;
        Console.Display.set_count parent ~cur:1 ~total:1);
    Console.Display.log (Console.Display.root d) "root output";
    Console.Display.set_result ~detail:"detail" d (Ok "done")
  in
  with_dimensions 80 24 (fun () ->
      List.iter
        (fun (_, progress) ->
          let direct =
            capture_progress_formatter ~theme ~use_str (fun ppf ->
                let size =
                  if progress = `Auto || k land 2 = 0 then None
                  else Some (fun () -> (80, 24))
                in
                let d =
                  Console.Display.v ?size ~progress ~header:"header"
                    ~now:(fun () -> 0.)
                    ppf
                in
                program d;
                Console.Display.redraw d;
                Console.Display.close d)
          in
          let driven =
            capture_progress_formatter ~theme ~use_str (fun ppf ->
                Eio_mock.Backend.run (fun () ->
                    Console_eio.run ~clock:(Eio_mock.Clock.Mono.make ())
                      ~ppf ~progress ~header:"header" (fun d ->
                        program d;
                        Console.Display.redraw d)))
          in
          check_eq ~pp:Fmt.Dump.string direct driven)
        Console.Progress.all)

(* Progress.all is directly usable as a command-line enum, and diagnostics
   for any unknown spelling enumerate every available choice. *)
let test_progress_enum text =
  let parse =
    Cmdliner.Arg.conv_parser (Cmdliner.Arg.enum Console.Progress.all)
  in
  List.iter
    (fun (name, expected) ->
      (match parse name with
      | Ok got -> check (got = expected)
      | Error _ -> failf "Cmdliner rejects progress %s" name);
      match Console.Progress.of_string name with
      | Ok got -> check (got = expected)
      | Error _ -> failf "of_string rejects progress %s" name)
    Console.Progress.all;
  let unknown = "invalid:" ^ text in
  check (Result.is_error (parse unknown));
  match Console.Progress.of_string unknown with
  | Ok _ -> fail "an unknown progress spelling was accepted"
  | Error (`Msg message) ->
      List.iter
        (fun (name, _) ->
          let rec contains i =
            i + String.length name <= String.length message
            && (String.sub message i (String.length name) = name
               || contains (i + 1))
          in
          if not (contains 0) then failf "diagnostic omitted progress %s" name)
        Console.Progress.all

let suite =
  ( "console_eio",
    [
      test_case
        "progress names are a Cmdliner enum and errors list every choice"
        [ bytes ] test_progress_enum;
      test_case
        "direct progress modes agree with run on buffer and string formatters"
        [ uint8; bytes; bool ] test_direct_progress_parity;
      test_case "JSON preserves long lines across formatter chunks"
        [ uint8; uint8 ] test_json_long_line;
      test_case "a line goes to the task of its line feed" [ uint8; bool ]
        test_json_line_attribution;
      test_case "quiet prints an unended line at exit" [ uint8; bool ]
        test_quiet_unended_line;
      test_case "concurrent JSON records stay whole and complete"
        [ uint8; uint8 ] test_json_concurrent_records;
      test_case "quiet captures every terminal output source" [ uint8 ]
        test_quiet_terminal_records;
      test_case "JSON lifecycles keep counts parents levels and endings"
        [ uint8 ] test_json_lifecycle;
      test_case "forced tty keeps screen guarantees without a terminal"
        [ uint8; bytes ] test_forced_tty_screen;
      test_case "JSON events read back" [ bytes; bytes ] test_json_events;
      test_case "a surviving fiber uses a live display" [ uint8 ]
        test_surviving_fiber;
      test_case "a refused record falls back and completes once"
        [ bytes; uint8 ] test_refused_record;
      test_case "a sibling keeps the display after the first callback ends"
        [ uint8; bool ] test_sibling_lifetime;
    ] )
