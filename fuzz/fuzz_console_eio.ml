(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar

(* A logging transport can refuse any record. The caller still completes,
   with the record handed once to the reporter installed before the run. *)
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
  let refused = ref false in
  let ppf =
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
  in
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
  check_eq ~pp:Fmt.(Dump.list Dump.string) [ text ] (List.rev !kept);
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

let suite =
  ( "console_eio",
    [
      test_case "a refused record falls back and completes once"
        [ bytes; uint8 ] test_refused_record;
      test_case "a sibling keeps the display after the first callback ends"
        [ uint8; bool ] test_sibling_lifetime;
    ] )
