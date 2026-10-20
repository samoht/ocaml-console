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
    with ex -> Some ex
  in
  check (Logs.reporter () == previous);
  (match failure with
  | None -> ()
  | Some ex -> failf "a Logs call raised %s" (Printexc.to_string ex));
  check_eq ~pp:Fmt.(Dump.list Dump.string) [ text ] (List.rev !kept);
  check_eq ~pp:Fmt.int 1 !over_count;
  check_eq ~pp:Fmt.int 1 !continued

let suite =
  ( "console_eio",
    [
      test_case "a refused record falls back and completes once"
        [ bytes; uint8 ] test_refused_record;
    ] )
