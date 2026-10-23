(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type output = {
  ppf : Format.formatter;
  out : Format.formatter_out_functions;
  pending : Buffer.t;
}

type t = {
  file : string;
  oc : out_channel;
  mutable outputs : output list;
  mutable time : unit -> float;
  mutable last : float;
}

let current = ref None
let size () = Terminal.dimensions None

(* The time never goes back, though the clock of a later run starts again
   at zero. *)
let emit t data =
  let time = Float.max t.last (t.time ()) in
  t.last <- time;
  output_string t.oc (Cast.to_line Cast.event { time; data });
  flush t.oc

(* Each flush is a write, cut between two characters: the bytes of one cut
   short wait for those that end it. *)
let written t o =
  match Session.take_from o.pending Console_text.boundary with
  | "" -> ()
  | data -> emit t data

let record t ppf =
  if not (List.exists (fun o -> o.ppf == ppf) t.outputs) then begin
    Format.pp_print_flush ppf ();
    let o =
      {
        ppf;
        out = Format.pp_get_formatter_out_functions ppf ();
        pending = Buffer.create 256;
      }
    in
    let blank n =
      if n > 0 then Buffer.add_string o.pending (String.make n ' ')
    in
    Format.pp_set_formatter_out_functions ppf
      {
        o.out with
        out_string = Buffer.add_substring o.pending;
        out_flush = (fun () -> written t o);
        out_newline = (fun () -> Buffer.add_char o.pending '\n');
        out_spaces = blank;
        out_indent = blank;
      };
    t.outputs <- o :: t.outputs
  end

let stop t =
  List.iter
    (fun o ->
      Format.pp_print_flush o.ppf ();
      if Buffer.length o.pending > 0 then emit t (Buffer.contents o.pending);
      Format.pp_set_formatter_out_functions o.ppf o.out)
    t.outputs;
  close_out t.oc

let open_cast file =
  let oc = open_out_bin file in
  let width, height = size () in
  let term = match Sys.getenv_opt "TERM" with Some "" -> None | t -> t in
  output_string oc (Cast.to_line Cast.header { width; height; term });
  flush oc;
  { file; oc; outputs = []; time = (fun () -> 0.); last = 0. }

let start ~time ppf =
  let file = Cast.file () in
  (match !current with
  | Some t when Some t.file = file -> ()
  | last ->
      Option.iter stop last;
      current := Option.map open_cast file);
  match !current with
  | None -> false
  | Some t ->
      t.time <- time;
      let std = ppf == Fmt.stdout || ppf == Fmt.stderr in
      List.iter (record t) (if std then [ Fmt.stdout; Fmt.stderr ] else [ ppf ]);
      true
