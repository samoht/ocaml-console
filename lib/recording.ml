(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type output = {
  ppf : Format.formatter;
  mutable out : Format.formatter_out_functions;
  pending : Buffer.t;
}

type t = {
  file : string;
  oc : out_channel;
  mutable outputs : output list;
  mutable time : unit -> float;
  mutable last : float;
  mutable cr : bool;
}

let current = ref None

let decimal s =
  if s <> "" && String.for_all Char.Ascii.is_digit s then int_of_string_opt s
  else None

(* A display takes at most 4096 columns or rows, so a larger size is not a
   terminal's. *)
let dimension name ~default =
  match Option.bind (Sys.getenv_opt name) decimal with
  | Some n when n > 0 && n <= 4096 -> n
  | _ -> default

let size () = (dimension "COLUMNS" ~default:80, dimension "LINES" ~default:24)

(* [shown t data] is [data] as the terminal receives it: the tty driver puts
   a carriage return before each line feed that lacks one. *)
let shown t data =
  let b = Buffer.create (String.length data + 16) in
  String.iter
    (fun c ->
      if c = '\n' && not t.cr then Buffer.add_char b '\r';
      Buffer.add_char b c;
      t.cr <- c = '\r')
    data;
  Buffer.contents b

(* The time written never goes back, even when the clock of a later run
   starts again at zero. *)
let emit t data =
  let time = Float.max t.last (t.time ()) in
  t.last <- time;
  let data = shown t data in
  output_string t.oc (Cast.to_line Cast.event { time; data });
  flush t.oc

(* Each flush writes one event, cut at a character boundary: the bytes of a
   character cut short wait in [pending] for the rest of it. *)
let written t o =
  let all = Buffer.contents o.pending in
  match Console_text.boundary all with
  | 0 -> ()
  | k ->
      Buffer.clear o.pending;
      Buffer.add_substring o.pending all k (String.length all - k);
      emit t (String.sub all 0 k)

let redirect t o =
  let blank n = if n > 0 then Buffer.add_string o.pending (String.make n ' ') in
  Format.pp_set_formatter_out_functions o.ppf
    {
      o.out with
      out_string = Buffer.add_substring o.pending;
      out_flush = (fun () -> written t o);
      out_newline = (fun () -> Buffer.add_char o.pending '\n');
      out_spaces = blank;
      out_indent = blank;
    }

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
    redirect t o;
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

(* Before the first domain starts, Format gives [Fmt.stdout] and [Fmt.stderr]
   the [out_string] and [out_flush] of a buffer per domain. This runs after
   that, so a recorded formatter is recorded again over the new pair. *)
let () =
  let rebase t o =
    if o.ppf == Format.std_formatter || o.ppf == Format.err_formatter then begin
      let now = Format.pp_get_formatter_out_functions o.ppf () in
      o.out <-
        { o.out with out_string = now.out_string; out_flush = now.out_flush };
      redirect t o
    end
  in
  try
    Domain.before_first_spawn (fun () ->
        Option.iter (fun t -> List.iter (rebase t) t.outputs) !current)
  with Invalid_argument _ -> ()

(* A recording that cannot be opened ends the program, as a bad option
   would. *)
let open_cast file =
  let oc =
    try open_out_bin file
    with Sys_error e ->
      Fmt.epr "%s: CONSOLE_CAST: cannot record: %s@."
        (Filename.basename Sys.executable_name)
        e;
      exit 1
  in
  let width, height = size () in
  let term = match Sys.getenv_opt "TERM" with Some "" -> None | t -> t in
  output_string oc (Cast.to_line Cast.header { width; height; term });
  flush oc;
  { file; oc; outputs = []; time = (fun () -> 0.); last = 0.; cr = false }

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
