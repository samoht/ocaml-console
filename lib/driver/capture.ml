(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Display = Console.Display
module Task = Console.Task

type run = { display : Display.t; root : Task.t; json : bool }

(* An unended line, and whether the line before it was cut rather than ended. *)
type line = { pending : Buffer.t; mutable cut : bool }

let line () = { pending = Buffer.create 256; cut = false }

type taken = {
  by : run;
  ppf : Format.formatter;
  direct : unit -> bool;
  mutable out : Format.formatter_out_functions;
  mutable pass : Format.formatter_out_functions;
      (* [out], as the display watches it *)
  held : Buffer.t; (* whole lines to write above the region *)
  root : line; (* the unended line outside any task *)
}

let taken : taken list Atomic.t = Atomic.make []
let lines : (int, line) Hashtbl.t = Hashtbl.create 16

(* The maximum size of an unended line: the size of a channel's buffer. A
   longer line goes out cut, between two characters and outside any escape
   code. *)
let max_pending = 65536

let safely f =
  match f () with
  | () -> ()
  | exception e when not (Task_driver.is_async e) -> ()

let take_from b k =
  let all = Buffer.contents b in
  let k = k all in
  Buffer.clear b;
  Buffer.add_substring b all k (String.length all - k);
  String.sub all 0 k

(* [add_to l s o len emit] adds the bytes to [l]. It then gives [emit] the
   lines that are now complete or, if the line is longer than [max_pending],
   the piece cut from it. *)
let add_to l s o len emit =
  Buffer.add_substring l.pending s o len;
  let ends_a_line =
    match String.index_from_opt s o '\n' with
    | Some k -> k < o + len
    | None -> false
  in
  if ends_a_line then
    emit ~cut:false
      (take_from l.pending (fun all -> String.rindex all '\n' + 1))
  else if Buffer.length l.pending > max_pending then
    emit ~cut:true (take_from l.pending Console_text.boundary)

(* The line feed that ends a cut line does not make an empty line. *)
let give l ~cut task s =
  let s =
    if l.cut && String.starts_with ~prefix:"\n" s then
      String.sub s 1 (String.length s - 1)
    else s
  in
  l.cut <- cut;
  if s <> "" then Task.log task (Console_text.strip s)

let send_held display i =
  if Buffer.length i.held > 0 then begin
    let s = Buffer.contents i.held in
    Buffer.clear i.held;
    Display.with_output display (fun _ ->
        i.pass.out_string s 0 (String.length s);
        i.pass.out_flush ())
  end

let outside (run : run) i ~cut s =
  if run.json then give i.root ~cut run.root s
  else begin
    i.root.cut <- false;
    Buffer.add_string i.held s;
    if cut || Buffer.length i.held > max_pending then send_held run.display i
  end

let task_line task =
  let id = Task_driver.id task in
  match Hashtbl.find_opt lines id with
  | Some l -> l
  | None ->
      let l = line () in
      Hashtbl.replace lines id l;
      l

let take_over ({ by = run; _ } as i) =
  let add s o len =
    if i.direct () then i.pass.out_string s o len
    else
      match Task_driver.current run.root with
      | Some task ->
          let l = task_line task in
          add_to l s o len (fun ~cut piece ->
              send_held run.display i;
              give l ~cut task piece)
      | None -> add_to i.root s o len (outside run i)
  in
  let flush () =
    if i.direct () then i.pass.out_flush () else send_held run.display i
  in
  Format.pp_set_formatter_out_functions i.ppf
    {
      i.out with
      out_string = add;
      out_flush = flush;
      out_newline = (fun () -> add "\n" 0 1);
      out_spaces = (fun n -> add (String.make (max 0 n) ' ') 0 (max 0 n));
      out_indent = (fun n -> add (String.make (max 0 n) ' ') 0 (max 0 n));
    }

let take ~direct run ppf =
  Format.pp_print_flush ppf ();
  let out = Format.pp_get_formatter_out_functions ppf () in
  let pass = Display_driver.watch run.display out in
  let i =
    {
      by = run;
      ppf;
      direct;
      out;
      pass;
      held = Buffer.create 256;
      root = line ();
    }
  in
  take_over i;
  let rec remember () =
    let l = Atomic.get taken in
    if not (Atomic.compare_and_set taken l (i :: l)) then remember ()
  in
  remember ()

(* Before the first domain starts, Format replaces the [out_string] and
   [out_flush] of [Fmt.stdout] and [Fmt.stderr] with a buffer per domain. This
   hook runs after that and captures these formatters again over the new
   functions. *)

let () =
  let rebase i =
    if i.ppf == Format.std_formatter || i.ppf == Format.err_formatter then begin
      let now = Format.pp_get_formatter_out_functions i.ppf () in
      i.out <-
        { i.out with out_string = now.out_string; out_flush = now.out_flush };
      i.pass <- Display_driver.watch i.by.display i.out;
      take_over i
    end
  in
  try Domain.before_first_spawn (fun () -> List.iter rebase (Atomic.get taken))
  with Invalid_argument _ -> ()

let taking () = Atomic.get taken <> []

let flush_all () =
  List.iter
    (fun i -> safely (fun () -> Format.pp_print_flush i.ppf ()))
    (Atomic.get taken)

let end_line task =
  let id = Task_driver.id task in
  match Hashtbl.find_opt lines id with
  | None -> ()
  | Some l ->
      Hashtbl.remove lines id;
      let s = Buffer.contents l.pending in
      if s <> "" then give l ~cut:false task s

let settle (run : run) =
  if run.json then begin
    flush_all ();
    List.iter
      (fun i ->
        let s = Buffer.contents i.root.pending in
        Buffer.clear i.root.pending;
        if s <> "" then outside run i ~cut:false s)
      (Atomic.get taken)
  end

let restore ?(flush = true) () =
  List.iter
    (fun i ->
      if flush then safely (fun () -> Format.pp_print_flush i.ppf ());
      Format.pp_set_formatter_out_functions i.ppf i.out;
      let rest = Buffer.contents i.held ^ Buffer.contents i.root.pending in
      if flush && rest <> "" then
        safely (fun () ->
            i.out.out_string rest 0 (String.length rest);
            i.out.out_flush ()))
    (Atomic.exchange taken []);
  if flush then Hashtbl.reset lines
