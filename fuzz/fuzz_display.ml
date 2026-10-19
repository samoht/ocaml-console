(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A display drives an emulated terminal. Each write it makes is fed to the
   emulator as one chunk, and the screen is read after every chunk: the
   guarantees of [display.mli] are checked on what a reader would see. *)

open Alcobar
open Console

let labels =
  [|
    "fetch";
    "compile a rather long label that will not fit a narrow terminal";
    "\xe7\x95\x8c\xe7\x95\x8c wide";
    "\xf0\x9f\x91\xa8\xe2\x80\x8d\xf0\x9f\x91\xa9 family";
    "e\xcc\x81 accent";
    "\027[2J escape";
    "tab\there";
  |]

type op =
  | Task of int * int option
  | Count of int * int
  | Bytes of int * int
  | Log of int * int
  | End of int * int
  | Message of int
  | Advance of int
  | Resize of int * int
  | Redraw
  | Output of int

let op =
  let open Alcobar in
  choose
    [
      map [ uint8; option uint8 ] (fun l p -> Task (l, p));
      map [ uint8; uint8 ] (fun i n -> Count (i, n));
      map [ uint8; uint8 ] (fun i n -> Bytes (i, n));
      map [ uint8; uint8 ] (fun i l -> Log (i, l));
      map [ uint8; uint8 ] (fun i o -> End (i, o));
      map [ uint8 ] (fun l -> Message l);
      map [ uint8 ] (fun d -> Advance d);
      map [ uint8; uint8 ] (fun w h -> Resize (w, h));
      const Redraw;
      map [ uint8 ] (fun l -> Output l);
    ]

let pp_op ppf = function
  | Task (l, p) -> Fmt.pf ppf "task %d %a" l Fmt.(option int) p
  | Count (i, n) -> Fmt.pf ppf "count %d %d" i n
  | Bytes (i, n) -> Fmt.pf ppf "bytes %d %d" i n
  | Log (i, l) -> Fmt.pf ppf "log %d %d" i l
  | End (i, o) -> Fmt.pf ppf "end %d %d" i o
  | Message l -> Fmt.pf ppf "message %d" l
  | Advance d -> Fmt.pf ppf "advance %d" d
  | Resize (w, h) -> Fmt.pf ppf "resize %d %d" w h
  | Redraw -> Fmt.string ppf "redraw"
  | Output l -> Fmt.pf ppf "output %d" l

let op = with_printer pp_op op
let header = "HEADER"

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

let screen vte = List.map rstrip (String.split_on_char '\n' (Vte.to_string vte))
let count p l = List.length (List.filter p l)
let has affix s = Re.execp (Re.compile (Re.str affix)) s

(* Each line a chunk writes, its escapes removed, fits the terminal. *)
let check_chunk_lines ~width chunk =
  List.iter
    (fun l ->
      (* A carriage return moves to the first column; it draws nothing. *)
      let l = String.concat "" (String.split_on_char '\r' l) in
      let l = Console_text.strip l in
      if Width.string_width l > width then
        failf "a line of %d cells on %d columns: %S" (Width.string_width l)
          width l)
    (String.split_on_char '\n' chunk)

(* Every style turned on in a chunk is off again at the end of each line. *)
let check_styles_closed chunk =
  List.iter
    (fun l ->
      let rec last_sgr i acc =
        match String.index_from_opt l i '\027' with
        | None -> acc
        | Some j when j + 1 < String.length l && l.[j + 1] = '[' -> (
            match String.index_from_opt l j 'm' with
            | Some k
              when String.for_all
                     (fun c -> (c >= '0' && c <= '9') || c = ';')
                     (String.sub l (j + 2) (k - j - 2)) ->
                last_sgr (k + 1) (Some (String.sub l j (k - j + 1)))
            | _ -> last_sgr (j + 1) acc)
        | Some j -> last_sgr (j + 1) acc
      in
      match last_sgr 0 None with
      | None | Some "\027[0m" -> ()
      | Some s -> failf "%S left on in %S" s l)
    (String.split_on_char '\n' chunk)

let test ops w h styled =
  let width = 8 + (w mod 72) and height = 3 + (h mod 20) in
  let size = ref (width, height) in
  let vte = Vte.create ~rows:height ~cols:width () in
  let chunk = Buffer.create 256 in
  let chunks = ref 0 and in_output = ref false in
  let check_screen ~in_output =
    let rows = screen vte in
    let width, height = !size in
    (* The header is on the screen exactly once: the region is never blank,
       half drawn or drawn twice. *)
    if not in_output then begin
      let n = count (has header) rows in
      if n <> 1 && height > 1 then
        failf "the header is on the screen %d times:@.%a" n
          Fmt.(list ~sep:cut string)
          rows
    end;
    ignore width
  in
  let out_flush () =
    let s = Buffer.contents chunk in
    Buffer.clear chunk;
    if s <> "" then begin
      incr chunks;
      let width, _ = !size in
      (* What the program writes under [with_output] is its own. *)
      if not !in_output then check_chunk_lines ~width s;
      if styled then check_styles_closed s;
      Vte.feed_string vte s
    end
  in
  let ppf = Format.make_formatter (Buffer.add_substring chunk) out_flush in
  if styled then Fmt.set_style_renderer ppf `Ansi_tty;
  let time = ref 0. in
  let d =
    Display.v ~header ~size:(fun () -> !size) ~now:(fun () -> !time) ppf
  in
  let tasks = ref [||] in
  let task i =
    let n = Array.length !tasks in
    if n = 0 then None else Some !tasks.(i mod n)
  in
  let label i = labels.(i mod Array.length labels) in
  let apply = function
    | Task (l, p) ->
        let parent = Option.bind p task in
        let t = Display.task ?parent ~log_lines:(l mod 4) d (label l) in
        tasks := Array.append !tasks [| t |]
    | Count (i, n) ->
        Option.iter
          (fun t ->
            try Display.set_count t ~cur:(n / 2) ~total:n
            with Invalid_argument _ -> ())
          (task i)
    | Bytes (i, n) ->
        Option.iter
          (fun t ->
            try
              Display.set_bytes t ~cur:(Int64.of_int (n * 1000)) ~total:256000L
            with Invalid_argument _ -> ())
          (task i)
    | Log (i, l) -> Option.iter (fun t -> Display.log t (label l)) (task i)
    | End (i, o) ->
        Option.iter
          (fun t ->
            match o mod 3 with
            | 0 -> Display.succeed t
            | 1 -> Display.fail t
            | _ -> Display.cancel t)
          (task i)
    | Message l -> Display.message d (label l)
    | Advance n -> time := !time +. (Float.of_int n /. 100.)
    | Resize (w, h) ->
        let w = 8 + (w mod 72) and h = 3 + (h mod 20) in
        size := (w, h);
        Vte.resize vte ~rows:h ~cols:w
    | Redraw ->
        Display.redraw d;
        (* A second redraw at the same time changes nothing and writes
           nothing. *)
        let before = !chunks in
        Display.redraw d;
        if !chunks <> before then fail "an idle redraw wrote"
    | Output l ->
        Display.with_output d (fun ppf ->
            in_output := true;
            Fmt.pf ppf "%s@." (label l);
            Format.pp_print_flush ppf ();
            check_screen ~in_output:true;
            in_output := false)
  in
  List.iter
    (fun o ->
      apply o;
      Display.redraw d;
      check_screen ~in_output:false)
    ops;
  Display.close d;
  Format.pp_print_flush ppf ();
  if not (Vte.cursor_visible vte) then fail "the cursor is hidden after close";
  if snd (Vte.cursor_pos vte) <> 0 then
    fail "the cursor is not at the start of a line"

(* Without a terminal, nothing moves the cursor: every line is printed once,
   and the header comes last. *)
let test_plain ops =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let d = Display.v ~header ~now:(fun () -> 0.) ppf in
  let tasks = ref [] in
  List.iter
    (function
      | Task (l, _) ->
          tasks := Display.task d labels.(l mod Array.length labels) :: !tasks
      | Log (_, l) -> List.iter (fun t -> Display.log t labels.(l mod 7)) !tasks
      | End _ -> List.iter Display.succeed !tasks
      | _ -> ())
    ops;
  Display.close d;
  Format.pp_print_flush ppf ();
  let out = Buffer.contents buf in
  if has "\027" out then failf "an escape without a terminal: %S" out;
  let lines = List.filter (( <> ) "") (String.split_on_char '\n' out) in
  match List.rev lines with
  | last :: _ ->
      if not (has header last) then failf "the header is not last: %S" last
  | [] -> fail "no header"

let suite =
  ( "display",
    [
      test_case "screen guarantees" [ list op; uint8; uint8; bool ] test;
      test_case "plain output" [ list op ] test_plain;
    ] )
