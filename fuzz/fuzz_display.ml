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

(* Every line feed the display writes on a terminal follows a carriage
   return, so a tty in raw mode starts each line at the first column too. *)
let check_line_ends chunk =
  String.iteri
    (fun i c ->
      if c = '\n' && (i = 0 || chunk.[i - 1] <> '\r') then
        failf "a bare line feed at byte %d of %S" i chunk)
    chunk

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

(* A display on an emulated terminal, and what the test knows of it. *)
type term = {
  vte : Vte.t;
  size : (int * int) ref;
  time : float ref;
  chunks : int ref;
  in_output : bool ref;
  mutable display : Display.t option;
  mutable tasks : Display.task array;
}

(* The header is on the screen exactly once: the region is never blank, half
   drawn or drawn twice. *)
let check_screen t =
  let rows = screen t.vte in
  let n = count (has header) rows in
  if n <> 1 then
    failf "the header is on the screen %d times:@.%a" n
      Fmt.(list ~sep:cut string)
      rows

let terminal ~width ~height ~styled =
  let t =
    {
      vte = Vte.create ~rows:height ~cols:width ();
      size = ref (width, height);
      time = ref 0.;
      chunks = ref 0;
      in_output = ref false;
      display = None;
      tasks = [||];
    }
  in
  let chunk = Buffer.create 256 in
  let out_flush () =
    let s = Buffer.contents chunk in
    Buffer.clear chunk;
    if s <> "" then begin
      incr t.chunks;
      (* What the program writes under [with_output] is its own. *)
      if not !(t.in_output) then begin
        check_chunk_lines ~width:(fst !(t.size)) s;
        check_line_ends s
      end;
      if styled then check_styles_closed s;
      Vte.feed_string t.vte s
    end
  in
  let ppf = Format.make_formatter (Buffer.add_substring chunk) out_flush in
  if styled then Fmt.set_style_renderer ppf `Ansi_tty;
  t.display <-
    Some
      (Display.v ~header
         ~size:(fun () -> !(t.size))
         ~now:(fun () -> !(t.time))
         ppf);
  t

let display t = Option.get t.display
let label i = labels.(i mod Array.length labels)

let task t i =
  let n = Array.length t.tasks in
  if n = 0 then None else Some t.tasks.(i mod n)

let progress t i f =
  Option.iter
    (fun task -> try f task with Invalid_argument _ -> ())
    (task t i)

let finish task o =
  match o mod 3 with
  | 0 -> Display.succeed task
  | 1 -> Display.fail task
  | _ -> Display.cancel task

(* A redraw changes only the region: the rows from the cursor's up to the
   height of the screen less one. A second redraw at the same time changes
   nothing and writes nothing. *)
let redraw_twice t =
  let before = screen t.vte in
  Display.redraw (display t);
  let after = screen t.vte in
  let _, height = !(t.size) in
  let top = fst (Vte.cursor_pos t.vte) - (height - 2) in
  if Vte.scrollback_size t.vte = 0 || List.length before = List.length after
  then
    List.iteri
      (fun i (b, a) ->
        if i < top && not (String.equal a b) then
          failf "row %d above the region changed: %S -> %S" i b a)
      (List.combine before after);
  let before = !(t.chunks) in
  Display.redraw (display t);
  if !(t.chunks) <> before then fail "an idle redraw wrote"

let output t l =
  Display.with_output (display t) (fun ppf ->
      t.in_output := true;
      Fmt.pf ppf "%s@." (label l);
      t.in_output := false)

let apply t = function
  | Task (l, p) ->
      let parent = Option.bind p (task t) in
      let task =
        Display.task ?parent ~log_lines:(l mod 4) (display t) (label l)
      in
      t.tasks <- Array.append t.tasks [| task |]
  | Count (i, n) ->
      progress t i (fun task -> Display.set_count task ~cur:(n / 2) ~total:n)
  | Bytes (i, n) ->
      progress t i (fun task ->
          Display.set_bytes task ~cur:(Int64.of_int (n * 1000)) ~total:256000L)
  | Log (i, l) ->
      Option.iter (fun task -> Display.log task (label l)) (task t i)
  | End (i, o) -> Option.iter (fun task -> finish task o) (task t i)
  | Message l -> Display.message (display t) (label l)
  | Advance n -> t.time := !(t.time) +. (Float.of_int n /. 100.)
  | Resize (w, h) ->
      let w = 8 + (w mod 72) and h = 3 + (h mod 20) in
      t.size := (w, h);
      Vte.resize t.vte ~rows:h ~cols:w
  | Redraw -> redraw_twice t
  | Output l -> output t l

let test ops w h styled =
  let t = terminal ~width:(8 + (w mod 72)) ~height:(3 + (h mod 20)) ~styled in
  List.iter
    (fun o ->
      apply t o;
      Display.redraw (display t);
      check_screen t)
    ops;
  Display.close (display t);
  if not (Vte.cursor_visible t.vte) then fail "the cursor is hidden after close";
  if snd (Vte.cursor_pos t.vte) <> 0 then
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

(* The sink and the caller retain neither completed tasks nor old log text.
   After warmup, live heap measures retention rather than allocation volume. *)
let test_retained_heap n =
  let ppf = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ()) in
  let d = Display.v ~size:(fun () -> (80, 10)) ~now:(fun () -> 0.) ppf in
  let task = Display.task ~log_lines:2 d "long running" in
  let batch () =
    for i = 1 to 1000 + n do
      Display.succeed (Display.task ~transient:true d "short lived");
      Display.log ~keep:false task (string_of_int i ^ String.make 512 'x')
    done;
    Display.redraw d;
    Gc.full_major ();
    (Gc.stat ()).live_words
  in
  Fun.protect
    (fun () ->
      let warm = batch () in
      for _ = 1 to 3 do
        let retained = batch () in
        if retained > warm + 4096 then
          failf "retained heap grew from %d to %d words" warm retained
      done)
    ~finally:(fun () -> Display.close d)

(* Each call has a unique token, so duplicates, reorderings and misattribution
   are detectable even when several tasks have the same display label. *)
let test_pipe_attribution n =
  let b = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer b in
  let d = Display.v ~header:"SUMMARY" ~now:(fun () -> 0.) ppf in
  let tasks =
    Array.init 4 (fun i -> Display.task d ("task" ^ string_of_int i))
  in
  let expected =
    List.init (1 + n) (fun i ->
        let task = i mod 4 in
        let token = "record_" ^ string_of_int i ^ "_end" in
        Display.log tasks.(task) (token ^ "\n" ^ token ^ "_second");
        Display.log ~keep:false tasks.(task) "PRIVATE";
        [
          "task" ^ string_of_int task ^ ": " ^ token;
          "task" ^ string_of_int task ^ ": " ^ token ^ "_second";
        ])
    |> List.concat
  in
  Array.iter Display.succeed tasks;
  Display.close d;
  Format.pp_print_flush ppf ();
  let out = Buffer.contents b in
  check (String.is_valid_utf_8 out);
  check (not (String.contains out '\027'));
  check (not (has "PRIVATE" out));
  let records =
    String.split_on_char '\n' out
    |> List.filter (has ": record_")
    |> List.map (fun line ->
        match String.index_opt line 't' with
        | Some i -> String.sub line i (String.length line - i)
        | None -> fail "missing task label")
  in
  check_eq ~pp:Fmt.(Dump.list string) expected records;
  let nonempty = List.filter (( <> ) "") (String.split_on_char '\n' out) in
  check (has "SUMMARY" (List.hd (List.rev nonempty)))

let test_output_exception s cancel =
  let t = terminal ~width:40 ~height:8 ~styled:true in
  let d = display t in
  ignore (Display.task d "interrupted");
  Display.redraw d;
  let partial = Console_text.sanitize ~keep_newlines:false s in
  let partial = Span.to_string (Span.truncate 30 (Span.text partial)) in
  (match
     Display.with_output d (fun ppf ->
         t.in_output := true;
         Fun.protect
           (fun () ->
             Fmt.pf ppf "%s" partial;
             raise Exit)
           ~finally:(fun () -> t.in_output := false))
   with
  | _ -> fail "callback exception swallowed"
  | exception Exit -> ());
  Display.redraw d;
  check_screen t;
  Display.close ~running:(if cancel then `Cancel else `Succeed) d;
  check (Vte.cursor_visible t.vte);
  check_eq ~pp:Fmt.int 0 (snd (Vte.cursor_pos t.vte));
  let y, _ = Vte.cursor_pos t.vte in
  Vte.feed_string t.vte "AFTER";
  check (has "AFTER" (List.nth (screen t.vte) y));
  let grid = Vte.grid t.vte in
  let index = Grid.idx grid ~x:0 ~y in
  let reference = Vte.create ~rows:8 ~cols:40 () in
  Vte.feed_string reference "AFTER";
  let plain = Vte.grid reference in
  let first = Grid.idx plain ~x:0 ~y:0 in
  check (Grid.get_fg_r grid index = Grid.get_fg_r plain first);
  check (Grid.get_fg_g grid index = Grid.get_fg_g plain first);
  check (Grid.get_fg_b grid index = Grid.get_fg_b plain first);
  check (Grid.get_bg_r grid index = Grid.get_bg_r plain first)

let test_resize_erases_old_frame n =
  let t = terminal ~width:80 ~height:12 ~styled:true in
  let d = display t in
  let a = Display.task d ("OLD_SENTINEL_" ^ String.make 45 'x') in
  let b = Display.task d ("OLD_SENTINEL_" ^ String.make 45 'y') in
  Display.redraw d;
  let w = 12 + (n mod 40) in
  t.size := (w, 8);
  Vte.resize t.vte ~rows:8 ~cols:w;
  Display.set_label a "new-a";
  Display.set_label b "new-b";
  Display.redraw d;
  check_screen t;
  let rows =
    String.concat "\n" (Vte.scrollback_lines t.vte @ [ Vte.to_string t.vte ])
  in
  if has "OLD_SENTINEL" rows then failf "old frame survived shrink: %S" rows;
  t.size := (80, 12);
  Vte.resize t.vte ~rows:12 ~cols:80;
  Display.redraw d;
  let rows =
    String.concat "\n" (Vte.scrollback_lines t.vte @ [ Vte.to_string t.vte ])
  in
  if has "OLD_SENTINEL" rows then
    failf "old frame returned after growth: %S" rows;
  check (has "new-a" rows && has "new-b" rows);
  redraw_twice t;
  Display.close d

let test_static_rows_and_idle n =
  let t = terminal ~width:60 ~height:10 ~styled:true in
  let d = display t in
  let task = Display.task d "moving" in
  Display.with_output d (fun ppf ->
      t.in_output := true;
      Fmt.pf ppf "STATIC_SENTINEL@.";
      t.in_output := false);
  let before = screen t.vte in
  let rec prefix = function
    | [] -> fail "permanent output missing"
    | l :: ls -> if has "STATIC_SENTINEL" l then [ l ] else l :: prefix ls
  in
  let permanent = prefix before in
  for i = 0 to n do
    t.time := Float.of_int i /. 5.;
    Display.set_count task ~cur:i ~total:256;
    redraw_twice t;
    check_eq ~pp:Fmt.(Dump.list string) permanent (prefix (screen t.vte))
  done;
  Display.close d

(* Compare every visible frame: CRLF handling may remove trailing padding
   without changing what the terminal shows. Keep cursor state in the oracle. *)
let capture_frames () =
  let out = Buffer.create 512 and chunk = Buffer.create 512 in
  let vte = Vte.create ~rows:8 ~cols:40 () in
  Vte.feed_string vte "OUTSIDE_SENTINEL\r\n";
  let frames = ref [] in
  let write bytes start len =
    Buffer.add_substring out bytes start len;
    Buffer.add_substring chunk bytes start len
  in
  let flush () =
    let bytes = Buffer.contents chunk in
    Buffer.clear chunk;
    if bytes <> "" then begin
      Vte.feed_string vte bytes;
      let rows = List.map rstrip (Vte.scrollback_lines vte @ screen vte) in
      frames := (rows, Vte.cursor_pos vte, Vte.cursor_visible vte) :: !frames
    end
  in
  (Format.make_formatter write flush, out, frames)

let sanitized_log_lines input =
  String.split_on_char '\n' input
  |> List.map (fun line ->
      let line =
        if String.ends_with ~suffix:"\r" line then
          String.sub line 0 (String.length line - 1)
        else line
      in
      Console_text.sanitize line)
  |> String.concat "\n"

let test_untrusted_arguments s =
  let render input lines =
    let ppf, out, frames = capture_frames () in
    Fmt.set_style_renderer ppf `Ansi_tty;
    let theme =
      Theme.v ~marks:(fun _ -> input) ~spinner:(Spinner.v [| input |]) ()
    in
    let layout =
      Display.Line.(v [ text input; accent input; label; spacer; elapsed ])
    in
    let d =
      Display.v ~theme ~layout ~header:input
        ~size:(fun () -> (40, 8))
        ~now:(fun () -> 0.)
        ppf
    in
    let task = Display.task d input in
    Display.set_label task input;
    Display.log task lines;
    Display.message ~level:`Warning d lines;
    Display.redraw d;
    Display.succeed ~message:input task;
    Display.set_result ~detail:lines d (`Ok input);
    Display.close d;
    Format.pp_print_flush ppf ();
    (Buffer.contents out, List.rev !frames)
  in
  List.iter
    (fun input ->
      let raw, actual = render input input in
      let _, expected =
        render (Console_text.sanitize input) (sanitized_log_lines input)
      in
      check (String.is_valid_utf_8 raw && not (has "\027]" raw));
      check_styles_closed raw;
      if actual <> expected then failf "unsafe visible frame from %S" input)
    [
      s;
      String.make 18 'x' ^ "\r";
      "s\002\246\147/\194\r";
      "\027]52;c;Y2xpcGJvYXJk\007";
      "\027]8;;https://evil.invalid\027\\click";
      "\027[2J\027[?25l";
      "\u{009b}2J\255\u{202e}spoof";
    ]

let test_extreme_terminals n styled =
  List.iter
    (fun (width, height) ->
      let t = terminal ~width ~height ~styled in
      let d = display t in
      for i = 0 to 20 + (n mod 20) do
        let task =
          Display.task ~log_lines:1 d
            (string_of_int i ^ " \u{754c}\u{1f469}\u{200d}\u{1f4bb}")
        in
        Display.log ~keep:false task "\u{754c}\u{1f1eb}\u{1f1f7} tail";
        Display.set_count task ~cur:i ~total:100
      done;
      redraw_twice t;
      (* Nothing has completed or logged a permanent line. A region that fits
       leaves its bottom row free and cannot have produced scrollback. *)
      check_eq ~pp:Fmt.int 0 (Vte.scrollback_size t.vte);
      Display.close d;
      check (Vte.cursor_visible t.vte);
      check_eq ~pp:Fmt.int 0 (snd (Vte.cursor_pos t.vte)))
    [ (1, 1); (2, 80); (300, 10) ]

let suite =
  ( "display",
    [
      test_case "extreme terminal sizes keep region width and height bounded"
        [ uint8; bool ] test_extreme_terminals;
      test_case "untrusted display arguments behave like sanitized text"
        [ bytes ] test_untrusted_arguments;
      test_case "only live rows change and idle frames write nothing" [ uint8 ]
        test_static_rows_and_idle;
      test_case "resize erases every old live row" [ uint8 ]
        test_resize_erases_old_frame;
      test_case "callback exceptions restore a usable terminal" [ bytes; bool ]
        test_output_exception;
      test_case "pipe logs retain order and task attribution" [ uint8 ]
        test_pipe_attribution;
      test_case "long runs retain bounded heap" [ uint8 ] test_retained_heap;
      test_case "screen guarantees" [ list op; uint8; uint8; bool ] test;
      test_case "plain output" [ list op ] test_plain;
    ] )
