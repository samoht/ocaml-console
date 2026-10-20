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
  | Report of int
  | Advance of int
  | Resize of int * int
  | Redraw
  | Output of int
  | Program of int * int

let op =
  let open Alcobar in
  choose
    [
      map [ uint8; option uint8 ] (fun l p -> Task (l, p));
      map [ uint8; uint8 ] (fun i n -> Count (i, n));
      map [ uint8; uint8 ] (fun i n -> Bytes (i, n));
      map [ uint8; uint8 ] (fun i l -> Log (i, l));
      map [ uint8; uint8 ] (fun i o -> End (i, o));
      map [ uint8 ] (fun l -> Report l);
      map [ uint8 ] (fun d -> Advance d);
      map [ uint8; uint8 ] (fun w h -> Resize (w, h));
      const Redraw;
      map [ uint8 ] (fun l -> Output l);
      map [ uint8; uint8 ] (fun c n -> Program (c, n));
    ]

let pp_op ppf = function
  | Task (l, p) -> Fmt.pf ppf "task %d %a" l Fmt.(option int) p
  | Count (i, n) -> Fmt.pf ppf "count %d %d" i n
  | Bytes (i, n) -> Fmt.pf ppf "bytes %d %d" i n
  | Log (i, l) -> Fmt.pf ppf "log %d %d" i l
  | End (i, o) -> Fmt.pf ppf "end %d %d" i o
  | Report l -> Fmt.pf ppf "report %d" l
  | Advance d -> Fmt.pf ppf "advance %d" d
  | Resize (w, h) -> Fmt.pf ppf "resize %d %d" w h
  | Redraw -> Fmt.string ppf "redraw"
  | Output l -> Fmt.pf ppf "output %d" l
  | Program (c, n) -> Fmt.pf ppf "program %d %d" c n

let op = with_printer pp_op op
let header = "HEADER"

let rstrip s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

let screen vte =
  List.map rstrip (String.split_on_char '\n' (Console_vte.to_string vte))

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

(* A write of the display is one synchronized update: it opens and closes
   the mode once, at its ends. *)
let check_synchronized chunk =
  let begin_ = "\027[?2026h" and end_ = "\027[?2026l" in
  let occurrences sub =
    List.length (Re.matches (Re.compile (Re.str sub)) chunk)
  in
  if
    not
      (String.starts_with ~prefix:begin_ chunk
      && String.ends_with ~suffix:end_ chunk
      && occurrences begin_ = 1
      && occurrences end_ = 1)
  then failf "not one synchronized update: %S" chunk

(* A display on an emulated terminal, and what the test knows of it. *)
type term = {
  vte : Console_vte.t;
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
      rows;
  match List.find_opt (has header) rows with
  | Some row when String.contains row 'Z' ->
      failf "the program's bytes are in the header row: %S" row
  | _ -> ()

(* A frame drawn in place shows what the same frame drawn afresh shows:
   nothing of the frame before it is left, and nothing of its own erased. *)
let check_in_place t =
  let before = screen t.vte in
  Option.iter (fun d -> Display.with_output d ignore) t.display;
  let after = screen t.vte in
  if before <> after then
    failf "drawn in place:@.%a@.drawn afresh:@.%a"
      Fmt.(list ~sep:cut string)
      before
      Fmt.(list ~sep:cut string)
      after

let terminal ?(prefix = "") ?theme ?scope ~width ~height ~styled () =
  let t =
    {
      vte = Console_vte.v ~rows:height ~cols:width ();
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
        check_line_ends s;
        check_synchronized s
      end;
      if styled then check_styles_closed s;
      Console_vte.feed t.vte s
    end
  in
  Console_vte.feed t.vte prefix;
  let ppf = Format.make_formatter (Buffer.add_substring chunk) out_flush in
  if styled then Fmt.set_style_renderer ppf `Ansi_tty;
  Option.iter (Theme.set ppf) theme;
  t.display <-
    Some
      (Display.v ~header ?scope
         ~size:(fun () -> !(t.size))
         ~now:(fun () -> !(t.time))
         ppf);
  t

let display t = Option.get t.display
let label i = labels.(i mod Array.length labels)
let levels = [| `App; `Error; `Warning; `Info; `Debug |]
let level i = levels.(i mod Array.length levels)

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
  let top = fst (Console_vte.cursor_pos t.vte) - (height - 2) in
  if
    Console_vte.scrollback_size t.vte = 0
    || List.length before = List.length after
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

(* What a program writes on the display's terminal goes out as it is: here
   a line of [Z] with bytes that move the cursor or erase in it. *)
let moves =
  [|
    "\r";
    "\027[A";
    "\027[3A";
    "\027[K";
    "\027[1K";
    "\027[J";
    "\027[H";
    "\027[9C";
    "\b";
    "\t";
  |]

let program t c n =
  let move = moves.(c mod Array.length moves) in
  let z = String.make (1 + (n mod 30)) 'Z' in
  Display.with_output (display t) (fun ppf ->
      t.in_output := true;
      Fmt.pf ppf "%s%s%s@." z move z;
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
  | Report l -> Display.report (display t) (level l) (label l)
  | Advance n -> t.time := !(t.time) +. (Float.of_int n /. 100.)
  | Resize (w, h) ->
      let w = 8 + (w mod 72) and h = 3 + (h mod 20) in
      t.size := (w, h);
      Console_vte.resize t.vte ~rows:h ~cols:w
  | Redraw -> redraw_twice t
  | Output l -> output t l
  | Program (c, n) -> program t c n

(* The host left [p] cells of a line unended: the region starts on a row of
   its own, below it. *)
let test ops w h styled p =
  let width = 8 + (w mod 72) in
  let prefix = String.make (p mod width) 'p' in
  let t = terminal ~prefix ~width ~height:(3 + (h mod 20)) ~styled () in
  (match Console_vte.scrollback_lines t.vte @ screen t.vte with
  | _ when prefix = "" -> ()
  | first :: _ when rstrip first = prefix -> ()
  | first :: _ -> failf "the host's line %S became %S" prefix first
  | [] -> fail "no screen");
  List.iter
    (fun o ->
      apply t o;
      Display.redraw (display t);
      check_screen t;
      check_in_place t)
    ops;
  Display.close (display t);
  if not (Console_vte.cursor_visible t.vte) then
    fail "the cursor is hidden after close";
  if snd (Console_vte.cursor_pos t.vte) <> 0 then
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

(* The sink and the caller retain no completed task, and a running task
   keeps at most 1 MiB of its output, for the case where it fails. After
   warmup past that cap, live heap measures retention rather than
   allocation volume. *)
let test_retained_heap n =
  let ppf = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ()) in
  let d = Display.v ~size:(fun () -> (80, 10)) ~now:(fun () -> 0.) ppf in
  let task = Display.task ~log_lines:2 d "long running" in
  let batch () =
    for i = 1 to 1000 + n do
      Display.succeed (Display.task ~transient:true d "short lived");
      Display.log task (string_of_int i ^ String.make 512 'x')
    done;
    Display.redraw d;
    Gc.full_major ();
    (Gc.stat ()).live_words
  in
  Fun.protect
    (fun () ->
      ignore (batch ());
      ignore (batch ());
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
  let t = terminal ~width:40 ~height:8 ~styled:true () in
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
           ~finally:(fun () ->
             Format.pp_print_flush ppf ();
             t.in_output := false))
   with
  | _ -> fail "callback exception swallowed"
  | exception Exit -> ());
  Display.redraw d;
  check_screen t;
  Display.close ~running:(if cancel then `Cancel else `Succeed) d;
  check (Console_vte.cursor_visible t.vte);
  check_eq ~pp:Fmt.int 0 (snd (Console_vte.cursor_pos t.vte));
  let y, _ = Console_vte.cursor_pos t.vte in
  Console_vte.feed t.vte "AFTER";
  check (has "AFTER" (List.nth (screen t.vte) y));
  let grid = Console_vte.grid t.vte in
  let index = Grid.idx grid ~x:0 ~y in
  let reference = Console_vte.v ~rows:8 ~cols:40 () in
  Console_vte.feed reference "AFTER";
  let plain = Console_vte.grid reference in
  let first = Grid.idx plain ~x:0 ~y:0 in
  check (Grid.get_fg_r grid index = Grid.get_fg_r plain first);
  check (Grid.get_fg_g grid index = Grid.get_fg_g plain first);
  check (Grid.get_fg_b grid index = Grid.get_fg_b plain first);
  check (Grid.get_bg_r grid index = Grid.get_bg_r plain first)

let test_resize_erases_old_frame n =
  let t = terminal ~width:80 ~height:12 ~styled:true () in
  let d = display t in
  let a = Display.task d ("OLD_SENTINEL_" ^ String.make 45 'x') in
  let b = Display.task d ("OLD_SENTINEL_" ^ String.make 45 'y') in
  Display.redraw d;
  let w = 12 + (n mod 40) in
  t.size := (w, 8);
  Console_vte.resize t.vte ~rows:8 ~cols:w;
  Display.set_label a "new-a";
  Display.set_label b "new-b";
  Display.redraw d;
  check_screen t;
  let rows =
    String.concat "\n"
      (Console_vte.scrollback_lines t.vte @ [ Console_vte.to_string t.vte ])
  in
  if has "OLD_SENTINEL" rows then failf "old frame survived shrink: %S" rows;
  t.size := (80, 12);
  Console_vte.resize t.vte ~rows:12 ~cols:80;
  Display.redraw d;
  let rows =
    String.concat "\n"
      (Console_vte.scrollback_lines t.vte @ [ Console_vte.to_string t.vte ])
  in
  if has "OLD_SENTINEL" rows then
    failf "old frame returned after growth: %S" rows;
  check (has "new-a" rows && has "new-b" rows);
  redraw_twice t;
  Display.close d

let test_static_rows_and_idle n =
  let t = terminal ~width:60 ~height:10 ~styled:true () in
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
  let vte = Console_vte.v ~rows:8 ~cols:40 () in
  Console_vte.feed vte "OUTSIDE_SENTINEL\r\n";
  let frames = ref [] in
  let write bytes start len =
    Buffer.add_substring out bytes start len;
    Buffer.add_substring chunk bytes start len
  in
  let flush () =
    let bytes = Buffer.contents chunk in
    Buffer.clear chunk;
    if bytes <> "" then begin
      Console_vte.feed vte bytes;
      let rows =
        List.map rstrip (Console_vte.scrollback_lines vte @ screen vte)
      in
      frames :=
        (rows, Console_vte.cursor_pos vte, Console_vte.cursor_visible vte)
        :: !frames
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
      Display.Layout.(v [ text input; accent input; label; spacer; elapsed ])
    in
    Theme.set ppf theme;
    let d =
      Display.v ~layout ~header:input
        ~size:(fun () -> (40, 8))
        ~now:(fun () -> 0.)
        ppf
    in
    let task = Display.task d input in
    Display.set_label task input;
    Display.log task lines;
    Display.report d `Warning lines;
    Display.redraw d;
    Display.succeed ~message:input task;
    Display.set_result ~detail:lines d (Ok input);
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
      let t = terminal ~width ~height ~styled () in
      let d = display t in
      for i = 0 to 20 + (n mod 20) do
        let task =
          Display.task ~log_lines:1 d
            (string_of_int i ^ " \u{754c}\u{1f469}\u{200d}\u{1f4bb}")
        in
        Display.log task "\u{754c}\u{1f1eb}\u{1f1f7} tail";
        Display.set_count task ~cur:i ~total:100
      done;
      redraw_twice t;
      (* Nothing has completed or logged a permanent line. A region that fits
       leaves its bottom row free and cannot have produced scrollback. *)
      check_eq ~pp:Fmt.int 0 (Console_vte.scrollback_size t.vte);
      Display.close d;
      check (Console_vte.cursor_visible t.vte);
      check_eq ~pp:Fmt.int 0 (snd (Console_vte.cursor_pos t.vte)))
    [ (1, 1); (2, 80); (300, 10) ]

(* On a terminal of any width above 100 columns, no row the display draws
   nor line it prints for a task is wider than 100. *)
let test_wide n label =
  let width = 101 + (n land 0xff) in
  let t = terminal ~width ~height:10 ~styled:true () in
  let d = display t in
  let task = Display.task ~log_lines:2 d label in
  Display.set_bytes task ~cur:(Int64.of_int (n land 0xffff)) ~total:1_000_000L;
  Display.log task label;
  let child = Display.task ~parent:task d label in
  Display.set_count child ~cur:1 ~total:(1 + (n land 0xfff));
  redraw_twice t;
  Display.succeed child;
  Display.fail task;
  Display.close d;
  List.iter
    (fun l ->
      if Width.string_width l > 100 then
        failf "a line of %d cells on %d columns: %S" (Width.string_width l)
          width l)
    (List.map rstrip (Console_vte.scrollback_lines t.vte) @ screen t.vte)

(* [n] running tasks with [l] log lines each, on a screen [h] rows high: the
   region is at most [h - 1] lines, and a marker counts exactly the lines
   it hides. *)
let test_marker_counts n l h =
  let n = 1 + (n mod 12) and l = l mod 3 and h = 3 + (h mod 12) in
  let t = terminal ~width:30 ~height:h ~styled:false () in
  let d = display t in
  for i = 0 to n - 1 do
    let task = Display.task ~log_lines:l d ("t" ^ string_of_int i) in
    for j = 1 to l do
      Display.log task ("l" ^ string_of_int j)
    done
  done;
  Display.redraw d;
  let rows = List.filter (( <> ) "") (screen t.vte) in
  let shown = List.length rows in
  if shown > h - 1 then failf "%d lines on %d rows" shown h;
  let total = n * (1 + l) in
  let budget = h - 2 in
  (match List.find_opt (has "more lines") rows with
  | Some m ->
      let hidden = Scanf.sscanf (String.trim m) "%_s %d" Fun.id in
      check_eq ~pp:Fmt.int (total - (shown - 2)) hidden
  | None -> if budget >= 3 then check_eq ~pp:Fmt.int total (shown - 1));
  Display.close d

(* {1 Task output and Logs messages}

   A model of where each line goes. Every line is a unique token; the model
   follows which task is current, what each window holds and how each task
   ends, and the properties count where each token lands. *)

type step =
  | New of int option * int
  | Out of int
  | Diag of int option
  | Finish of int * int

let step =
  choose
    [
      map [ option uint8; uint8 ] (fun p l -> New (p, l));
      map [ uint8 ] (fun i -> Out i);
      map [ option uint8 ] (fun i -> Diag i);
      map [ uint8; uint8 ] (fun i o -> Finish (i, o));
    ]

let pp_step ppf = function
  | New (p, l) -> Fmt.pf ppf "new %a %d" Fmt.(option int) p l
  | Out i -> Fmt.pf ppf "out %d" i
  | Diag i -> Fmt.pf ppf "diag %a" Fmt.(option int) i
  | Finish (i, o) -> Fmt.pf ppf "finish %d %d" i o

let step = with_printer pp_step step

type mtask = {
  task : Display.task;
  name : string;
  window_size : int;
  mutable window : string list;  (** Newest first. *)
  mutable outcome : [ `Succeeded | `Failed | `Cancelled ] option;
  mutable kids : mtask list;
}

(* A token, the task it was given to if any, whether it is output and
   whether that task still ran. *)
type line = { token : string; owner : mtask option; output : bool; ran : bool }

let rec take n = function x :: l when n > 0 -> x :: take (n - 1) l | _ -> []

(* The model of [finish]: the children that run end first. *)
let rec model_end o t =
  if Option.is_none t.outcome then begin
    let child = match o with `Succeeded -> `Succeeded | _ -> `Cancelled in
    List.iter (model_end child) (List.rev t.kids);
    t.outcome <- Some o
  end

let outcome o =
  match o mod 3 with 0 -> `Succeeded | 1 -> `Failed | _ -> `Cancelled

let scope current =
  Display.scope
    ~current:(fun () -> !current)
    ~within:(fun task f ->
      let saved = !current in
      current := Some task;
      Fun.protect f ~finally:(fun () -> current := saved))

(* A task of the model, started on [d] under [parent]. *)
let new_task d parent name window_size =
  let task =
    Display.task
      ?parent:(Option.map (fun p -> p.task) parent)
      ~log_lines:window_size d name
  in
  let t = { task; name; window_size; window = []; outcome = None; kids = [] } in
  (match parent with
  | Some ({ outcome = None; _ } as p) -> p.kids <- t :: p.kids
  | Some { outcome = Some `Succeeded; _ } -> model_end `Succeeded t
  | Some _ -> model_end `Cancelled t
  | None -> ());
  t

let end_task t o =
  let o = outcome o in
  (match o with
  | `Succeeded -> Display.succeed t.task
  | `Failed -> Display.fail t.task
  | `Cancelled -> Display.cancel t.task);
  model_end o t

(* Runs [steps] on [d], whose scope reads its current task from [current],
   and closes [d]. *)
let run_steps d current steps =
  let tasks = ref [||] and lines = ref [] in
  let pick i =
    let n = Array.length !tasks in
    if n = 0 then None else Some !tasks.(i mod n)
  in
  let add owner output =
    let token = Fmt.str "k%04d" (List.length !lines) in
    let ran =
      match owner with Some t -> Option.is_none t.outcome | None -> false
    in
    (match owner with
    | Some t when ran -> t.window <- take t.window_size (token :: t.window)
    | _ -> ());
    lines := { token; owner; output; ran } :: !lines;
    token
  in
  List.iter
    (function
      | New (p, l) when Array.length !tasks < 16 ->
          let name = "t" ^ string_of_int (Array.length !tasks) in
          let t = new_task d (Option.bind p pick) name (l mod 4) in
          tasks := Array.append !tasks [| t |]
      | New _ -> ()
      | Out i ->
          Option.iter (fun t -> Display.log t.task (add (Some t) true)) (pick i)
      | Diag i ->
          let t = Option.bind i pick in
          let token = add t false in
          current := Option.map (fun t -> t.task) t;
          Display.report d `Info token;
          current := None
      | Finish (i, o) -> Option.iter (fun t -> end_task t o) (pick i))
    steps;
  Display.close d;
  (* Closing cancels what runs, the latest first. *)
  List.iter (fun t -> model_end `Cancelled t) (List.rev (Array.to_list !tasks));
  List.rev !lines

(* [count token ~label lines] is how many of [lines] hold [token] after
   [label], and how many hold it otherwise. *)
let count token ~label lines =
  List.fold_left
    (fun (l, o) line ->
      if not (has token line) then (l, o)
      else
        match label with
        | Some name when has (name ^ ": " ^ token) line -> (l + 1, o)
        | _ -> (l, o + 1))
    (0, 0) lines

let check_lines ~where expected out =
  List.iter
    (fun (l, (want_labelled, want_other)) ->
      let label = Option.map (fun t -> t.name) l.owner in
      let labelled, other = count l.token ~label out in
      if labelled <> want_labelled || other <> want_other then
        failf "%s: %s %d labelled (want %d), %d other (want %d) in@.%a" where
          l.token labelled want_labelled other want_other
          Fmt.(list ~sep:cut string)
          out)
    expected

let in_window l =
  match l.owner with Some t -> List.mem l.token t.window | None -> false

let ended l = match l.owner with Some t -> t.outcome | None -> None

(* On a pipe every line comes at once, after its task's label or alone
   without a task, and a failed task repeats its window under its line. *)
let test_pipe_lines steps =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let current = ref None in
  let d = Display.v ~scope:(scope current) ~now:(fun () -> 0.) ppf in
  let lines = run_steps d current steps in
  Format.pp_print_flush ppf ();
  let expected l =
    match l.owner with
    | None -> (0, 1)
    | Some _ when l.output && not l.ran -> (0, 0)
    | Some _ ->
        (1, if ended l = Some `Failed && l.ran && in_window l then 1 else 0)
  in
  check_lines ~where:"pipe"
    (List.map (fun l -> (l, expected l)) lines)
    (String.split_on_char '\n' (Buffer.contents buf))

(* On a terminal a succeeded task's output never reaches the scrollback, a
   failed one's all goes under its line, a cancelled one's window stays,
   and every Logs line is kept once, after its task's label. *)
let test_terminal_lines steps =
  let current = ref None in
  let t =
    terminal ~scope:(scope current) ~width:80 ~height:12 ~styled:false ()
  in
  let lines = run_steps (display t) current steps in
  let expected l =
    match l.owner with
    | None -> (0, 1)
    | Some _ when not l.output -> (1, 0)
    | Some _ when not l.ran -> (0, 0)
    | Some _ -> (
        match ended l with
        | Some `Failed -> (0, 1)
        | Some `Cancelled when in_window l -> (0, 1)
        | _ -> (0, 0))
  in
  check_lines ~where:"terminal"
    (List.map (fun l -> (l, expected l)) lines)
    (List.map rstrip (Console_vte.scrollback_lines t.vte) @ screen t.vte)

(* {1 Scenes} *)

let themes = Array.of_list (List.map snd Theme.all)
let pick_theme n = themes.(n mod Array.length themes)

(* The glyphs of straight lines: the only ones a frame that moves swaps, so
   that its corners and tees stay where lines meet. *)
let line_glyphs =
  List.concat_map
    (fun b ->
      let c = Border.chars b in
      [ c.top; c.left; c.right; c.bottom ])
    Border.[ ascii; single; rounded; double; heavy ]

let clusters s =
  let l = ref [] in
  Glyph.String.iter_graphemes
    (fun ~offset ~len -> l := String.sub s offset len :: !l)
    s;
  List.rev !l

(* A table of [cells] from [labels], or a panel with them as its title and
   its lines. *)
let widget kind cells =
  let cells = List.filteri (fun i _ -> i < 4) cells in
  let cells = List.map (fun c -> Span.text (label c)) cells in
  let cells = if cells = [] then [ Span.text "x" ] else cells in
  if kind then fun ppf ->
    Table.pp ppf
      (Table.v
         ~rules:(List.length cells mod 2 = 0)
         [ Table.column "a"; Table.column ~align:`Right "b" ]
         (List.map (fun c -> [ c; c ]) cells))
  else fun ppf ->
    Panel.pp ppf
      (Panel.v ~title:(List.hd cells) (Span.concat ~sep:(Span.text "\n") cells))

(* The rows above the header, at each of [times] in tenths of a second. *)
let scene_frames theme ~width pp times =
  let t = terminal ~theme ~width ~height:600 ~styled:true () in
  let d = display t in
  Display.set_scene d (fun ~height:_ ppf -> pp ppf);
  let frames =
    List.map
      (fun dt ->
        t.time := !(t.time) +. (Float.of_int dt /. 10.);
        Display.redraw d;
        let rec above = function
          | [] -> []
          | r :: rs -> if has header r then [] else r :: above rs
        in
        above (screen t.vte))
      times
  in
  Display.close d;
  frames

(* In every frame of a scene in a theme with motion, only the glyphs of
   straight lines differ from the theme without motion, each from a glyph as wide: every
   line keeps its width and every cell its place. The theme without motion
   draws the same frame at every time: the table as Fmt prints it. *)
let test_scene_frames kind cells n w times =
  let theme = pick_theme n and width = 8 + (w mod 113) in
  let pp = widget kind cells in
  let times = 0 :: times in
  let moving = scene_frames theme ~width pp times in
  let still = scene_frames (Theme.still theme) ~width pp times in
  (* As the same emulator shows it. *)
  let printed =
    let buf = Buffer.create 256 in
    let ppf = Format.formatter_of_buffer buf in
    Theme.set ppf theme;
    Format.pp_set_margin ppf (min width 100);
    pp ppf;
    Format.pp_print_flush ppf ();
    let vte = Console_vte.v ~rows:600 ~cols:width () in
    Console_vte.feed vte
      (String.concat "\r\n" (String.split_on_char '\n' (Buffer.contents buf)));
    List.filter (( <> ) "") (screen vte)
  in
  List.iter (fun rows -> check_eq ~pp:Fmt.(Dump.list string) printed rows) still;
  List.iter2
    (fun moving still ->
      if List.length moving <> List.length still then
        failf "%d rows moving, %d still" (List.length moving)
          (List.length still);
      List.iter2
        (fun m s ->
          let a = clusters m and b = clusters s in
          if List.length a <> List.length b then failf "%S moved to %S" s m;
          List.iter2
            (fun a b ->
              if
                a <> b
                && not
                     (List.mem b line_glyphs
                     && Width.string_width a = Width.string_width b)
              then failf "%S moved to %S" s m)
            a b)
        moving still)
    moving still

(* A scene is never drawn on a pipe, and a widget printed on a formatter is
   motionless: printed twice it is the same bytes, without colour those of
   its theme without motion, and in colour too but for a sweep, which
   colours its frame. *)
let test_scene_pipe kind cells n m =
  let theme = pick_theme n in
  let pp = widget kind cells in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Theme.set ppf theme;
  let d = Display.v ~header ~now:(fun () -> 1.5) ppf in
  Display.set_scene d (fun ~height:_ ppf -> Fmt.string ppf "SCENE_SENTINEL");
  Display.succeed (Display.task d "t");
  Display.close d;
  Format.pp_print_flush ppf ();
  if has "SCENE_SENTINEL" (Buffer.contents buf) then fail "a scene on a pipe";
  let printed ?(styled = true) theme =
    let buf = Buffer.create 256 in
    let ppf = Format.formatter_of_buffer buf in
    Theme.set ppf theme;
    if styled then begin
      Fmt.set_style_renderer ppf `Ansi_tty;
      Color.set_depth ppf `True_color
    end;
    Format.pp_set_margin ppf (4 + (m mod 100));
    pp ppf;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  check_eq ~pp:Fmt.Dump.string (printed theme) (printed theme);
  check_eq ~pp:Fmt.Dump.string
    (printed ~styled:false (Theme.still theme))
    (printed ~styled:false theme);
  if Theme.motion theme <> Some `Sweep then
    check_eq ~pp:Fmt.Dump.string (printed (Theme.still theme)) (printed theme)

let suite =
  ( "display",
    [
      test_case "rows keep to 100 columns on a wide terminal" [ int; bytes ]
        test_wide;
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
      test_case "screen guarantees" [ list op; uint8; uint8; bool; uint8 ] test;
      test_case "plain output" [ list op ] test_plain;
      test_case "on a pipe each line comes once after its task's label"
        [ list step ]
        test_pipe_lines;
      test_case "on a terminal a line stays as its task ended"
        [ list step ]
        test_terminal_lines;
      test_case "the marker counts what the region hides"
        [ uint8; uint8; uint8 ] test_marker_counts;
      test_case "a scene's frames move only its border"
        [ bool; list uint8; uint8; uint8; list uint8 ]
        test_scene_frames;
      test_case "a scene is still off a live display"
        [ bool; list uint8; uint8; uint8 ]
        test_scene_pipe;
    ] )
