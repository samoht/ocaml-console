(** Render a {!Console.Display} TTY session (real control characters) through a
    tiny terminal emulator and print the reconstructed screen after each frame,
    so a cram test can pin exactly what a terminal shows: nested rows with log
    windows, rows being finished, fresh logs, and the redraw in between.

    A fixed clock keeps spinners and timers deterministic. Width and height are
    taken from [$COLUMNS]/[$LINES] (set by the cram test) so the geometry -- and
    therefore the wrapping and the height cap -- is stable. *)

(* {2 A minimal VT}: applies exactly the controls Progress emits -- text, [\r],
   [\n] (CR+LF, as a cooked-mode tty), CSI K/2K erase, CSI nA cursor-up clamped
   to the top of the physical screen -- and ignores SGR colours and cursor
   show/hide. The cell grid never holds escapes, so the dump is plain text. *)
module Vt = struct
  type t = {
    height : int;
    mutable lines : string array;
    mutable used : int;
    mutable row : int;
    mutable col : int;
  }

  let create ~height =
    { height; lines = Array.make 16 ""; used = 1; row = 0; col = 0 }

  let ensure t r =
    if r >= Array.length t.lines then begin
      let bigger = Array.make (max (r + 1) (2 * Array.length t.lines)) "" in
      Array.blit t.lines 0 bigger 0 t.used;
      t.lines <- bigger
    end;
    if r >= t.used then t.used <- r + 1

  let screen_top t = max 0 (t.used - t.height)

  let put t s =
    ensure t t.row;
    let line = t.lines.(t.row) in
    let line =
      if String.length line < t.col then
        line ^ String.make (t.col - String.length line) ' '
      else line
    in
    let before = String.sub line 0 t.col in
    let off = t.col + String.length s in
    let after =
      if String.length line > off then
        String.sub line off (String.length line - off)
      else ""
    in
    t.lines.(t.row) <- before ^ s ^ after;
    t.col <- t.col + String.length s

  let read_csi s i =
    let n = String.length s in
    let j = ref i in
    while !j < n && not (s.[!j] >= '@' && s.[!j] <= '~') do
      incr j
    done;
    if !j < n then (String.sub s i (!j - i), s.[!j], !j + 1)
    else (String.sub s i (!j - i), ' ', !j)

  let int_param p = match int_of_string_opt p with Some n -> n | None -> 1

  let feed t s =
    let n = String.length s in
    let i = ref 0 in
    while !i < n do
      match s.[!i] with
      | '\027' when !i + 1 < n && s.[!i + 1] = '[' ->
          let params, final, next = read_csi s (!i + 2) in
          (match final with
          | 'A' -> t.row <- max (screen_top t) (t.row - int_param params)
          | 'B' -> t.row <- t.row + int_param params
          | 'K' ->
              ensure t t.row;
              if params = "2" then t.lines.(t.row) <- ""
              else
                let line = t.lines.(t.row) in
                if String.length line > t.col then
                  t.lines.(t.row) <- String.sub line 0 t.col
          | _ -> ());
          i := next
      | '\r' ->
          t.col <- 0;
          incr i
      | '\n' ->
          t.col <- 0;
          t.row <- t.row + 1;
          ensure t t.row;
          incr i
      | c ->
          put t (String.make 1 c);
          incr i
    done

  (* Trailing blanks dropped so the cram diff is not whitespace-sensitive. *)
  let rstrip s =
    let n = ref (String.length s) in
    while !n > 0 && (s.[!n - 1] = ' ' || s.[!n - 1] = '\t') do
      decr n
    done;
    String.sub s 0 !n

  (* The screen content with trailing blank rows dropped, so the cram diff is
     not sensitive to how many cleared rows the emulator is still tracking. *)
  let dump t =
    let all = List.init t.used (fun i -> rstrip t.lines.(i)) in
    let rec drop_trailing = function "" :: tl -> drop_trailing tl | l -> l in
    List.rev (drop_trailing (List.rev all))
end

let geom name default =
  match Option.bind (Sys.getenv_opt name) int_of_string_opt with
  | Some v -> v
  | None -> default

let () =
  let height = geom "LINES" 24 in
  let width = geom "COLUMNS" 80 in
  let vt = Vt.create ~height in
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let now = ref 0. in
  let ctx =
    Console.Display.ctx ~ppf
      ~now:(fun () -> !now)
      ~dimensions:(fun () -> (width, height))
      ~is_tty:true ()
  in
  let d = Console.Display.v ~ctx ~mode:`Console ~height ~header:"Build" () in
  let sync () =
    Format.pp_print_flush ppf ();
    Vt.feed vt (Buffer.contents buf);
    Buffer.clear buf
  in
  let frame title =
    sync ();
    Printf.printf "--- %s ---\n" title;
    List.iter
      (fun l ->
        print_string l;
        print_newline ())
      (Vt.dump vt)
  in
  let module P = Console.Display in
  (* A tree: app -> {resolve, compile}, each with a small log window. *)
  let app = P.row ~log_lines:2 d "app" in
  P.log app "planning";
  frame "tree with one node + a log";
  let resolve = P.row ~parent:app ~log_lines:2 d "resolve" in
  P.log resolve "fetch index";
  let compile = P.row ~parent:app ~log_lines:3 d "compile" in
  P.log compile "cc a.o";
  P.log compile "cc b.o";
  frame "two children, each streaming logs";
  (* Close a row, then add more logs to the surviving rows and redraw. *)
  P.finish resolve;
  frame "resolve finished (committed), siblings still live";
  P.log compile "cc c.o";
  P.log compile "link";
  P.log app "still working";
  frame "more logs after the row closed";
  P.finish compile;
  P.finish app;
  P.finish_all d;
  frame "all finished"
