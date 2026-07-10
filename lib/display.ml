(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {1 Shared building blocks} *)

(* Colours, ANSI helpers and the themes all come from {!Theme}; the live display
   renders with them. *)
open Ansi

type color = Theme.color

(* The bar accent a display falls back to when its theme has no palette: amber,
   the DOS satellite-workstation CRT phosphor. *)
let amber : color = `Hex (0xff, 0xb0, 0x00)
let hide_cursor = "\027[?25l"
let show_cursor = "\027[?25h"

(** {1 Human-readable quantities} *)

let bytes n =
  let units = [| "B"; "KiB"; "MiB"; "GiB"; "TiB"; "PiB" |] in
  let rec pick i v =
    if v < 1024. || i = Array.length units - 1 then (v, units.(i))
    else pick (i + 1) (v /. 1024.)
  in
  let v, u = pick 0 (Int64.to_float n) in
  if u = "B" then Fmt.str "%.0f %s" v u else Fmt.str "%.2f %s" v u

(* A compact elapsed: "1.3s" below a minute, then "1m05s", then "1h02m". *)
let span secs =
  if secs < 60. then Fmt.str "%.1fs" (Float.max 0. secs)
  else
    let s = int_of_float secs in
    if s < 3600 then Fmt.str "%dm%02ds" (s / 60) (s mod 60)
    else Fmt.str "%dh%02dm" (s / 3600) (s / 60 mod 60)

(* Replace control bytes -- tabs, carriage returns, escapes, BEL, NUL -- in
   caller-supplied text (labels, log lines, headers, messages) with a space.
   They would otherwise reach the terminal raw: a tab expands to a tab stop and
   makes the line wider than its measured width, wrapping it onto a second row
   and desyncing the redraw; an escape would inject control sequences. UTF-8
   continuation bytes are >= 0x80, so this never splits a multibyte glyph. *)
let sanitize s =
  String.map
    (fun c -> if Char.code c < 0x20 || Char.code c = 0x7f then ' ' else c)
    s

(* The live region currently painted on the terminal. [suspend] (and hence the
   Logs reporter) clears it before foreign stdout writes and redraws it
   after. *)
type region = { clear : unit -> unit; redraw : unit -> unit }

let active_region : region option ref = ref None

(* Run [f] with [ppf]'s margin set to [max_int] so ANSI escapes and raw control
   characters pass through without wrapping. *)
let with_unlimited_margin ppf f =
  let old_margin = Format.pp_get_margin ppf () in
  Format.pp_set_margin ppf max_int;
  Fun.protect f ~finally:(fun () -> Format.pp_set_margin ppf old_margin)

(* Clear [n] lines: the current one + (n-1) above; cursor ends at column 0 of
   the topmost cleared line. *)
let pp_clear ppf n =
  if n > 0 then begin
    Format.pp_print_string ppf "\r\027[K";
    for _ = 1 to n - 1 do
      Format.pp_print_string ppf "\027[1A\027[2K"
    done
  end

let pp_block ppf lines =
  let rec write = function
    | [] -> ()
    | [ last ] -> Format.pp_print_string ppf last
    | line :: rest ->
        Format.pp_print_string ppf line;
        Format.pp_print_string ppf "\n";
        write rest
  in
  write lines

(** {1 Interleaving foreign output} *)

let suspend f =
  match !active_region with
  | None -> f ()
  | Some r ->
      r.clear ();
      Fun.protect f ~finally:r.redraw

let logs_reporter (r : Logs.reporter) : Logs.reporter =
  let report src level ~over k msgf =
    r.Logs.report src level ~over k (fun construction ->
        suspend (fun () -> msgf construction))
  in
  { Logs.report }

(* The reporter Logs had before a display took over the terminal. A display
   wraps it on install and puts it back on teardown, so a program never has to
   know that printing a log line must suspend a live region, and successive
   displays do not nest one wrapper inside another. *)
let saved_reporter : Logs.reporter option ref = ref None

let install_reporter () =
  match !saved_reporter with
  | Some _ -> ()
  | None ->
      let r = Logs.reporter () in
      saved_reporter := Some r;
      Logs.set_reporter (logs_reporter r)

let uninstall_reporter () =
  match !saved_reporter with
  | None -> ()
  | Some r ->
      saved_reporter := None;
      Logs.set_reporter r

(** {1 Row layout}

    A small composable DSL for the look of a row, in the spirit of
    [Progress.Line] from the [progress] library: build a list of pieces and the
    row renders them left to right. The default is itself built from these
    primitives. *)

module Line = struct
  type piece =
    | Spinner  (** animated spinner; a green check once finished *)
    | Label  (** the row's label (or finish message), bold *)
    | Bar of int  (** an [n]-cell filled bar *)
    | Percent  (** [ 75%] *)
    | Count  (** [3/4] for counts, ["30.09 MiB / 2.63 GiB"] for bytes *)
    | Rate  (** transfer rate; byte rows only *)
    | Elapsed  (** the row's running timer *)
    | Text of string  (** a literal separator *)
    | Accent of string  (** a literal rendered with the row accent *)
    | Spacer  (** flexible gap; absorbs slack to push later pieces right *)

  type t = { pieces : piece list; color : color }

  let spinner = Spinner
  let label = Label
  let bar n = Bar n
  let percent = Percent
  let count = Count
  let rate = Rate
  let elapsed = Elapsed
  let text s = Text s
  let accent s = Accent s
  let spacer = Spacer
  let v ?(color = amber) pieces = { pieces; color }
  let with_color color t = { t with color }

  let default =
    v
      [
        spinner;
        text " ";
        label;
        text "  ";
        bar 20;
        text " ";
        percent;
        text " ";
        count;
        text " ";
        rate;
        spacer;
        elapsed;
      ]
end

(** {1 Display} *)

type mode = [ `Auto | `Console | `Plain ]
type metric = Spin | Count of int * int | Bytes of int64 * int64

(* The effects a live display needs from its environment: where to write, the
   clock that drives the animation, the terminal geometry to fit, and whether
   the output is a terminal. The eio driver supplies these. *)
type ctx = {
  ppf : Format.formatter;
  now : unit -> float;
  dimensions : unit -> int * int;
  is_tty : bool;
  char_width : Uchar.t -> int;
}

let ctx ~ppf ~now ~dimensions ~is_tty () =
  { ppf; now; dimensions; is_tty; char_width = Width.default_char_width }

let with_char_width c char_width = { c with char_width }
let is_tty c = c.is_tty
let dimensions c = c.dimensions ()
let char_width c = c.char_width

(* A non-interactive context -- standard output, a fixed 80x24 geometry, a zero
   clock, not a tty -- for rendering to a string or a test, where no real
   terminal or clock is in play. *)
let dumb () =
  ctx ~ppf:Format.std_formatter
    ~now:(fun () -> 0.)
    ~dimensions:(fun () -> (80, 24))
    ~is_tty:false ()

type t = {
  dppf : Format.formatter;
  plain : bool;
  now : unit -> float;
  dimensions : unit -> int * int;
      (* terminal (width, height), re-read each paint so a resize is handled *)
  char_width : Uchar.t -> int;
  header : string option;
  mutable banner : string Anim.t option;
      (* an optional animation painted above the header; sampled at the
         display's elapsed time each paint, so the tick loop animates it *)
  default_style : Line.t;
  theme : Theme.t;
  palette : color list;
  dstart : float;
  mutable width : int;
  mutable height : int;
      (* current terminal geometry. [height] caps the live region so a redraw
         never scrolls a stale copy into unreachable scrollback; [width] bounds
         every emitted line so nothing wraps onto a second physical row (which
         would desync the one-line-per-row redraw maths). Re-read each paint
         unless pinned, so a resize never corrupts the display. *)
  auto_width : bool;
  auto_height : bool;
  mutable rows : row list; (* insertion order *)
  mutable finished : int;
  mutable live_height : int;
  mutable last_lines : string list; (* the lines currently on screen *)
  mutable torn_down : bool;
}

and row = {
  disp : t;
  parent : row option;
  mutable style : Line.t;
  log_lines : int; (* size of the live log tail shown under the row; 0 = none *)
  mutable logs : (float * string) list;
      (* the last [log_lines] lines, oldest first; each paired with the gap (in
         seconds) since the previous log line, so a stall between two lines is
         visible rather than silent *)
  mutable log_prev_t : float; (* time of the previous log line *)
  mutable rlabel : string;
  mutable metric : metric;
  rstart : float;
  mutable rfinish : float option;
  mutable rmessage : string option;
  mutable rkeep_live : bool;
  mutable rerror : bool;
  (* finished in error; flips the summary marker to [x] *)
  (* EWMA byte-rate sampling *)
  mutable last_cur : int64;
  mutable last_t : float;
  mutable rate : float;
}

let pp ppf t =
  Fmt.pf ppf "<tty-display %dx%d %d/%d rows%s>" t.width t.height t.finished
    (List.length t.rows)
    (if t.torn_down then " torn-down" else "")

let render_width t = if t.plain then t.width else max 0 (t.width - 1)
let string_width t = Width.string_width ~char_width:t.char_width
let truncate t = Width.truncate ~char_width:t.char_width
let rec depth = function None -> 0 | Some r -> 1 + depth r.parent

(* Plain (append-only) mode keeps a flat indent: a committed line is emitted
   before later siblings are known, so it cannot draw a stable tree guide. *)
let indent row = String.make (2 * depth row.parent) ' '

(* The live (tty) view draws real tree guides, sharing {!Tree.prefix}. A row's
   guide is computed from its position among siblings each repaint. *)
let siblings t row =
  List.filter
    (fun r ->
      match (r.parent, row.parent) with
      | Some a, Some b -> a == b
      | None, None -> true
      | _ -> false)
    t.rows

let is_last_child t row =
  match List.rev (siblings t row) with last :: _ -> last == row | [] -> true

let rec lasts_path t row =
  let own = is_last_child t row in
  match row.parent with None -> [ own ] | Some p -> lasts_path t p @ [ own ]

let guide_prefix t row =
  Tree.prefix ~guide:t.theme.Theme.guide (lasts_path t row)

(* The vertical-bar column beneath a row, under which its log tail sits: each
   level is a pipe when more siblings follow, or blank when it was the last. *)
let continuation t row =
  let g = t.theme.Theme.guide in
  String.concat ""
    (List.map
       (fun is_last -> if is_last then g.Tree.space else g.Tree.pipe)
       (lasts_path t row))

let elapsed t row =
  match row.rfinish with
  | Some f -> f -. row.rstart
  | None -> t.now () -. row.rstart

let pct_of = function
  | Spin -> None
  | Count (c, tot) -> Some (if tot > 0 then min 100 (c * 100 / tot) else 0)
  | Bytes (c, tot) ->
      Some
        (if tot > 0L then
           min 100 (Int64.to_int (Int64.div (Int64.mul c 100L) tot))
         else 0)

(* Render one style piece against a row's current data; [""] when the piece does
   not apply. [Label] and [Spacer] render empty here -- they are substituted
   after width budgeting. Glyphs come from the display's {!Theme.t}. *)
let render_spinner theme ~finished ~color ~frame row =
  (* Match the header's marker vocabulary so a row and the summary never use two
     different glyphs for the same state: green [+] done, red [x] on error, the
     animated spinner while running. *)
  if not finished then
    styled (color_code color) (Spinner.frame theme.Theme.spinner frame)
  else if row.rerror then styled (bold ^ color_code `Red) "[x]"
  else styled (bold ^ color_code `Green) theme.Theme.done_marker

let render_bar theme row ~finished ~color ~width =
  if finished then ""
  else
    match pct_of row.metric with
    | Some p -> Bar.render ~theme ~color ~width ~pct:p ()
    | None -> ""

let render_percent row ~finished =
  if finished then ""
  else
    match pct_of row.metric with
    | Some p -> Fmt.kstr dimmed "%3d%%" p
    | None -> ""

let render_count row =
  match row.metric with
  | Count (c, tot) -> Fmt.kstr dimmed "%d/%d" c tot
  | Bytes (c, tot) -> dimmed (bytes c ^ " / " ^ bytes tot)
  | Spin -> ""

let render_rate row ~finished =
  if finished then ""
  else
    match row.metric with
    | Bytes _ when row.rate > 1. ->
        dimmed (bytes (Int64.of_float row.rate) ^ "/s")
    | _ -> ""

let render_piece t row ~finished ~color ~frame (piece : Line.piece) =
  let theme = t.theme in
  match piece with
  | Line.Spinner -> render_spinner theme ~finished ~color ~frame row
  | Line.Label | Line.Spacer -> ""
  | Line.Bar n -> render_bar theme row ~finished ~color ~width:n
  | Line.Percent -> render_percent row ~finished
  | Line.Count -> render_count row
  | Line.Rate -> render_rate row ~finished
  | Line.Elapsed -> dimmed (span (elapsed t row))
  | Line.Text s -> s
  | Line.Accent s -> styled (bold ^ color_code color) s

(* Render a row from its style. The label is the flexible content (truncated to
   fit) and a single [Spacer] absorbs the remaining slack so later pieces (the
   elapsed timer) sit at the right margin. *)
let render_row t row =
  let finished = row.rfinish <> None in
  let { Line.pieces; color } = row.style in
  let frame = int_of_float ((t.now () -. t.dstart) *. 10.) in
  let rendered =
    List.map (fun p -> (p, render_piece t row ~finished ~color ~frame p)) pieces
  in
  let indent_s = guide_prefix t row in
  let has_label = List.exists (fun (p, _) -> p = Line.Label) rendered in
  let has_spacer = List.exists (fun (p, _) -> p = Line.Spacer) rendered in
  let fixed_w =
    string_width t indent_s
    + List.fold_left
        (fun acc (p, s) ->
          match p with
          | Line.Label | Line.Spacer -> acc
          | _ -> acc + string_width t s)
        0 rendered
  in
  let label_text =
    if finished then Option.value row.rmessage ~default:row.rlabel
    else row.rlabel
  in
  (* Reserve one column for the spacer so the pieces flanking it never touch. *)
  let reserve = if has_spacer then 1 else 0 in
  let width = render_width t in
  let avail = max 0 (width - fixed_w - reserve) in
  let label_shown =
    if not has_label then ""
    else if string_width t label_text > avail then truncate t avail label_text
    else label_text
  in
  let slack = max reserve (width - fixed_w - string_width t label_shown) in
  let buf = Buffer.create 80 in
  Buffer.add_string buf indent_s;
  List.iter
    (fun (p, s) ->
      match p with
      | Line.Label -> Buffer.add_string buf (styled bold label_shown)
      | Line.Spacer -> Buffer.add_string buf (String.make slack ' ')
      | _ -> Buffer.add_string buf s)
    rendered;
  (* Final guard: never exceed the width, or the line wraps and the redraw maths
     (which assumes one row per line) corrupts. *)
  truncate t width (Buffer.contents buf)

(* The compact metric for a finished row in a plain (non-tty) log line. *)
let final_metric row =
  match row.metric with
  | Spin -> ""
  | Count (c, tot) -> Fmt.str "  %d/%d" c tot
  | Bytes (c, tot) -> Fmt.str "  %s / %s" (bytes c) (bytes tot)

let plain_committed t row =
  let label = Option.value row.rmessage ~default:row.rlabel in
  Fmt.str "%s=> %s%s  %s" (indent row) label (final_metric row)
    (span (elapsed t row))

let complete t =
  let total = List.length t.rows in
  total > 0 && t.finished >= total

let any_error t = List.exists (fun r -> r.rerror) t.rows

(* The summary marker and its colour: [-] (accent) while work is in progress,
   then [+] (green) once every row is done, or [x] (red) if any finished in
   error. *)
let header_marker t =
  if not (complete t) then ("[-]", t.default_style.Line.color)
  else if any_error t then ("[x]", `Red)
  else ("[+]", `Green)

let summary t =
  match t.header with
  | None -> None
  | Some h ->
      Fmt.kstr
        (fun s -> Some s)
        "%s %s %s (%d/%d)"
        (fst (header_marker t))
        h
        (span (t.now () -. t.dstart))
        t.finished (List.length t.rows)

(* The coloured header for a terminal: a bright marker, a bold name, a dim
   elapsed and done/total. *)
let header_styled t =
  match t.header with
  | None -> None
  | Some h ->
      (* Bounded to the terminal width, like every row: an over-wide header that
         wrapped onto a second physical row would make the redraw under-clear by
         one line every frame and strand a stale header (the crawling-timestamp
         duplication). *)
      Some
        (truncate t (render_width t)
           (String.concat ""
              (let marker, color = header_marker t in
               [
                 color_code color;
                 marker;
                 reset_code;
                 " ";
                 bold;
                 h;
                 reset_code;
                 " ";
                 Fmt.kstr dimmed "%s (%d/%d)"
                   (span (t.now () -. t.dstart))
                   t.finished (List.length t.rows);
               ])))

(* A live row split into its own line and its log tail (dimmed and indented
   beneath it, like buildkit's "=> => # ..." lines). Keeping the two apart lets
   the height cap drop log lines while preserving the tree of rows. *)
(* A log line's trailing time gap, shown only when at least a second elapsed
   since the previous line, so rapid output stays clean but a stall stands out
   (e.g. ["# linking  +18.0s"]). *)
let log_gap_threshold = 1.0
let gap_suffix gap = if gap >= log_gap_threshold then "  +" ^ span gap else ""

let row_block_parts t row =
  let tail =
    List.map
      (fun (gap, l) ->
        truncate t (render_width t)
          (String.concat ""
             [ continuation t row; dimmed ("# " ^ l ^ gap_suffix gap) ]))
      row.logs
  in
  (render_row t row, tail)

let rec drop n = function _ :: tl when n > 0 -> drop (n - 1) tl | l -> l

(* Keep the live region within the terminal height. A redraw clears the old
   region with cursor-relative moves ([ESC[1A]); those cannot reach a line that
   scrolled into scrollback, so a region taller than the screen would leave a
   stale copy behind on the next paint, which reads as a duplicated row.

   Trim like buildkit: the rows (the tree skeleton) always stay; only the log
   windows are shed, oldest first, until the region fits, with a marker counting
   what was hidden. If even the bare rows overflow, keep the most recent
   ones. *)
let cap_region t ~header ~blocks =
  let hn = List.length header in
  let budget = t.height - 1 - hn in
  (* reserve one row for the cursor / next commit *)
  let total =
    List.fold_left (fun n (_, logs) -> n + 1 + List.length logs) 0 blocks
  in
  if t.height <= 0 || total <= budget then
    header @ List.concat_map (fun (line, logs) -> line :: logs) blocks
  else
    let marker hidden =
      truncate t (render_width t)
        (Fmt.kstr dimmed "  ... (%d more lines)" hidden)
    in
    let nrows = List.length blocks in
    if nrows + 1 > budget then begin
      (* Even the bare rows do not fit. Keep the first row (the build phase /
         root, the context for everything below) and the most recent rows (the
         active leaf), collapsing the middle behind a marker -- so a deep tree
         keeps both its phase and its active step on screen. *)
      let rows = List.map fst blocks in
      match rows with
      | [] -> header
      | first :: rest ->
          let tail_keep = max 0 (budget - 2) in
          let shown = drop (List.length rest - tail_keep) rest in
          let hidden = total - 1 - List.length shown in
          header @ (first :: marker hidden :: shown)
    end
    else begin
      (* Spend the budget (less one row for the marker) on the most recent log
         lines, shedding the oldest. Walk the flat row/log stream and drop the
         first [to_drop] log lines, keeping every row line. *)
      let to_drop = total - (budget - 1) in
      let flat =
        List.concat_map
          (fun (line, logs) -> `Row line :: List.map (fun l -> `Log l) logs)
          blocks
      in
      let rec shed dropped acc = function
        | rest when dropped >= to_drop ->
            List.rev_append acc
              (List.map (function `Row s | `Log s -> s) rest)
        | `Log _ :: tl -> shed (dropped + 1) acc tl
        | `Row s :: tl -> shed dropped (s :: acc) tl
        | [] -> List.rev acc
      in
      header @ (marker to_drop :: shed 0 [] flat)
    end

let rec row_live row =
  (row.rfinish = None || row.rkeep_live)
  && match row.parent with None -> true | Some parent -> row_live parent

(* The current banner frame, each line truncated to the terminal width so it
   never wraps. Empty when no banner is set. *)
let banner_lines t =
  match t.banner with
  | None -> []
  | Some a ->
      let frame = Anim.frame a ~elapsed:(t.now () -. t.dstart) in
      List.map
        (fun l -> truncate t (render_width t) l)
        (String.split_on_char '\n' frame)

let live_lines t =
  let header =
    banner_lines t @ match header_styled t with Some s -> [ s ] | None -> []
  in
  let active = List.filter row_live t.rows in
  let blocks = List.map (row_block_parts t) active in
  cap_region t ~header ~blocks

(* Re-read the terminal geometry (unless pinned) so a resize between frames is
   picked up before the next clear/redraw, the way buildkit reads the size every
   frame. A stale, too-large width would let a line wrap; a stale height would
   mis-size the cap -- either desyncs the redraw. *)
let refresh_size t =
  if t.auto_width then t.width <- fst (t.dimensions ());
  if t.auto_height then t.height <- snd (t.dimensions ())

let paint t =
  if (not t.plain) && not t.torn_down then begin
    refresh_size t;
    let lines = live_lines t in
    (* Skip the repaint when the frame is byte-identical to what is already on
       screen: clearing and rewriting the same glyphs on every animation tick is
       the flicker. [live_height] is the on-screen height; [commit] and [clear]
       reset it to 0, so a region that was actually cleared never matches and is
       always repainted. *)
    if lines <> t.last_lines || List.length lines <> t.live_height then begin
      with_unlimited_margin t.dppf (fun () ->
          pp_clear t.dppf t.live_height;
          pp_block t.dppf lines;
          Format.pp_print_flush t.dppf ());
      (* No trailing newline: it would land on the bottom screen row and scroll
         the region up every paint, stranding the top line (the header) in
         scrollback where the cursor-relative clear can no longer reach it. The
         cursor rests at the end of the last region line; the next [pp_clear]
         returns to column 0 from there. *)
      t.live_height <- List.length lines;
      t.last_lines <- lines
    end
  end

(* Clear the live region, append [s] as a permanent scrollback line, then
   repaint the (now smaller) region beneath it. [s] is bounded to the current
   width so a committed line never wraps in the scrollback. *)
let commit t s =
  refresh_size t;
  with_unlimited_margin t.dppf (fun () ->
      pp_clear t.dppf t.live_height;
      Format.pp_print_string t.dppf (truncate t (render_width t) s);
      Format.pp_print_string t.dppf "\n";
      Format.pp_print_flush t.dppf ());
  t.live_height <- 0;
  paint t

(* Take over the terminal: hide the cursor, register the clear/redraw hooks that
   {!suspend} uses around foreign output, wrap the installed Logs reporter so a
   log line suspends the live region, and paint the initial frame. *)
let install_region t =
  Format.pp_print_string t.dppf hide_cursor;
  Format.pp_print_flush t.dppf ();
  install_reporter ();
  active_region :=
    Some
      {
        clear =
          (fun () ->
            with_unlimited_margin t.dppf (fun () ->
                pp_clear t.dppf t.live_height;
                Format.pp_print_flush t.dppf ());
            t.live_height <- 0);
        redraw = (fun () -> paint t);
      };
  paint t

let v ~ctx ?(mode = `Auto) ?(theme = Theme.default) ?(palette = [])
    ?(bar = Line.default) ?width ?height ?header () =
  let plain =
    match mode with
    | `Plain -> true
    | `Console -> false
    | `Auto -> not ctx.is_tty
  in
  (* A pinned [width]/[height] (tests) stays fixed; otherwise re-read the live
     terminal geometry on every paint so a resize is handled. *)
  let auto_width = width = None and auto_height = height = None in
  let term_w, term_h = ctx.dimensions () in
  let width =
    match width with Some w -> w | None -> if plain then 0 else term_w
  in
  let height = match height with Some h -> h | None -> term_h in
  let t =
    {
      dppf = ctx.ppf;
      plain;
      now = ctx.now;
      dimensions = ctx.dimensions;
      char_width = ctx.char_width;
      header = Option.map sanitize header;
      banner = None;
      default_style = bar;
      theme;
      palette;
      dstart = ctx.now ();
      width;
      height;
      auto_width = auto_width && not plain;
      auto_height = auto_height && not plain;
      rows = [];
      finished = 0;
      live_height = 0;
      last_lines = [];
      torn_down = false;
    }
  in
  if not plain then install_region t;
  t

let set_banner t a =
  t.banner <- Some a;
  if not t.plain then paint t

let clear_banner t =
  t.banner <- None;
  if not t.plain then paint t

let touch r = if not r.disp.plain then paint r.disp

(* A row's accent: an explicit [color], else the next palette colour (fixed at
   creation, so it is stable across redraws), else the base style's own. *)
let row_style t ~bar ~color =
  let base = Option.value bar ~default:t.default_style in
  match color with
  | Some c -> Line.with_color c base
  | None -> (
      match (bar, t.palette) with
      | None, (_ :: _ as pal) ->
          Line.with_color
            (List.nth pal (List.length t.rows mod List.length pal))
            base
      | _ -> base)

let row ?parent ?bar ?color ?(log_lines = 0) t label =
  let now = t.now () in
  let r =
    {
      disp = t;
      parent;
      style = row_style t ~bar ~color;
      log_lines = max 0 log_lines;
      logs = [];
      log_prev_t = now;
      rlabel = sanitize label;
      metric = Spin;
      rstart = now;
      rfinish = None;
      rmessage = None;
      rkeep_live = false;
      rerror = false;
      last_cur = 0L;
      last_t = now;
      rate = 0.;
    }
  in
  t.rows <- t.rows @ [ r ];
  touch r;
  r

let set_label r label =
  r.rlabel <- sanitize label;
  touch r

let set_bar r bar =
  r.style <- bar;
  touch r

(* Split on newlines, dropping a trailing CR and empty lines. *)
let lines_of_string s =
  String.split_on_char '\n' s
  |> List.map (fun l ->
      let n = String.length l in
      if n > 0 && l.[n - 1] = '\r' then String.sub l 0 (n - 1) else l)
  |> List.filter (fun l -> l <> "")

let log r line =
  let t = r.disp in
  let lines = List.map sanitize (lines_of_string line) in
  (* Gap since the previous log line, surfacing time spent with no output. *)
  let gap_of l =
    let now = t.now () in
    let gap = now -. r.log_prev_t in
    r.log_prev_t <- now;
    (gap, l)
  in
  if t.plain then
    List.iter
      (fun l ->
        let gap, l = gap_of l in
        Fmt.pf t.dppf "%s  # %s%s@." (indent r) l (gap_suffix gap))
      lines
  else if r.log_lines > 0 then begin
    List.iter
      (fun l ->
        r.logs <- r.logs @ [ gap_of l ];
        if List.length r.logs > r.log_lines then r.logs <- List.tl r.logs)
      lines;
    touch r
  end

let set_count r ~cur ~total =
  r.metric <- Count (cur, total);
  touch r

let set_bytes r ~cur ~total =
  let now = r.disp.now () in
  let dt = now -. r.last_t in
  if dt > 0. then begin
    let inst = Float.max 0. (Int64.to_float (Int64.sub cur r.last_cur) /. dt) in
    r.rate <- (if r.rate <= 0. then inst else (0.3 *. inst) +. (0.7 *. r.rate));
    r.last_cur <- cur;
    r.last_t <- now
  end;
  r.metric <- Bytes (cur, total);
  touch r

let finish ?(live = false) ?(error = false) ?message r =
  let t = r.disp in
  if r.rfinish = None then begin
    r.rfinish <- Some (t.now ());
    r.rmessage <- Option.map sanitize message;
    r.rkeep_live <- live;
    r.rerror <- error;
    t.finished <- t.finished + 1;
    if live then touch r
    else if t.plain then Fmt.pf t.dppf "%s@." (plain_committed t r)
    else commit t (render_row t r)
  end

let remove r =
  let t = r.disp in
  if List.memq r t.rows then begin
    t.rows <- List.filter (fun x -> x != r) t.rows;
    if r.rfinish <> None then t.finished <- t.finished - 1;
    if not t.plain then paint t
  end

let tick t = if not t.plain then paint t

let with_output t f =
  if t.plain then f ()
  else begin
    with_unlimited_margin t.dppf (fun () ->
        pp_clear t.dppf t.live_height;
        Format.pp_print_flush t.dppf ());
    t.live_height <- 0;
    Fun.protect f ~finally:(fun () -> paint t)
  end

let clear t =
  if not t.torn_down then begin
    t.torn_down <- true;
    active_region := None;
    uninstall_reporter ();
    if not t.plain then begin
      with_unlimited_margin t.dppf (fun () ->
          pp_clear t.dppf t.live_height;
          Format.pp_print_string t.dppf show_cursor;
          Format.pp_print_flush t.dppf ());
      t.live_height <- 0
    end
  end

(* The final result banner: a faint coloured [OK]/[FAIL] marker (green or red)
   followed by the message in bold, plain text under [`Plain]. A long message
   word-wraps under a hanging indent aligned past the marker, rather than being
   cut at the width -- this is printed after the live region is released, so a
   second physical line is safe. *)
let result_line t result =
  let marker, code, msg =
    match result with
    | `Ok msg -> (t.theme.Theme.ok_marker, color_code `Green, msg)
    | `Fail msg -> (t.theme.Theme.fail_marker, color_code `Red, msg)
  in
  if t.plain then marker ^ " " ^ msg
  else begin
    let indent = string_width t marker + 1 in
    let body = Width.wrap ~indent t.width msg in
    (* [wrap] indents every line, including the first; drop the first line's
       indent so the marker sits there and continuation lines stay aligned. *)
    let rest =
      if String.length body >= indent then
        String.sub body indent (String.length body - indent)
      else body
    in
    styled (dim ^ code) marker ^ " " ^ styled bold rest
  end

(* The optional detail paragraph shown dimmed just above the verdict banner: the
   reason, a log-path pointer, whatever context the result needs. Split on
   newlines; a line wider than the terminal word-wraps (it is printed after the
   live region is released, so extra physical lines are safe), each resulting
   line dimmed (plain text under [`Plain]). *)
let detail_lines t = function
  | None -> []
  | Some d ->
      List.concat_map
        (fun l ->
          if t.plain then [ l ]
          else
            String.split_on_char '\n' (Width.wrap t.width l) |> List.map dimmed)
        (String.split_on_char '\n' d)

let finish_all ?result ?detail t =
  if not t.torn_down then begin
    List.iter (fun r -> if r.rfinish = None then finish r) t.rows;
    t.torn_down <- true;
    active_region := None;
    uninstall_reporter ();
    let detail = detail_lines t detail in
    let banner = Option.map (result_line t) result in
    if t.plain then begin
      (match summary t with Some s -> Fmt.pf t.dppf "%s@." s | None -> ());
      List.iter (fun s -> Fmt.pf t.dppf "%s@." s) detail;
      (match banner with Some s -> Fmt.pf t.dppf "%s@." s | None -> ());
      Format.pp_print_flush t.dppf ()
    end
    else begin
      let line s =
        Format.pp_print_string t.dppf s;
        Format.pp_print_string t.dppf "\n"
      in
      with_unlimited_margin t.dppf (fun () ->
          pp_clear t.dppf t.live_height;
          (match header_styled t with Some s -> line s | None -> ());
          List.iter line detail;
          (match banner with Some s -> line s | None -> ());
          Format.pp_print_string t.dppf show_cursor;
          Format.pp_print_flush t.dppf ());
      t.live_height <- 0
    end
  end
