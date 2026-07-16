(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Terminal progress display.

    A {!t} owns a region of the terminal holding an optional header and a set of
    rows that update in place. {!finish}ing a row freezes its timer and commits
    it to the terminal's scrollback; rows still running keep animating below. A
    single progress bar is just a one-row display.

    With no terminal -- a captured log, a pipe -- or in [`Plain] mode, the very
    same calls instead emit one tidy append-only line per finished row and a
    final summary, with no escape codes: the text a log collector ingests.

    {[
    let build ~ctx =
      let d = Console.Display.v ~ctx ~header:"Building" () in
      let step = Console.Display.row d "resolve image" in
      Console.Display.finish step;
      let layer = Console.Display.row ~parent:step d "sha256:abcd" in
      Console.Display.set_bytes layer ~cur:1024L ~total:1048576L;
      Console.Display.finish layer;
      Console.Display.finish_all d
    ]} *)

type color = Theme.color
(** A foreground colour; the {!Theme.color} the widgets share. *)

(** {1 Human-readable byte sizes} *)

val bytes : int64 -> string
(** [bytes n] is [n] bytes in the largest binary (1024-based) unit whose
    mantissa stays below 1024, to two decimals: [bytes 31554000L] is
    ["30.09 MiB"]. Units are [B], [KiB], [MiB], [GiB], [TiB], [PiB]. *)

(** {1 Row layout}

    A small composable DSL for the look of a row, in the spirit of [Line] from
    the {{:https://github.com/CraigFe/progress}progress} library: build a list
    of {!Line.piece}s and the row renders them left to right. Pieces that do not
    apply to a row's data render as nothing (e.g. {!Line.rate} on a counter).
    Pass a style to {!v} (the display default) or {!row} (per row). The
    {!Line.default} is itself built from these primitives. *)

module Line : sig
  type piece
  (** One piece of a row's rendering. *)

  (** {2 Pieces} *)

  val spinner : piece
  (** An animated spinner; a green check once the row is finished. *)

  val label : piece
  (** The row's label (or its finish message), in bold. *)

  val bar : int -> piece
  (** [bar n] is an [n]-cell filled progress bar. *)

  val percent : piece
  (** A right-justified percentage, e.g. [ 75%]. *)

  val count : piece
  (** [3/4] for a counter, ["30.09 MiB / 2.63 GiB"] for bytes. *)

  val rate : piece
  (** A transfer rate (byte rows only), e.g. ["12.30 MiB/s"]. *)

  val elapsed : piece
  (** The row's running elapsed timer, e.g. ["1.3s"]. *)

  val text : string -> piece
  (** [text s] is the literal string [s] -- a separator or label. *)

  val accent : string -> piece
  (** [accent s] is the literal string [s] rendered in the row's accent colour
      and bold weight. *)

  val spacer : piece
  (** A flexible gap that absorbs slack, pushing later pieces to the right
      margin. *)

  (** {2 Styles} *)

  type t
  (** A row's layout: a list of pieces and an accent colour for the spinner and
      bar fill. *)

  val v : ?color:color -> piece list -> t
  (** [v pieces] renders [pieces] left to right. The accent colour (default
      [`Cyan]) tints the spinner and bar fill. *)

  val with_color : color -> t -> t
  (** [with_color c t] is [t] with its accent colour set to [c]. *)

  val default : t
  (** The default style, built from the primitives:
      {[
      Console.Display.Line.(
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
          ])
      ]} *)
end

(** {1 Display} *)

type t
(** A live display. Create one with {!v}, populate it with {!row}, and release
    the terminal with {!finish_all}. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] writes a short representation of [t] to [ppf]. *)

type mode = [ `Auto | `Console | `Plain ]
(** How a display paints. [`Console] redraws rows in place with the cursor;
    [`Plain] writes append-only lines and never moves the cursor; [`Auto], the
    default, is [`Console] on a terminal and [`Plain] otherwise. *)

type ctx
(** The environment a display draws on: an output formatter, a clock, the
    terminal geometry, and whether the output is a terminal. Applications obtain
    one from {!Console_eio.run} and pass it to {!v}; a test can build one with
    pure stubs through {!val-ctx}. *)

val ctx :
  ppf:Format.formatter ->
  now:(unit -> float) ->
  dimensions:(unit -> int * int) ->
  is_tty:bool ->
  unit ->
  ctx
(** [ctx ~ppf ~now ~dimensions ~is_tty ()] assembles a context: [ppf] is where
    frames are written, [now] gives wall-clock seconds for timers and animation,
    [dimensions] is the terminal (width, height) re-read each paint, [is_tty]
    says whether [ppf] is a terminal (resolving [`Auto]). *)

val with_char_width : ctx -> (Uchar.t -> int) -> ctx
(** [with_char_width ctx f] is [ctx] with [f] as its per-code-point cell-width
    hint. The default context uses {!Width.default_char_width}; applications
    that need full Unicode terminal widths can install a provider from their own
    dependency set. *)

val is_tty : ctx -> bool
(** Whether the context's output is a terminal. *)

val dimensions : ctx -> int * int
(** The context's terminal (width, height). *)

val char_width : ctx -> Uchar.t -> int
(** The context's per-code-point cell-width hint (default
    {!Width.default_char_width}). *)

val dumb : unit -> ctx
(** [dumb ()] is a non-interactive context -- standard output, a fixed 80x24
    geometry, a zero clock, [is_tty = false] -- for rendering to a string or a
    test where no real terminal or clock is in play. *)

val v :
  ctx:ctx ->
  ?mode:mode ->
  ?theme:Theme.t ->
  ?palette:color list ->
  ?bar:Line.t ->
  ?width:int ->
  ?height:int ->
  ?header:string ->
  unit ->
  t
(** [v ~ctx ()] starts an empty display drawing on [ctx]. The optional [theme]
    fixes the glyphs (default {!Theme.default}) and the optional bar style is
    the default for its rows (otherwise {!Line.default}). [palette], if
    non-empty, gives each new row a distinct accent by cycling through it (the
    colour is fixed when the row is created, so it is stable across redraws) --
    a row's explicit colour still wins. [width] and [height] pin the terminal
    geometry; left unset they come from [ctx.dimensions] and are
    {e re-read on every paint}, so a terminal resize is handled rather than
    corrupting the display. [height] bounds the live region to that many rows --
    a region that would exceed it keeps the header and the most recent rows and
    collapses the overflow into a marker, so a redraw never scrolls a stale copy
    into the scrollback. Every emitted line, the header included, is truncated
    to [width] so nothing wraps onto a second physical row. [header], if given,
    labels a persistent top line reading [<header> <elapsed> (<done>/<total>)],
    e.g. ["Building 12.3s (8/15)"]. *)

type row
(** A row of a display. A fresh row shows a spinner, its label and a running
    timer; {!set_count} or {!set_bytes} give it a bar (styled per {!Line.t}). *)

val row :
  ?parent:row ->
  ?bar:Line.t ->
  ?color:color ->
  ?log_lines:int ->
  t ->
  string ->
  row
(** [row d label] appends a row to [d], starts its timer and returns its handle.
    [parent] indents it beneath an earlier row, as a sub-step. An optional bar
    style overrides the display's default for this row, and a colour gives the
    row its own accent (its identity). [log_lines] (default [0]) reserves a live
    log tail of that many lines under the row, fed by {!log}. *)

val log : row -> string -> unit
(** [log r s] appends [s] (split on newlines) to [r]'s log tail; only the last
    [log_lines] lines are kept, shown dimmed and indented beneath the row, like
    buildkit's per-step log window. A line whose gap since the previous log line
    is at least a second carries that gap as a trailing [+<elapsed>] (e.g.
    ["# linking  +18.0s"]), so time spent with no output is visible rather than
    silent. On {!finish} the tail is dropped and the row collapses to its
    committed line. With no terminal (or [`Plain] mode) the lines are instead
    printed append-only (with the same gap suffix), so captured logs keep the
    output. No-op on a terminal when the row has no log window. *)

val default_row : ?log_lines:int -> t -> row
(** [default_row d] is [d]'s shared untagged-logs row, labelled ["logs"]:
    created on first call and memoized, so every log line with no destination of
    its own lands in one window. [log_lines] (default [0]) sizes the log window
    when the call creates the row; later calls return the existing row and
    ignore it. *)

val set_label : row -> string -> unit
(** [set_label r s] replaces [r]'s label. *)

val set_bar : row -> Line.t -> unit
(** [set_bar r b] replaces [r]'s row style. *)

val set_count : row -> cur:int -> total:int -> unit
(** [set_count r ~cur ~total] gives [r] a counter of [cur/total] items. *)

val set_bytes : row -> cur:int64 -> total:int64 -> unit
(** [set_bytes r ~cur ~total] gives [r] a byte counter with humanised sizes and,
    if the style has [`Rate], a transfer rate. The rate is a moving average of
    the deltas across calls. *)

val finish : ?live:bool -> ?error:bool -> ?message:string -> row -> unit
(** [finish r] freezes [r]'s timer, marks it done and commits it to the
    terminal's scrollback; [r] is no longer redrawn. [message], if given,
    replaces the label in the committed line. [error] marks the row as failed.

    With [~live:true], the row is marked done but remains in the live tree until
    the display is finished. This is useful for build traces where completed
    sub-steps should stay visible under their parent.

    With [~error:true], the row is recorded as having failed: the header's
    summary marker flips to a red [[x]] once the display is complete, instead of
    the green [[+]]. *)

val remove : row -> unit
(** [remove r] discards [r] from the display entirely: unlike {!finish} it does
    not commit a line to the scrollback, and unlike [finish ~live:true] it does
    not keep the row visible -- the row (and its log tail) just disappear on the
    next redraw. Use it for transient sub-rows that should leave no trace once
    done, e.g. a per-transfer byte-progress row that has reached its total. The
    done/total count is adjusted so a removed row no longer counts. Intended for
    leaf rows; removing a row with live children orphans them. *)

val tick : t -> unit
(** [tick d] redraws the live rows, advancing their timers, with no new event --
    call it on a ~100ms loop to keep timers and spinners moving even while idle.
    No-op in [`Plain] mode. *)

val set_banner : t -> string Anim.t -> unit
(** [set_banner d a] paints [a] above the header as part of the live region,
    sampled at the display's elapsed time on every redraw -- so {!tick} animates
    it (e.g. [set_banner d (Panel.rain banner)] for a Matrix banner over a
    progress display). Each banner line is truncated to the width like the rest
    of the region. No-op in [`Plain] mode. *)

val clear_banner : t -> unit
(** [clear_banner d] removes the banner set by {!set_banner}. *)

val with_output : t -> (unit -> 'a) -> 'a
(** [with_output d f] clears the live region, runs [f] -- whose output then
    lands cleanly in the scrollback above the display -- and redraws. Use it
    around code that prints to the same terminal. In [`Plain] mode it just runs
    [f]. *)

val set_result :
  t -> ?detail:string -> [ `Ok of string | `Fail of string ] -> unit
(** [set_result d result] records the verdict {!finish_all} renders on
    completion -- the banner and the dimmed [detail] paragraph described there.
    The last call wins. An explicit [result] passed to {!finish_all} overrides
    the recorded verdict, its detail included. *)

val finish_all :
  ?result:[ `Ok of string | `Fail of string ] -> ?detail:string -> t -> unit
(** [finish_all d] finishes every still-running row, writes the header's final
    summary and releases the terminal. When [result] is omitted, the verdict
    recorded by {!set_result} (if any) is rendered in its place.

    [result] adds a final verdict banner under the summary: a bold green
    [<ok_marker> <msg>] for [`Ok msg], or a bold red [<fail_marker> <msg>] for
    [`Fail msg] (the markers default to [[OK]] / [[FAIL]]; see {!Theme.v}). This
    is the one-line headline a reader looks for at the end of a run -- the
    bracketed-status idiom of systemd's [[ OK ]] / [[FAILED]] boot lines and the
    pass/fail summary of test runners -- distinct from the live [+]/[x] row
    markers because it is a sentence, not a glyph.

    [detail] is a paragraph (split on newlines) shown dimmed just above the
    verdict -- a reason, a log-path pointer, the failing command -- the
    details-then-headline shape of [pytest] and [cargo]. Under [`Plain] both the
    detail and the verdict are emitted as plain text, so a captured log keeps
    them. *)

val clear : t -> unit
(** [clear d] erases the live region and releases the terminal {e without}
    committing any rows or a summary -- use it to abort a display. No-op in
    [`Plain] mode (nothing was drawn to erase). *)

(** {1 Interleaving foreign output} *)

val suspend : (unit -> 'a) -> 'a
(** [suspend f] clears the active display, runs [f] (which may print to the same
    terminal), then redraws. If no display is active, just calls [f]. *)

val logs_reporter : Logs.reporter -> Logs.reporter
(** [logs_reporter r] wraps [r] so log messages {!suspend} the active display
    around each write.

    A program does not normally call this: {!v} wraps whatever reporter [Logs]
    holds when the display takes the terminal, and restores it on {!clear} or
    {!finish_all}. Call it directly only to wrap a reporter installed after a
    display is already live. *)
