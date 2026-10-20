(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Styled text.

    A span is text in one or more {!Style.t}s, parts of it possibly a link. It
    is the text every widget lays out.

    A span only holds text that prints: the constructors replace each control
    character with a space, a line end ({!Console_text.sanitize}'s) with a line
    feed and each malformed UTF-8 sequence with U+FFFD, and remove the
    characters that reorder or hide the text around them (the bidirectional
    controls), so no span can move the cursor, change the colours of what
    follows it or show its text in another order. A line feed starts a new line,
    and the functions below work line by line.

    Widths are counted in terminal cells, as a terminal draws the text: a wide
    character, such as a Chinese one or an emoji, takes two cells and an accent
    added to a letter takes none, even when the accent is in another style than
    the letter. *)

(** {1:spans Spans} *)

type t = Span_core.t
(** The type for spans. *)

val empty : t
(** [empty] is the span with no text. *)

val text : string -> t
(** [text s] is [s], unstyled. *)

val styled : Style.t -> string -> t
(** [styled style s] is [s] drawn in [style]. *)

val link : ?style:Style.t -> uri:string -> string -> t
(** [link ~style ~uri label] is [label] drawn in [style] (defaults to
    {!Style.none}) as a link to [uri], which a terminal that supports links
    opens on a click. Printed without colours, it is [label] alone. Each byte of
    [uri] that a URI may not hold
    ({{:https://www.rfc-editor.org/rfc/rfc3986#section-2}RFC 3986}), such as a
    control character, a space or a byte of a non-ASCII character, is written
    percent-encoded, and the other bytes, escapes included, as they are. An
    empty [uri] links nowhere: the span is [label] in [style]. *)

val role : Theme.role -> string -> t
(** [role r s] is [s] in the part [r] of the theme of the formatter it is
    printed on ({!Theme.get}), taken when it is printed: [role `Ok "passed"] is
    green in {!Theme.default} and Nord's green in {!Theme.nord}. It is as wide
    as [s], printed in colours at the formatter's {!Color.val-depth} and as the
    text of [s] without colour; {!to_string} styles it in {!Theme.default}.
    {!with_style} adds a style under it, as
    [with_style Style.bold (role `Ok "passed")] makes it bold. *)

val gradient : ?colors:Color.t list -> string -> t
(** [gradient ~colors s] is [s] in colours graded along each of its lines: each
    grapheme cluster takes the {!Color.blend} of the two stops around its place,
    the stops spread evenly from the line's first cluster, in the first stop, to
    its last, in the last one. The stops are [colors] (defaults to the
    {!Theme.palette} of the theme of the formatter it is printed on, as is
    [[]]). It is as wide as [s], printed in colours at the formatter's
    {!Color.val-depth} and as the text of [s] without colour. *)

val ( ++ ) : t -> t -> t
(** [s0 ++ s1] is [s0] followed by [s1]. *)

val concat : ?sep:t -> t list -> t
(** [concat ~sep spans] is [spans] in order, [sep] (defaults to {!empty})
    between each two. *)

val with_style : Style.t -> t -> t
(** [with_style base s] is [s] with [base] under each of its styles: each part
    of [s] in [style] is in [Style.(base + style)]. *)

(** {1:measuring Measuring and fitting} *)

type align = [ `Left | `Center | `Right ]
(** The type for the places of a span in a room wider than it: at its left edge,
    in its middle or at its right edge, as a {!Table} column or a {!Rule} label
    sits. *)

val width : t -> int
(** [width s] is the number of cells the widest line of [s] takes. The invariant
    [width (s0 ++ s1) <= width s0 + width s1] holds. *)

val truncate : int -> t -> t
(** [truncate w s] is [s] with each line wider than [w] cells cut between two
    characters and ended with an ellipsis ["…"], within [w] cells. Lines that
    fit are kept as they are, and so are the styles and links of what is kept.
    It is {!empty} if [w] is not positive. The invariants
    [width (truncate w s) <= max 0 w] and [truncate w s = s] when [width s <= w]
    hold. *)

val wrap : int -> t -> t list
(** [wrap w s] is the lines of [s], each broken into lines of at most [w] cells.
    A line breaks at a run of spaces, which the break removes, or after a slash;
    the spaces inside a line and its leading ones are kept. A word wider than
    [w] goes on over the next lines, broken where the room ends. A character
    wider than [w] (a wide one when [w] is [1], or one with a spacing mark when
    [w] is its width less one) cannot be drawn and is left out. The result is
    never empty, and every other non-space character of [s] is in it, in order,
    styles and links kept.

    @raise Invalid_argument if [w] is not positive. *)

(** {1:fmt Formatting} *)

val pp : t Fmt.t
(** [pp] formats a span with its styles and links iff the formatter's style
    renderer ({!Fmt.style_renderer}) is [`Ansi_tty], its colours as the
    formatter's {!Color.val-depth} shows them. Each style that is turned on is
    turned off after the text it covers, and before each line feed. A part drawn
    when printed, such as a [Bar.to_span], is drawn in the formatter's theme
    ({!Theme.get}) and {!Fmt.utf_8}.

    The formatter's margin does not apply: each line is printed whole, and
    {!truncate} or {!wrap} fit a span to a width first. A line feed is printed
    as it is, not as a break of the enclosing box, so the box's indentation does
    not apply to the next line; print a span of several lines on lines of its
    own, as ["%a@."] does. *)

val to_string : ?styled:bool -> t -> string
(** [to_string s] is [s] with its styles and links iff [styled] (defaults to
    [false]), in 24-bit colour, a part drawn when printed (such as a
    [Bar.to_span]) drawn in {!Theme.default} in UTF-8. *)
