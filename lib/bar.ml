(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type style = Theme.bar
type color = Theme.color

(* Eighth-of-a-cell glyphs for the smooth bar. *)
let eighths = [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]
let rainbow_palette = [| `Red; `Yellow; `Green; `Cyan; `Blue; `Magenta |]

(* A blocky DOS-style bar: dim [ ] brackets, full-block fill, dim shade
   track. *)
let blocky ~color ~width ~pct =
  let full = width * pct / 100 in
  let empty = max 0 (width - full) in
  let fill = String.concat "" (List.init full (fun _ -> "█")) in
  let track = String.concat "" (List.init empty (fun _ -> "░")) in
  String.concat ""
    [
      Ansi.dim;
      "[";
      Ansi.reset_code;
      Ansi.color_code color;
      fill;
      Ansi.reset_code;
      Ansi.dim;
      track;
      "]";
      Ansi.reset_code;
    ]

(* A smooth bar with sub-cell eighth fills and thin brackets. *)
let smooth ~color ~width ~pct =
  let total = width * 8 in
  let filled = total * pct / 100 in
  let full = filled / 8 in
  let part = filled mod 8 in
  let empty = max 0 (width - full - if part > 0 then 1 else 0) in
  let fill =
    String.concat "" (List.init full (fun _ -> eighths.(8)))
    ^ if part > 0 then eighths.(part) else ""
  in
  String.concat ""
    [
      Ansi.dim;
      "▕";
      Ansi.reset_code;
      Ansi.color_code color;
      fill;
      Ansi.reset_code;
      Ansi.dim;
      String.make empty ' ';
      "▏";
      Ansi.reset_code;
    ]

(* A rainbow bar: each filled cell steps through the spectrum. *)
let rainbow ~color:_ ~width ~pct =
  let full = width * pct / 100 in
  let empty = max 0 (width - full) in
  let buf = Buffer.create (width * 8) in
  Buffer.add_string buf (Ansi.dim ^ "[" ^ Ansi.reset_code);
  for i = 0 to full - 1 do
    let c = rainbow_palette.(i mod Array.length rainbow_palette) in
    Buffer.add_string buf (Ansi.color_code c ^ "█" ^ Ansi.reset_code)
  done;
  Buffer.add_string buf Ansi.dim;
  for _ = 1 to empty do
    Buffer.add_string buf "░"
  done;
  Buffer.add_string buf ("]" ^ Ansi.reset_code);
  Buffer.contents buf

let draw_style : style -> color:color -> width:int -> pct:int -> string =
  function
  | `Blocky -> blocky
  | `Smooth -> smooth
  | `Rainbow -> rainbow

(* An explicit argument wins, else the theme's, else a modern default. *)
let resolve ?theme ?style ?color () =
  let style =
    match style with
    | Some s -> s
    | None -> ( match theme with Some t -> t.Theme.bar | None -> `Smooth)
  in
  let color =
    match color with
    | Some c -> c
    | None -> ( match theme with Some t -> t.Theme.accent | None -> `Cyan)
  in
  (style, color)

let render ?theme ?style ?color ~width ~pct () =
  let style, color = resolve ?theme ?style ?color () in
  let pct = max 0 (min 100 pct) in
  draw_style style ~color ~width ~pct

let at ?theme ?style ?color ~width ~elapsed () =
  let style, color = resolve ?theme ?style ?color () in
  (* Sweep the fill up and back over a two-second period: a triangle wave on
     [elapsed] mapped to a percentage. *)
  let period = 2.0 in
  let phase = if elapsed <= 0. then 0. else Float.rem (elapsed /. period) 1.0 in
  let tri = if phase < 0.5 then phase *. 2. else (1. -. phase) *. 2. in
  draw_style style ~color ~width ~pct:(int_of_float (tri *. 100.))

let anim ?theme ?style ?color ~width () =
  Anim.v (fun ~elapsed -> at ?theme ?style ?color ~width ~elapsed ())
