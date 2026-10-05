(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type style = Theme.bar
type color = Color.t

(* Eighth-of-a-cell glyphs for the smooth bar. *)
let eighths = [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]

let rainbow_palette =
  [|
    Color.red; Color.yellow; Color.green; Color.cyan; Color.blue; Color.magenta;
  |]

(* The SGR a bar adds, dropped when the caller renders unstyled: the glyphs
   carry the shape and the colour only reinforces it, so a bar without colour
   is the same bar. *)
let code ~styled sequence = if styled then sequence else ""

(* A blocky DOS-style bar: dim [ ] brackets, full-block fill, dim shade
   track. *)
let blocky ~styled ~color ~width ~pct =
  let full = width * pct / 100 in
  let empty = max 0 (width - full) in
  let fill = String.concat "" (List.init full (fun _ -> "█")) in
  let track = String.concat "" (List.init empty (fun _ -> "░")) in
  String.concat ""
    [
      code ~styled Ansi.dim;
      "[";
      code ~styled Ansi.reset_code;
      code ~styled (Ansi.color_code color);
      fill;
      code ~styled Ansi.reset_code;
      code ~styled Ansi.dim;
      track;
      "]";
      code ~styled Ansi.reset_code;
    ]

(* A smooth bar with sub-cell eighth fills and thin brackets. *)
let smooth ~styled ~color ~width ~pct =
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
      code ~styled Ansi.dim;
      "▕";
      code ~styled Ansi.reset_code;
      code ~styled (Ansi.color_code color);
      fill;
      code ~styled Ansi.reset_code;
      code ~styled Ansi.dim;
      String.make empty ' ';
      "▏";
      code ~styled Ansi.reset_code;
    ]

(* A rainbow bar: each filled cell steps through the spectrum. *)
let rainbow ~styled ~color:_ ~width ~pct =
  let full = width * pct / 100 in
  let empty = max 0 (width - full) in
  let buf = Buffer.create (width * 8) in
  Buffer.add_string buf
    (code ~styled Ansi.dim ^ "[" ^ code ~styled Ansi.reset_code);
  for i = 0 to full - 1 do
    let c = rainbow_palette.(i mod Array.length rainbow_palette) in
    Buffer.add_string buf
      (code ~styled (Ansi.color_code c) ^ "█" ^ code ~styled Ansi.reset_code)
  done;
  Buffer.add_string buf (code ~styled Ansi.dim);
  for _ = 1 to empty do
    Buffer.add_string buf "░"
  done;
  Buffer.add_string buf ("]" ^ code ~styled Ansi.reset_code);
  Buffer.contents buf

let draw_style :
    style -> styled:bool -> color:color -> width:int -> pct:int -> string =
  function
  | `Blocky -> blocky
  | `Smooth -> smooth
  | `Rainbow -> rainbow

(* An explicit argument wins, else the theme's, else a modern default. *)
let resolve ?theme ?style ?color () =
  let style =
    match style with
    | Some s -> s
    | None -> ( match theme with Some t -> Theme.bar t | None -> `Smooth)
  in
  let color =
    match color with
    | Some c -> c
    | None -> (
        match theme with Some t -> Theme.accent t | None -> Color.cyan)
  in
  (style, color)

let render ?theme ?style ?color ?(styled = true) ~width ~pct () =
  if width < 0 then invalid_arg "Console.Bar.render: negative width";
  let style, color = resolve ?theme ?style ?color () in
  let pct = max 0 (min 100 pct) in
  draw_style style ~styled ~color ~width ~pct

let at ?theme ?style ?color ?(styled = true) ~width ~elapsed () =
  if width < 0 then invalid_arg "Console.Bar.at: negative width";
  let style, color = resolve ?theme ?style ?color () in
  (* Sweep the fill up and back over a two-second period: a triangle wave on
     [elapsed] mapped to a percentage. *)
  let period = 2.0 in
  let phase =
    if (not (Float.is_finite elapsed)) || elapsed <= 0. then 0.
    else Float.rem (elapsed /. period) 1.0
  in
  let tri = if phase < 0.5 then phase *. 2. else (1. -. phase) *. 2. in
  draw_style style ~styled ~color ~width ~pct:(int_of_float (tri *. 100.))

let anim ?theme ?style ?color ?styled ~width () =
  Anim.v (fun ~elapsed -> at ?theme ?style ?color ?styled ~width ~elapsed ())

(* {1 Bars as spans} *)

let repeat n glyph = String.concat "" (List.init (max 0 n) (fun _ -> glyph))
let default_style = Style.fg Color.cyan

(* [n] cells of [glyph] in [style]; no cell adds nothing, not even a style. *)
let run style n glyph =
  if n <= 0 then Span.empty else Span.styled style (repeat n glyph)

let framed ~track ~brackets body =
  if brackets then
    Span.concat [ Span.styled track "▕"; body; Span.styled track "▏" ]
  else body

let check_width fn width =
  if width < 0 then invalid_arg ("Console.Bar." ^ fn ^ ": negative width")

let v ?(style = default_style) ?(track = Style.faint) ?(brackets = false) ~width
    frac =
  check_width "v" width;
  let frac =
    if Float.is_nan frac then 0. else Float.max 0. (Float.min 1. frac)
  in
  let e = int_of_float (Float.round (frac *. float_of_int (width * 8))) in
  let full = e / 8 and part = e mod 8 in
  let fill =
    repeat full eighths.(8) ^ if part > 0 then eighths.(part) else ""
  in
  let used = full + if part > 0 then 1 else 0 in
  framed ~track ~brackets
    (Span.concat
       [
         (if fill = "" then Span.empty else Span.styled style fill);
         run track (width - used) "░";
       ])

let cells ~width ~scale x =
  if not (x > 0. && scale > 0.) then 0
  else
    let r = Float.min 1. (x /. scale) in
    max 1 (min width (int_of_float (Float.round (r *. float_of_int width))))

let spread ?(style = default_style) ?(track = Style.faint) ?(brackets = false)
    ~width ~scale median p95 =
  check_width "spread" width;
  let m = cells ~width ~scale median in
  let p = max m (cells ~width ~scale p95) in
  framed ~track ~brackets
    (Span.concat
       [ run style m "█"; run style (p - m) "▒"; run track (width - p) "░" ])

(* Largest remainder over integers: part [i] of [total] takes
   [v * width / total] cells, and the parts with the largest
   [v * width mod total], the earlier first on a tie, one more each until the
   cells add up to [width]. *)
let shares ~width counts =
  let total = List.fold_left ( + ) 0 counts in
  let floors = List.map (fun v -> v * width / total) counts in
  let left = width - List.fold_left ( + ) 0 floors in
  let extra =
    List.mapi (fun i v -> (i, v * width mod total)) counts
    |> List.stable_sort (fun (_, a) (_, b) -> Int.compare b a)
    |> List.filteri (fun k _ -> k < left)
    |> List.map fst
  in
  List.mapi (fun i f -> if List.mem i extra then f + 1 else f) floors

let stacked ?(track = Style.faint) ?(brackets = false) ~width parts =
  check_width "stacked" width;
  if List.exists (fun (n, _) -> n < 0) parts then
    invalid_arg "Console.Bar.stacked: negative count";
  let body =
    if List.for_all (fun (n, _) -> n = 0) parts then run track width "░"
    else
      Span.concat
        (List.map2
           (fun n (_, style) -> run style n "█")
           (shares ~width (List.map fst parts))
           parts)
  in
  framed ~track ~brackets body
