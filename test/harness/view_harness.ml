(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Console

let at ?(theme = Theme.default) ?(utf_8 = true) ?(live = true) ?width ~time v =
  let look = { Look.theme; utf_8; live } in
  View_core.to_string
    (View_core.paint look (View_core.resolve (Ctx.v ?width ~time look) v))

let laid ?(live = false) ?(tasks = false) () =
  let calls = ref 0 in
  let v =
    View_core.deferred
      ~kind:(fun _ -> { View_core.static with live; tasks })
      (fun c ->
        incr calls;
        View_core.text (String.make (Option.value (Ctx.width c) ~default:3) 'x'))
  in
  (v, calls)

let capture f =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  f ppf;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let printed ?theme ?utf_8 ?margin ?depth v =
  capture @@ fun ppf ->
  Option.iter (Theme.set ppf) theme;
  Option.iter (Fmt.set_utf_8 ppf) utf_8;
  Option.iter (Format.pp_set_margin ppf) margin;
  Option.iter
    (fun depth ->
      Fmt.set_style_renderer ppf `Ansi_tty;
      Color.set_depth ppf depth)
    depth;
  View.pp ppf v

let sgr style =
  let s = View.to_string ~styled:true (View.styled style (View.text "x")) in
  String.sub s 0 (String.index s 'x')

let fill glyph grow =
  View.fit ~grow (fun room ->
      View.text (String.make (Option.value ~default:1 (Room.width room)) glyph))

let frame glyph =
  Border.v
    Border.
      {
        top_left = glyph;
        top = glyph;
        top_right = glyph;
        left = glyph;
        right = glyph;
        bottom_left = glyph;
        bottom = glyph;
        bottom_right = glyph;
        vertical = glyph;
        horizontal = glyph;
        cross = glyph;
        top_cross = glyph;
        bottom_cross = glyph;
        left_cross = glyph;
        right_cross = glyph;
      }

let small_table ~border =
  Table.view ~border
    [ Table.column (View.text "A"); Table.column (View.text "B") ]
    [ [ View.text "x"; View.text "y" ] ]
