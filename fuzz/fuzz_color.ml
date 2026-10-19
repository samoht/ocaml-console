(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Alcobar
open Console

let in_range (r, g, b) =
  let ok v = v >= 0 && v <= 255 in
  ok r && ok g && ok b

(* [rgb] accepts exactly the components in [0;255], and [to_rgb] gives them
   back. *)
let test_rgb r g b =
  let valid = in_range (r, g, b) in
  match Color.rgb r g b with
  | c ->
      if not valid then fail "out of range accepted";
      check (Color.to_rgb c = (r, g, b))
  | exception Invalid_argument _ -> if valid then fail "in range refused"

(* [palette] accepts exactly [0;255]. *)
let test_palette n =
  match Color.palette n with
  | c ->
      if n < 0 || n > 255 then fail "out of range accepted";
      check (in_range (Color.to_rgb c))
  | exception Invalid_argument _ -> if n >= 0 && n <= 255 then fail "refused"

(* [of_hex] and [hex] agree, and a colour printed as hex reads back. *)
let test_hex s =
  (match Color.of_hex s with
  | Ok c -> check (Color.equal c (Color.hex s))
  | Error _ -> (
      match Color.hex s with
      | _ -> fail "hex accepted what of_hex refused"
      | exception Invalid_argument _ -> ()));
  let r, g, b =
    ( Char.code (String.get (s ^ "abc") 0),
      Char.code (String.get (s ^ "abc") 1),
      7 )
  in
  let c = Color.rgb r g b in
  match Fmt.kstr Color.of_hex "%a" Color.pp c with
  | Ok c' -> check (Color.equal c c')
  | Error _ -> fail "printed colour does not read back"

(* [blend] and [scale] stay in range, whatever the factor; [blend c0 c1 0] is
   [c0] and [blend c0 c1 1] is [c1]. *)
let test_blend_scale a b f =
  let c0 = Color.palette (a land 255) and c1 = Color.palette (b land 255) in
  check (in_range (Color.to_rgb (Color.blend c0 c1 f)));
  check (in_range (Color.to_rgb (Color.scale f c0)));
  check (Color.to_rgb (Color.blend c0 c1 0.) = Color.to_rgb c0);
  check (Color.to_rgb (Color.blend c0 c1 1.) = Color.to_rgb c1);
  if Float.is_finite f && f > 1e6 then
    List.iter
      (fun v -> check (v = 0 || v = 255))
      (let r, g, b = Color.to_rgb (Color.scale f c0) in
       [ r; g; b ])

(* [downsample] keeps a colour at [`True_color], gives a palette entry at
   [`Ansi_256] and a named colour at [`Ansi_16], and is idempotent. *)
let test_downsample r g b =
  let c = Color.rgb (r land 255) (g land 255) (b land 255) in
  check (Color.equal c (Color.downsample `True_color c));
  List.iter
    (fun d ->
      let once = Color.downsample d c in
      check (Color.equal once (Color.downsample d once));
      let code =
        let s = Span.to_string ~styled:true (Span.styled (Style.fg once) "x") in
        String.sub s 2 (String.index s 'm' - 2)
      in
      match d with
      | `Ansi_256 -> check (String.starts_with ~prefix:"38;5;" code)
      | `Ansi_16 -> check (not (String.contains code ';'))
      | `True_color -> ())
    [ `Ansi_256; `Ansi_16 ]

(* A formatter's depth is the one last set on it. *)
let test_depth n =
  let ppf = Format.formatter_of_buffer (Buffer.create 1) in
  check (Color.depth ppf = `True_color);
  let d =
    match n mod 3 with 0 -> `Ansi_16 | 1 -> `Ansi_256 | _ -> `True_color
  in
  Color.set_depth ppf d;
  Fmt.set_style_renderer ppf `Ansi_tty;
  check (Color.depth ppf = d)

let suite =
  ( "color",
    [
      test_case "rgb range" [ int; int; int ] test_rgb;
      test_case "palette range" [ int ] test_palette;
      test_case "hex" [ bytes ] test_hex;
      test_case "blend and scale stay in range" [ int; int; float ]
        test_blend_scale;
      test_case "downsample" [ int; int; int ] test_downsample;
      test_case "formatter depth" [ int ] test_depth;
    ] )
