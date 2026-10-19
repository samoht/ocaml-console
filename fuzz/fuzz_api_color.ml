open Alcobar
open Console

let test s n f =
  let c = Color.rgb (n land 255) ((n lsr 8) land 255) ((n lsr 16) land 255) in
  check (Color.to_rgb c = (n land 255, (n lsr 8) land 255, (n lsr 16) land 255));
  let names =
    [
      Color.black;
      Color.red;
      Color.green;
      Color.yellow;
      Color.blue;
      Color.magenta;
      Color.cyan;
      Color.white;
      Color.bright_black;
      Color.bright_red;
      Color.bright_green;
      Color.bright_yellow;
      Color.bright_blue;
      Color.bright_magenta;
      Color.bright_cyan;
      Color.bright_white;
    ]
  in
  let tags =
    [
      `Black;
      `Red;
      `Green;
      `Yellow;
      `Blue;
      `Magenta;
      `Cyan;
      `White;
      `Bright_black;
      `Bright_red;
      `Bright_green;
      `Bright_yellow;
      `Bright_blue;
      `Bright_magenta;
      `Bright_cyan;
      `Bright_white;
    ]
  in
  List.iter2
    (fun tag c ->
      ignore tag;
      ignore (Fmt.str "%a" Color.pp c);
      ignore (Color.to_rgb c))
    tags names;
  (match Color.of_hex s with
  | Ok v -> check (Color.equal v (Color.hex s))
  | Error _ ->
      check
        (try
           ignore (Color.hex s);
           false
         with Invalid_argument _ -> true));
  let hex =
    Printf.sprintf "#%02x%02x%02x" (n land 255)
      ((n lsr 8) land 255)
      ((n lsr 16) land 255)
  in
  check (Color.equal c (Color.hex hex));
  if n < 0 || n > 255 then
    check
      (try
         ignore (Color.palette n);
         false
       with Invalid_argument _ -> true)
  else ignore (Color.to_rgb (Color.palette n));
  List.iter
    (fun d ->
      let c' = Color.downsample d c in
      check (Color.equal c' (Color.downsample d c')))
    [ `Ansi_16; `Ansi_256; `True_color ];
  check (Color.equal c (Color.downsample `True_color c));
  let contains needle haystack =
    let rec scan i =
      i + String.length needle <= String.length haystack
      && (String.sub haystack i (String.length needle) = needle || scan (i + 1))
    in
    scan 0
  in
  List.iter
    (fun (term, colorterm) ->
      let expected =
        if
          colorterm = "truecolor" || colorterm = "24bit"
          || String.ends_with ~suffix:"-direct" term
        then `True_color
        else if contains "256color" term then `Ansi_256
        else `Ansi_16
      in
      check
        (Color.depth_of_env (function
           | "TERM" -> Some term
           | "COLORTERM" -> Some colorterm
           | _ -> None)
        = expected))
    [
      (s, s);
      (s, "truecolor");
      (s, "24bit");
      (s ^ "-direct", s);
      ("xterm-256color", s);
      ("dumb", "");
    ];
  let range c =
    let r, g, b = Color.to_rgb c in
    check (r >= 0 && r <= 255 && g >= 0 && g <= 255 && b >= 0 && b <= 255)
  in
  range (Color.blend c (Color.rgb 255 255 255) f);
  range (Color.scale f c);
  if Float.is_finite f then begin
    let p = Float.min 1. (Float.max 0. f) in
    let r, g, b = Color.to_rgb c in
    let mix a = a + int_of_float (Float.round (p *. float_of_int (255 - a))) in
    check
      (Color.to_rgb (Color.blend c (Color.rgb 255 255 255) f)
      = (mix r, mix g, mix b));
    let scale a =
      int_of_float
        (Float.min 255. (Float.max 0. (Float.round (f *. float_of_int a))))
    in
    check (Color.to_rgb (Color.scale f c) = (scale r, scale g, scale b))
  end

let suite =
  ( "api.color",
    [ test_case "colour constructors and laws" [ bytes; int; float ] test ] )
