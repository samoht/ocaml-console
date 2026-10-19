(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type mark =
  [ `Running | `Succeeded | `Failed | `Cancelled | `Info | `Warning | `Error ]

type t = {
  accent : Color.t;
  palette : Color.t list;
  ok : Color.t;
  warn : Color.t;
  fail : Color.t;
  header : Style.t;
  background : Color.t option;
  border : Border.t;
  guide : Guide.t;
  spinner : Spinner.t;
  marks : mark -> string;
}

let accent t = t.accent
let palette t = t.palette
let ok t = t.ok
let warn t = t.warn
let fail t = t.fail
let header t = t.header
let background t = t.background
let border t = t.border
let guide t = t.guide
let spinner t = t.spinner

(* A mark is drawn on one line, as text. *)
let mark t m = Console_text.sanitize ~keep_newlines:false (t.marks m)

let unicode_marks = function
  | `Running -> "\xe2\x80\xa2"
  | `Succeeded -> "\xe2\x9c\x93"
  | `Failed | `Error -> "\xe2\x9c\x97"
  | `Cancelled -> "\xe2\x97\x8b"
  | `Info -> "\xe2\x80\xba"
  | `Warning -> "!"

let ascii_marks = function
  | `Running -> "[-]"
  | `Succeeded -> "[+]"
  | `Failed -> "[x]"
  | `Cancelled -> "[~]"
  | `Info -> "#"
  | `Warning -> "!"
  | `Error -> "x"

let unicode =
  {
    accent = Color.cyan;
    palette = [ Color.cyan ];
    ok = Color.green;
    warn = Color.yellow;
    fail = Color.red;
    header = Style.bold;
    background = None;
    border = Border.rounded;
    guide = Guide.unicode;
    spinner = Spinner.braille;
    marks = unicode_marks;
  }

let v ?(accent = unicode.accent) ?palette ?(ok = unicode.ok)
    ?(warn = unicode.warn) ?(fail = unicode.fail) ?(header = unicode.header)
    ?background ?(border = unicode.border) ?(guide = unicode.guide)
    ?(spinner = unicode.spinner) ?(marks = unicode.marks) () =
  let palette =
    match palette with None | Some [] -> [ accent ] | Some p -> p
  in
  {
    accent;
    palette;
    ok;
    warn;
    fail;
    header;
    background;
    border;
    guide;
    spinner;
    marks;
  }

let amber = Color.rgb 0xff 0xb0 0x00

let dos =
  v ~accent:amber ~ok:amber ~warn:amber ~fail:(Color.rgb 0xff 0x55 0x55)
    ~background:Color.black ~border:Border.ascii ~guide:Guide.ascii
    ~spinner:Spinner.ascii ~marks:ascii_marks ()

let matrix_green = Color.rgb 0x33 0xff 0x33

let matrix =
  v ~accent:matrix_green ~ok:matrix_green ~warn:(Color.rgb 0xcc 0xff 0x66)
    ~fail:(Color.rgb 0xff 0x33 0x33) ~background:Color.black
    ~border:Border.single ()

let rainbow =
  v ~accent:Color.magenta
    ~palette:
      [
        Color.red;
        Color.yellow;
        Color.green;
        Color.cyan;
        Color.blue;
        Color.magenta;
      ]
    ()

(* The colours of each scheme are taken from its upstream source, named
   beside it, and degrade to 256 or 16 colours as a terminal's depth asks. *)

let scheme ~accent ~ok ~warn ~fail ~background =
  v ~accent:(Color.hex accent) ~ok:(Color.hex ok) ~warn:(Color.hex warn)
    ~fail:(Color.hex fail) ~background:(Color.hex background) ()

(* Dracula, by Zeno Rocha (MIT): draculatheme.com/spec. *)
let dracula =
  scheme ~accent:"#bd93f9" ~ok:"#50fa7b" ~warn:"#f1fa8c" ~fail:"#ff5555"
    ~background:"#282a36"

(* Nord, by Sven Greb (MIT): nordtheme/nord src/nord.css. *)
let nord =
  scheme ~accent:"#88c0d0" ~ok:"#a3be8c" ~warn:"#ebcb8b" ~fail:"#bf616a"
    ~background:"#2e3440"

(* Solarized, by Ethan Schoonover (MIT): altercation/solarized. *)
let solarized_dark =
  scheme ~accent:"#268bd2" ~ok:"#859900" ~warn:"#b58900" ~fail:"#dc322f"
    ~background:"#002b36"

(* Solarized, by Ethan Schoonover (MIT): altercation/solarized. *)
let solarized_light =
  scheme ~accent:"#268bd2" ~ok:"#859900" ~warn:"#b58900" ~fail:"#dc322f"
    ~background:"#fdf6e3"

(* gruvbox, by Pavel Pertsev (MIT/X11): morhetz/gruvbox colors/gruvbox.vim. *)
let gruvbox_dark =
  scheme ~accent:"#fe8019" ~ok:"#b8bb26" ~warn:"#fabd2f" ~fail:"#fb4934"
    ~background:"#282828"

(* gruvbox, by Pavel Pertsev (MIT/X11): morhetz/gruvbox colors/gruvbox.vim. *)
let gruvbox_light =
  scheme ~accent:"#af3a03" ~ok:"#79740e" ~warn:"#b57614" ~fail:"#9d0006"
    ~background:"#fbf1c7"

(* Catppuccin, by the Catppuccin organisation (MIT): catppuccin/palette palette.json 1.8.0. *)
let catppuccin_latte =
  scheme ~accent:"#8839ef" ~ok:"#40a02b" ~warn:"#df8e1d" ~fail:"#d20f39"
    ~background:"#eff1f5"

(* Catppuccin, by the Catppuccin organisation (MIT): catppuccin/palette palette.json 1.8.0. *)
let catppuccin_mocha =
  scheme ~accent:"#cba6f7" ~ok:"#a6e3a1" ~warn:"#f9e2af" ~fail:"#f38ba8"
    ~background:"#1e1e2e"

(* Tokyo Night, by enkia (MIT), night palette of folke/tokyonight.nvim (Apache-2.0). *)
let tokyo_night =
  scheme ~accent:"#7aa2f7" ~ok:"#9ece6a" ~warn:"#e0af68" ~fail:"#f7768e"
    ~background:"#1a1b26"

(* One Dark, by GitHub (MIT): atom/atom packages/one-dark-syntax. *)
let one_dark =
  scheme ~accent:"#528bff" ~ok:"#98c379" ~warn:"#e5c07b" ~fail:"#e06c75"
    ~background:"#282c34"

(* Monokai, by Wimer Hazenberg (no licence stated): the 2007 Monokai.tmTheme. *)
let monokai =
  scheme ~accent:"#66d9ef" ~ok:"#a6e22e" ~warn:"#e6db74" ~fail:"#f92672"
    ~background:"#272822"

(* GitHub Dark, by GitHub (MIT): primer/github-vscode-theme. *)
let github_dark =
  scheme ~accent:"#2f81f7" ~ok:"#3fb950" ~warn:"#d29922" ~fail:"#ff7b72"
    ~background:"#0d1117"

(* GitHub Light, by GitHub (MIT): primer/github-vscode-theme. *)
let github_light =
  scheme ~accent:"#0969da" ~ok:"#116329" ~warn:"#4d2d00" ~fail:"#cf222e"
    ~background:"#ffffff"

let pp ppf t =
  Fmt.pf ppf "@[<1>(theme@ accent=%a@ ok=%a@ warn=%a@ fail=%a@ marks=%s)@]"
    Color.pp t.accent Color.pp t.ok Color.pp t.warn Color.pp t.fail
    (String.concat " "
       (List.map (mark t)
          [ `Running; `Succeeded; `Failed; `Cancelled; `Info; `Warning; `Error ]))
