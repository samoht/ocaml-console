(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The rules of cmdliner and fmt: NO_COLOR set and not empty, or a TERM that
   is unset or dumb, turn colour off; otherwise a terminal gets colour. *)
let dumb ~getenv =
  match getenv "TERM" with None | Some ("" | "dumb") -> true | Some _ -> false

let renderer ~getenv ~is_tty =
  match getenv "NO_COLOR" with
  | Some v when v <> "" -> `None
  | _ -> if dumb ~getenv || not is_tty then `None else `Ansi_tty

let depth ~getenv =
  let has ~affix s =
    let n = String.length affix and m = String.length s in
    let rec at i = i + n <= m && (String.sub s i n = affix || at (i + 1)) in
    at 0
  in
  match (getenv "COLORTERM", getenv "TERM") with
  | Some ("truecolor" | "24bit"), _ -> `True_color
  | _, Some term when String.ends_with ~suffix:"-direct" term -> `True_color
  | _, Some term when has ~affix:"256color" term -> `Ansi_256
  | _ -> `Ansi_16
