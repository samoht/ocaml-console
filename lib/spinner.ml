(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string array

let pp ppf t = Fmt.pf ppf "spinner[%d frames]" (Array.length t)

let v frames =
  if Array.length frames = 0 then invalid_arg "Console.Spinner.v: no frames";
  Array.copy frames

let braille = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let ascii = [| "|"; "/"; "-"; "\\" |]

let frame ?(fps = 10.) t ~elapsed =
  if (not (Float.is_finite fps)) || fps <= 0. then
    invalid_arg "Console.Spinner.frame: fps must be positive";
  let n = Array.length t in
  let x = elapsed *. fps in
  if (not (Float.is_finite x)) || x <= 0. then t.(0)
  else t.(Float.to_int (Float.rem x (Float.of_int n)))
