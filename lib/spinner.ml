(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = string array

let pp ppf t = Fmt.pf ppf "spinner[%d frames]" (Array.length t)

let v frames =
  if Array.length frames = 0 then invalid_arg "Console.Spinner.v: no frames";
  Array.copy frames

let frames t = Array.copy t

let braille = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let ascii = [| "|"; "/"; "-"; "\\" |]

let anim ?(fps = 10.) t =
  if (not Float.(is_finite fps)) || fps <= 0. then
    invalid_arg "Console.Spinner.anim: fps must be positive";
  let n = Array.length t in
  let cycle = Float.of_int n /. fps in
  Anim.v (fun ~elapsed ->
      if not (Float.is_finite elapsed) then t.(0)
      else t.(int_of_float (Float.rem elapsed cycle *. fps) mod n))
