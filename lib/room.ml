(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = Ctx.t

let width = Ctx.width
let height = Ctx.height
let time = Ctx.time

let pp ppf r =
  let bound = Fmt.(option ~none:(any "unbounded") int) in
  Fmt.pf ppf "@[<h>width %a, height %a, time %g@]" bound (width r) bound
    (height r) (time r)

let theme r = (Ctx.look r).theme
let utf_8 r = (Ctx.look r).utf_8
