(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { theme : Theme.t; utf_8 : bool; elapsed : float option }

let v ppf = { theme = Theme.get ppf; utf_8 = Fmt.utf_8 ppf; elapsed = None }
let default = { theme = Theme.default; utf_8 = true; elapsed = None }

let pp ppf t =
  Fmt.pf ppf "@[<1>(look@ %a@ utf_8=%b@ elapsed=%a)@]" Theme.pp t.theme t.utf_8
    Fmt.(option ~none:(any "none") float)
    t.elapsed
