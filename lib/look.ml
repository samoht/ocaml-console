(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { theme : Theme.t; utf_8 : bool; live : bool }

let v ?(live = false) ppf =
  { theme = Theme.get ppf; utf_8 = Fmt.utf_8 ppf; live }

let default = { theme = Theme.default; utf_8 = true; live = false }
let equal a b = a.theme == b.theme && a.utf_8 = b.utf_8 && a.live = b.live

let pp ppf t =
  Fmt.pf ppf "@[<1>(look@ %a@ utf_8=%b@ live=%b)@]" Theme.pp t.theme t.utf_8
    t.live
