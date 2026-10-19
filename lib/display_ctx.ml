(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  ppf : Format.formatter;
  now : unit -> float;
  wait : float -> unit;
  dimensions : unit -> int * int;
  live : bool;
  depth : Color.depth;
}

let v ?(depth = `True_color) ~ppf ~now ~wait ~dimensions ~live () =
  { ppf; now; wait; dimensions; live; depth }

let pp ppf t =
  let width, height = t.dimensions () in
  Fmt.pf ppf "<ctx %dx%d%s>" width height (if t.live then " tty" else "")
