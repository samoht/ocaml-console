(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Prompt = Prompt

let clock = Virtual_clock.clock

let domain_mgr d =
  if Cast.deterministic () then Eio_mock.Domain_manager.create ()
  else (d :> Eio.Domain_manager.ty Eio.Resource.t)

let run ~clock ?ppf ?progress ?layout ?label ?view ?keep_ended ?commit ?plain
    ?delay f =
  Backend.run
    ~clock:(clock :> Eio.Time.Mono.ty Eio.Resource.t)
    ?ppf ?progress ?layout ?label ?view ?keep_ended ?commit ?plain ?delay f

let with_output = Backend.with_output
let print = Backend.print
let redraw = Backend.redraw
