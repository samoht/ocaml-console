(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Prompt = Prompt

let clock = Virtual_clock.clock

let run ~clock ?ppf ?progress ?layout ?label ?view ?keep_ended ?commit ?plain
    ?delay f =
  Backend.run
    ~clock:(clock :> Eio.Time.Mono.ty Eio.Resource.t)
    ?ppf ?progress ?layout ?label ?view ?keep_ended ?commit ?plain ?delay f

let with_output = Backend.with_output
let print = Backend.print
let redraw = Backend.redraw
