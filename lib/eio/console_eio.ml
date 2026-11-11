(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Prompt = Prompt

let clock = Virtual_clock.clock

(* The domain manager of deterministic mode: it runs a new domain as a plain
   function call. *)

module Fibers = struct
  type t = unit

  let run () fn = fn ~cancelled:(fst (Eio.Promise.create ()))
  let run_raw () fn = fn ()
end

let fibers = Eio.Resource.T ((), Eio.Domain_manager.Pi.mgr (module Fibers))

let domain_mgr d =
  if Cast.deterministic () then fibers
  else (d :> Eio.Domain_manager.ty Eio.Resource.t)

let run ~clock ?ppf ?progress ?layout ?label ?view ?keep_ended ?commit ?plain
    ?delay f =
  Eio.Fiber.with_binding Backend.clock
    (clock :> Eio.Time.Mono.ty Eio.Resource.t)
    (fun () ->
      Backend.run ?ppf ?progress ?layout ?label ?view ?keep_ended ?commit ?plain
        ?delay f)

let with_output = Backend.with_output
let print = Backend.print
let redraw = Backend.redraw
