(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type scope = Task_core.scope = {
  current : unit -> Task.t option;
  within : 'a. Task.t -> (unit -> 'a) -> 'a;
  cancelled : exn -> bool;
}

type level = Task_core.level

let root ?label ~scope ~now () = Task_core.root ?label ~scope ~now ()
let report = Task_core.report
let current (root : Task.t) = Task_core.current (Task_core.work root)
let ending = Task_core.on_ending
let id (t : Task.t) = Task_core.id t
let is_async = Task_core.is_async
let protect = Task_core.protect
