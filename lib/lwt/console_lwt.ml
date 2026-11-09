(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let run ?ppf ?progress ?layout ?label ?view ?keep_ended ?commit ?plain ?delay f
    =
  Fiber.spawn (fun () ->
      Backend.run ?ppf ?progress ?layout ?label ?view ?keep_ended ?commit ?plain
        ?delay (fun root -> Fiber.await (f root)))

let with_output = Backend.with_output
let print = Backend.print
let redraw = Backend.redraw

module Task = struct
  let run ?key ?log_lines ?transient parent label f =
    Fiber.spawn (fun () ->
        Console.Task.run ?key ?log_lines ?transient parent label (fun t ->
            Fiber.await (f t)))
end
