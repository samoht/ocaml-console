(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let wrap (r : Logs.reporter) : Logs.reporter =
  let report src level ~over k msgf =
    r.Logs.report src level ~over k (fun construction ->
        Display_term.suspend (fun () -> msgf construction))
  in
  { Logs.report }

let saved : Logs.reporter option Atomic.t = Atomic.make None

let install () =
  let r = Logs.reporter () in
  if Atomic.compare_and_set saved None (Some r) then Logs.set_reporter (wrap r)

let uninstall () =
  match Atomic.exchange saved None with
  | None -> ()
  | Some r -> Logs.set_reporter r
