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

let saved : Logs.reporter option ref = ref None

let install () =
  match !saved with
  | Some _ -> ()
  | None ->
      let r = Logs.reporter () in
      saved := Some r;
      Logs.set_reporter (wrap r)

let uninstall () =
  match !saved with
  | None -> ()
  | Some r ->
      saved := None;
      Logs.set_reporter r
