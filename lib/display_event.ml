(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type level = [ `Info | `Warning | `Error ]
type outcome = [ `Succeeded | `Failed | `Cancelled ]
type result = [ `Ok | `Fail ]

type kind =
  [ `Message of level
  | `Started
  | `Log of level
  | `Finished of outcome
  | `Result of result ]

let equal_outcome a b =
  match (a, b) with
  | `Succeeded, `Succeeded | `Failed, `Failed | `Cancelled, `Cancelled -> true
  | _ -> false

type t = {
  elapsed : float;
  task : int option;
  key : string option;
  path : string list;
  kind : kind;
  message : string;
  detail : string option;
}

let elapsed t = t.elapsed
let task t = t.task
let key t = t.key
let path t = t.path
let kind t = t.kind
let message t = t.message
let detail t = t.detail

let pp_kind ppf = function
  | `Message `Info -> Fmt.string ppf "info"
  | `Message `Warning -> Fmt.string ppf "warning"
  | `Message `Error -> Fmt.string ppf "error"
  | `Started -> Fmt.string ppf "started"
  | `Log `Info -> Fmt.string ppf "log"
  | `Log `Warning -> Fmt.string ppf "warning-log"
  | `Log `Error -> Fmt.string ppf "error-log"
  | `Finished `Succeeded -> Fmt.string ppf "succeeded"
  | `Finished `Failed -> Fmt.string ppf "failed"
  | `Finished `Cancelled -> Fmt.string ppf "cancelled"
  | `Result `Ok -> Fmt.string ppf "ok"
  | `Result `Fail -> Fmt.string ppf "fail"

let pp ppf t =
  Fmt.pf ppf "@[%a%s%s: %s%a@]" pp_kind t.kind
    (if t.path = [] then "" else " ")
    (String.concat "/" t.path) t.message
    Fmt.(option (any " -- " ++ string))
    t.detail
