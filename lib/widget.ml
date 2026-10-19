(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let border ?theme ?border ~default () =
  match border with
  | Some border -> border
  | None -> Option.fold ~none:default ~some:Theme.border theme

let one_line span = Span.concat ~sep:(Span.text " ") (Span.lines span)
