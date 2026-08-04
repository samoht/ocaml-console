(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = { branch : string; last : string; pipe : string; space : string }

let v ~branch ~last ~pipe ~space = { branch; last; pipe; space }
let branch t = t.branch
let last t = t.last
let pipe t = t.pipe
let space t = t.space
let ascii = v ~branch:"+-- " ~last:"+-- " ~pipe:"|   " ~space:"    "

let unicode = v ~branch:"├── " ~last:"└── " ~pipe:"│   " ~space:"    "

let equal a b =
  String.equal a.branch b.branch
  && String.equal a.last b.last && String.equal a.pipe b.pipe
  && String.equal a.space b.space

let pp ppf t =
  Fmt.pf ppf "{ branch = %S; last = %S; pipe = %S; space = %S }" t.branch t.last
    t.pipe t.space
