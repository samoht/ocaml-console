(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let positive n = if n > 0 then Some n else None

(* No terminal has more than the 4096 columns or rows a display takes. *)
let usable n = if n > 0 && n <= 4096 then Some n else None

let window_size = function
  | None -> None
  | Some fd -> (
      try Some (Eio_unix.Pty.get_window_size fd)
      with Unix.Unix_error _ -> None)

let width_of_ioctl fd () =
  Option.bind (window_size fd) (fun w -> positive w.cols)

let dimension_of_env name usable =
  Option.bind (Sys.getenv_opt name) (fun value ->
      Option.bind (Recording.decimal value) usable)

let first ~default sources =
  Option.value ~default (List.find_map (fun source -> source ()) sources)

(* POSIX XBD 8.3: when set, COLUMNS and LINES override the size of the terminal.
   No [tput] fallback: it reads the termcap entry the shell read, at the cost of
   a fork. *)
let width fd =
  first ~default:80
    [ (fun () -> dimension_of_env "COLUMNS" usable); width_of_ioctl fd ]

let height_of_ioctl fd () =
  Option.bind (window_size fd) (fun w -> positive w.rows)

let height fd =
  first ~default:24
    [ (fun () -> dimension_of_env "LINES" usable); height_of_ioctl fd ]

let asked () =
  Option.map (fun w () -> (w, height None)) (dimension_of_env "COLUMNS" usable)

let is_tty fd = Eio_unix.Fd.use_exn "isatty" fd Unix.isatty
let dimensions fd = (width fd, height fd)
