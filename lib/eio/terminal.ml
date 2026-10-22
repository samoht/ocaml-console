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
      Option.bind (int_of_string_opt value) usable)

let first ~default sources =
  Option.value ~default (List.find_map (fun source -> source ()) sources)

(* The ioctl answers whenever [fd] has a terminal behind it, $COLUMNS and
   $LINES answer when the shell exported them, and 80 by 24 is what everyone
   else falls back to. No [tput] probe: it reads the same termcap entry the
   shell already consulted and costs a shell, a fork and two blocking waits
   inside this Eio driver to say so. *)
let width fd =
  first ~default:80
    [ width_of_ioctl fd; (fun () -> dimension_of_env "COLUMNS" usable) ]

let height_of_ioctl fd () =
  Option.bind (window_size fd) (fun w -> positive w.rows)

let height fd =
  first ~default:24
    [ height_of_ioctl fd; (fun () -> dimension_of_env "LINES" usable) ]

let is_tty fd = Eio_unix.Fd.use_exn "isatty" fd Unix.isatty
let dimensions fd = (width fd, height fd)
