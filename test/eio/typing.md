# The terminal surface's types

The display draws a frame each time its driver refreshes it, and a caller that
needs a frame on screen before it goes on waits for that frame, not for a
span of time: the wait ends when the renderer draws it. Whether a descriptor
is a terminal is asked of that descriptor, since a display may draw on
standard error while standard output is a pipe.

```ocaml
# Console.Display.on_frame;;
- : Console.Display.t -> int -> (unit -> unit) -> unit = <fun>
# Console_eio.frame_drawn;;
- : Console.Display.t -> int -> unit Eio__core.Promise.t = <fun>
# Console_eio.is_tty;;
- : Unix.file_descr -> bool = <fun>
```
