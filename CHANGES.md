## unreleased

- `Console_input` now limits line length (`?max_line`, 4095 bytes by default)
  and history (`?history_size`, 500 lines by default). Typing past the line
  limit rings the bell (#1).

- Wrapped text no longer breaks after a slash, so paths and URLs stay whole
  unless they don't fit on a line (#2).

- Add `View.wrap ~words`, to choose where lines may break (#2).

- Add `Console_input.crlf`, so that output sent to a raw-mode terminal starts
  each line in the first column (#3).

- Add `Progress.pp_duration`, to print durations like the progress rows do,
  such as `6.4s` or `1m05s` (#4).

- `Logs.app` messages no longer show a `[source]` prefix under
  `Console_eio.run`. Other log levels keep it (#5).

- `Console_eio.run` now respects `$COLUMNS` on a pipe. Without it, lines are
  still printed in full (#7).

- Add `Task.advance`, to step a task's progress without tracking the count
  yourself (#8).

- Add `Color.gradient`, to get the colour at any point along a gradient (#9).

- Add `Console_eio.redraw`, to draw the display immediately instead of waiting
  for the next refresh (#10).

- Add `Env.setup` and `Env.ensure`, to apply console's colour and UTF-8 rules
  in programs that can't link Unix (#11).

- Add `Border.with_gradient`, to draw table and panel frames in a colour
  gradient (#12).

- Add `Console_eio.width`, to fit output printed outside `Console_eio.run` to
  the terminal, or to `$COLUMNS` on a pipe (#13).

- Add `Progress.pp` and `Room.pp`, to print a progress mode or a room in a log
  (#34).

- `Sys.Break`, `Out_of_memory` and `Stack_overflow` raised in a view are no
  longer held until the next redraw or close: they go on from the call that
  was drawing, and the view stays (#53).

- A closed output, such as a pipe whose reader has gone, no longer raises when
  a display starts or when a program prints through `Display.with_output`
  (#18).

- `$COLUMNS` and `$LINES` must now be decimal numbers: values such as `0x50` or
  `1_000` are ignored (#20).

- A `CONSOLE_CAST` recording now has a header in valid UTF-8 whatever bytes
  `$TERM` holds, as the asciicast v2 format requires (#50).

- A `CONSOLE_CAST` recording now writes C1 control characters as `\uXXXX`, as
  the asciicast v2 format asks (#50).

- `$COLUMNS` and `$LINES`, when set, now override the size of a terminal, as
  POSIX says (#49).

- Ctrl-Z now stops a program under `Console_eio.run`. It used to give the
  terminal back and carry on running (#29).

- Console no longer builds its width cache as a program starts, only when it
  first measures text (#30).

- Starting a domain during `Console_eio.run` no longer stops the display from
  taking the program's output (#39).

- Updating a task's progress no longer formats text, so a program that reports
  its progress often spends less time on it (#40).

- A task now ends whole, with its subtasks, and every display of its work sees
  it end, when an interrupt comes as it ends (#41).

- An exception from a `commit` or `plain` function of `Progress.show` no longer
  escapes `Task.run` or the end of a task, and waits for the next redraw or
  close as a view's does (#26).

- `Display.close` now restores the terminal and gives the formatter back
  whatever the view, a hook, the size function or the output raises, and
  raises the exception afterwards (#26).

- `Console_eio.run` now restores the cursor when a `commit` function raises as
  the run ends, and an output that breaks as it ends no longer keeps the next
  run from starting (#26).

- A cancelled `Console_eio.run` now restores the terminal, waiting a second at
  most for an output that takes nothing (#42).

- A fiber cancelled in a view or around `Console_eio.with_output` now stops
  there instead of raising its `Cancelled` from the next redraw or the close,
  and a report or a log in a cancelled fiber no longer raises (#42).

- An update cut short by an interrupt or a cancellation no longer erases lines
  above the region when the next one goes out, and the lines it was to print
  still go out (#27).

- Frames no longer pile up behind a write under way or one cut short (#27).

- A task that ends in a cancelled fiber, on an Eio flow, no longer loses its
  last line nor leaves its running row on the screen, and the redraw that the
  end of a run stops goes out whole instead of leaving its row above the last
  frame (#27).

- A signal now ends a `Console_eio.run` even after a view raised, and restores
  the cursor on an Eio flow too (#43).

- A view that calls `exit` during a `Console_eio.run` now leaves the cursor
  shown (#43).

- When the display cannot be reached, from another domain or thread or during
  a write that does not end within a second, a signal or an `exit` now ends a
  cut escape code and shows the cursor before the program ends or stops,
  instead of leaving the terminal as it was (#37).

- A restoration that fails, such as giving back the echo of a terminal that has
  gone, no longer stops SIGINT or SIGTERM from ending the program (#37).

## 0.9.0 (29/10/2026)

Initial release of `console` and `console-eio`.
