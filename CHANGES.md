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
  (#17).

- Add `Task.Event.equal`, to compare two events (#17).

- Add the `console.width` library, to measure the cells a string takes on a
  terminal as console does (#17).

- `Display.close` now restores the terminal even when a `commit` or `plain`
  function raises, and raises the exception afterwards (#17).

- A closed output, such as a pipe whose reader has gone, no longer raises when
  a display starts or around `Display.with_output` (#18).

- A clock that returns NaN or infinity no longer writes invalid times into a
  `CONSOLE_CAST` recording (#19).

- `$COLUMNS` and `$LINES` must now be decimal numbers: values such as `0x50` or
  `1_000` are ignored (#20).

- Add `Console_text.decimal`, to read an integer written in decimal digits alone
  (#24).

## 0.9.0 (29/10/2026)

Initial release of `console` and `console-eio`.
