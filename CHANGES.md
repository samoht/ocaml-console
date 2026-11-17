## unreleased

### Fixed

- Windows: the tests of `console` and `console-eio` pass in opam builds,
  which ran every Eio program on the posix backend that Windows lacks (#101).

## 0.10.0 (17/11/2026)

### Added

- New packages and libraries:
  - `console-lwt` shows progress rows in a program that runs with Lwt. It
    runs in real time, even under `CONSOLE_CAST` (#67).
  - `console.driver` runs console on any scheduler. A backend gives it a
    clock, fiber-local values, wake-ups and cancellation (#61, #67).
  - `console.cli` gives a Cmdliner program `--theme` and `--color` options
    (#78, reported by @toastal).

- Themes: `Theme.set_program` sets the theme of every formatter that has no
  theme of its own, including the ones `Fmt.str` creates (#78, reported by
  @toastal).

- Widgets: `View.wrap ~words` chooses where a line may break (#2).
  `Color.gradient` gives the colour at any point of a gradient (#9), and
  `Border.with_gradient` draws frames in one (#12).

- Progress rows: `Task.advance` steps a task without keeping the count (#8),
  and `Progress.Layout.label ~width` fixes the label column (#75).

- Printers: `Progress.pp_duration` prints a duration as the rows do (#4).
  `Progress.pp` and `Room.pp` print a mode or a room in a log (#34, #53).

- Terminal: `Console_tty.width` fits output printed outside a run to the
  terminal (#13, #23, #61). `Env.setup` and `Env.ensure` apply the colour
  rules without Unix (#11).

- `Console_input.crlf` writes lines for a raw-mode terminal (#3).

- Numbers: `Decimal.string_of_float` and `Decimal.pp` print a float with a
  fixed count of decimals, the same on every platform, where `Printf` rounds
  a tie as the C library does (#88).

- Eio: `Console_eio.redraw` draws the display at once (#10), and
  `Console_eio.domain_mgr` keeps recordings of programs with domains
  reproducible (#66).

### Changed

- Themes: `Console_tty.setup ~theme` now sets the program's theme, so
  `View.to_string` and strings built with `Fmt.str` use it too (#78,
  reported by @toastal). `Theme.default`'s accent is now bright blue (#75).

- `Console_eio.size` is now `Console_tty.size`: reading a terminal's size
  needs no scheduler (#61).

- Progress rows keep their columns, so steps line up in the scrollback, and
  look calmer: a finished bar and the lines under a running row are muted,
  a failed or cancelled bar red or yellow (#65, #75).

- Text: wrapped text no longer breaks after a slash, so paths and URLs stay
  whole (#2). `Logs.app` messages lose their `[source]` prefix (#5).

- `Console_input` limits a line to 4095 bytes, and rings the bell past
  that. It keeps the newest 500 lines of history (#1).

- `$COLUMNS` and `$LINES` set the output size on a terminal as on a pipe,
  as POSIX says. Values that are not decimal numbers are ignored (#7, #20,
  #49).

- `CONSOLE_DETERMINISTIC_CLOCK` now jumps time to the next sleep, as
  `CONSOLE_CAST` does. Under either variable, using the clock from another
  domain raises `Invalid_argument` (#66).

- Ctrl-Z now stops a program under `Console_eio.run` (#29). Ctrl-C,
  `Out_of_memory` and `Stack_overflow` raised in a view reach the program
  at once (#53).

- Progress updates cost less, and linking console no longer builds tables
  at startup (#30, #40, #64).

- The tests need matrix 0.2.0 and k7 0.10.0 (#74, #77, reported by
  @toastal).

### Fixed

- A short byte count in a narrow row, such as `1.3K`, now has the same
  digits on Windows as elsewhere (#88).

- The terminal is given back, with its cursor shown, when a view, a hook or
  the output raises, when a view calls `exit`, when a run is cancelled, and
  when a signal or an `exit` cannot reach the display (#26, #37, #42, #43, #70).

- Domains: `Task` and `Display` work from any domain, and a run joins the
  one under way (#57). Logs from workers or systhreads no longer hang or
  raise, and a new domain keeps the capture and recording (#37, #39, #59, #66).

- Windows: a run no longer raises `Invalid_argument` at start, because
  Windows has no SIGTSTP (#79). A program whose display writes to a pipe
  that closes now exits as it returns, not with status 2 (#93).

- Errors: an exception from `commit` or `plain` no longer escapes
  `Task.run` (#26), and a closed output pipe no longer raises (#18). A task
  ends once when interrupted as it ends (#41).

- Cancellation: a cancelled fiber stops in the view instead of at the next
  redraw (#42). An update cut short no longer erases lines above the
  display (#27).

- Running `Console_lwt.run` and `Console_eio.run` at the same time now
  raises `Invalid_argument` instead of opening a second display (#82).

- Finished tasks no longer go missing from the scrollback after a long
  failure, and tasks started under a finished one stay within the 1 MiB
  bound (#82).

- Layout: progress rows short of room keep their columns (#58), and a
  `Progress.Layout.const` or `custom` piece stays on one line (#82).

- Sizes: `Bar.horizontal ~brackets:true` fills a huge width (#60).
  `View.sized` and `View.place` treat a negative size as `0`, and `Guide.v`
  draws a control character as a space (#82).

- `CONSOLE_CAST` recordings are valid asciicast v2 (#50).

## 0.9.0 (29/10/2026)

Initial release of `console` and `console-eio`.
