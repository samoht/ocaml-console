## unreleased

### Added

- `View.wrap ~words`, to choose where a line may break (#2).

- `Console_eio.domain_mgr`, so that a program that runs domains shows the
  same frames on every run under `CONSOLE_CAST` or
  `CONSOLE_DETERMINISTIC_CLOCK` (#66).

- `Console_input.crlf`, so that output sent to a raw-mode terminal starts each
  line in the first column (#3).

- `Progress.pp_duration`, to print a duration as the progress rows do, such as
  `6.4s` or `1m05s` (#4).

- `Task.advance`, to step a task's progress without keeping the count yourself
  (#8).

- `Color.gradient`, to get the colour at any point of a gradient (#9), and
  `Border.with_gradient`, to draw table and panel frames in one (#12).

- `Console_eio.redraw`, to draw the display now instead of at the next refresh
  (#10).

- `Console_tty.width`, to fit output printed outside `Console_eio.run` to the
  terminal, or to `$COLUMNS` on a pipe (#13, #61).

- `Env.setup` and `Env.ensure`, to apply console's colour and UTF-8 rules in a
  program that can't link Unix (#11).

- `Progress.pp` and `Room.pp`, to print a progress mode or a room in a log
  (#34).

- `console.driver`, to run console on a scheduler other than Eio. A backend
  gives it a clock, fiber-local values, a way to wait and wake, daemons and
  cancellation (`Console_driver.S`); the driver calls them only on the
  scheduler's thread (#61, #67).

- `console-lwt`, to show the progress rows of a program that runs with Lwt.
  It has no deterministic mode: it runs in real time under
  `CONSOLE_DETERMINISTIC_CLOCK` or `CONSOLE_CAST` (#67).

### Changed

- `CONSOLE_DETERMINISTIC_CLOCK` alone no longer makes a sleep on
  `Console_eio.clock` last its real duration. As under `CONSOLE_CAST`, the
  time jumps to the next sleep once nothing in the program can run, and
  stands still while the program waits on anything else, such as a child
  process. Under either variable, such a clock used from another domain
  raises `Invalid_argument` (#66).

- `Console_eio.size` is now `Console_tty.size`, as reading a terminal's size
  needs no scheduler (#61).

- `Console_input` limits a line to 4095 bytes (`?max_line`) and its history to
  500 lines (`?history_size`). Typing past the limit rings the bell (#1).

- Wrapped text no longer breaks after a slash, so paths and URLs stay whole
  unless they don't fit on a line (#2).

- `Logs.app` messages no longer show a `[source]` prefix under
  `Console_eio.run`. Other levels keep it (#5).

- `$COLUMNS` and `$LINES`, when set to a decimal number, now give the size of
  the output, on a terminal as on a pipe, as POSIX says. Other values, such as
  `0x50`, are ignored (#7, #20, #49).

- Ctrl-Z now stops a program under `Console_eio.run`. It used to give the
  terminal back and carry on running (#29).

- Ctrl-C (`Sys.Break`), `Out_of_memory` and `Stack_overflow` raised while a
  view draws now reach the program at once, instead of at the next redraw or
  close (#53).

- Progress updates cost less, and a program that links console allocates less
  as it starts: no table of widths or durations is built until it is used,
  also when the program builds a progress layout at load (#30, #40, #64).

- The live lines under a running task's row, printed, logged with `Logs` or
  given with `Task.log`, are now drawn in the theme's muted colour (faint in
  the default theme), so they read as a log under the row. A warning or an
  error is drawn as before, and so is every line printed to the scrollback,
  such as the output of a failed task (#65).

### Fixed

- `Task` and `Display` now work from any domain, so a worker of an
  `Eio.Executor_pool` can report progress. `Task.advance` from several domains
  used to lose steps, and `Task.log` could end the program (#57).

- `Console_eio.run` from another domain now joins the run under way instead of
  raising `Invalid_argument` (#57).

- A `CONSOLE_CAST` recording now keeps what a program writes after it starts
  its first domain (for example by creating an `Eio.Executor_pool`). It used
  to go to the terminal instead (#66).

- A `Logs` message from a worker domain no longer waits for the display, so a
  program that waits for its workers no longer hangs. Past 4 MiB of waiting
  lines, the oldest are dropped and a line says how many (#59).

- `Console_eio.run` now restores the terminal and shows the cursor when a view,
  a hook or the output raises (#26), when a signal comes after a view raised,
  and when a view calls `exit` (#43). An output that breaks as a run ends no
  longer keeps the next run from starting (#26).

- A cancelled `Console_eio.run` restores the terminal too. It waits at most a
  second for an output nobody reads (#42).

- A signal or an `exit` that can't reach the display still shows the cursor,
  and ends an escape code cut in half. This covers another domain or thread,
  and a write that does not end within a second (#37).

- On OpenBSD too, a signal or an `exit` that cannot reach the display shows
  the cursor (#70).

- Progress rows short of room keep their count and time in their columns, and
  a label a task was renamed from no longer widens its column once the task
  ends (#58).

- A SIGINT or SIGTERM now ends the program when it is handled in another
  domain, and when giving the terminal back fails (#37).

- An output whose reader has gone, such as a closed pipe, no longer raises
  when a display starts or a program prints through `Display.with_output`
  (#18).

- An exception from a `commit` or `plain` function of `Progress.show` no
  longer escapes `Task.run`: the next redraw or `Display.close` raises it, as
  for a view (#26).

- A fiber cancelled in a view or around `Console_eio.with_output` now stops
  there, instead of raising its `Cancelled` at the next redraw or close. A
  report or a log from a cancelled fiber no longer raises (#42).

- An update cut short no longer erases lines above the display, and its lines
  still go out. A task that ends in a cancelled fiber no longer leaves its
  running row on the screen (#27).

- A task now ends once, with its subtasks, when an interrupt comes as it ends
  (#41).

- Starting a domain during `Console_eio.run`, as an Eio executor pool does, no
  longer stops the display from taking the program's output (#39).

- A log or a print from a systhread during `Console_eio.run` no longer raises
  `Effect.Unhandled` (#37), and two domains starting their first run at once no
  longer raise `Lazy.Undefined` (#37).

- `CONSOLE_CAST` recordings are now valid asciicast v2: the header is UTF-8
  whatever `$TERM` holds, and C1 control characters are escaped (#50).

- `Bar.horizontal ~brackets:true` given a width near `max_int` now fills its
  room, instead of drawing one cell or none (#60).

## 0.9.0 (29/10/2026)

Initial release of `console` and `console-eio`.
