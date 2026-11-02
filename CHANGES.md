## unreleased

- `Console_input.v` no longer holds whatever a peer sends: a line stops at
  `?max_line` bytes (default 4095) and rings the bell past it, and the
  history keeps the last `?history_size` lines (default 500) (#1).

## 0.9.0 (29/10/2026)

Initial release of `console` and `console-eio`.
