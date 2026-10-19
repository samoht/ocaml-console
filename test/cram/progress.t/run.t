In plain mode (no terminal), the display emits one append-only line per
finished row, then a header summary. This is the text a log collector ingests.

  $ dump_progress.exe
  ✓ foo.ml  0.1s
  ✓ bar.ml  0.1s
  ✓ baz.ml  0.1s
  ✓ Processing 0.3s (3/3)
