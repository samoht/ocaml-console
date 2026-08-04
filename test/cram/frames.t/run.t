A TTY progress session replayed through a terminal emulator: each frame is the
reconstructed screen (real control characters applied), so a duplicated or
stranded row would show up directly. Geometry is pinned for a stable diff.

On a roomy terminal the whole tree, its per-row log windows, the committed
rows and the redraw after a row closes all fit -- and every live row appears
exactly once:

  $ COLUMNS=70 LINES=20 frames.exe
  --- tree with one node + a log ---
    # planning
  [-] Build 0.0s (0/1)
  ⠋ app                                                             0.0s
    # planning
  --- two children, each streaming logs ---
    # planning
        # fetch index
        # cc a.o
        # cc b.o
  [-] Build 0.0s (0/3)
  ⠋ app                                                             0.0s
    # planning
    ├── ⠋ resolve                                                   0.0s
    │   # fetch index
    └── ⠋ compile                                                   0.0s
        # cc a.o
        # cc b.o
  --- resolve finished (committed), siblings still live ---
    # planning
        # fetch index
        # cc a.o
        # cc b.o
    => resolve  0.0s
  [-] Build 0.0s (1/3)
  ⠋ app                                                             0.0s
    # planning
    └── ⠋ compile                                                   0.0s
        # cc a.o
        # cc b.o
  --- more logs after the row closed ---
    # planning
        # fetch index
        # cc a.o
        # cc b.o
    => resolve  0.0s
        # cc c.o
        # link
    # still working
  [-] Build 0.0s (1/3)
  ⠋ app                                                             0.0s
    # planning
    # still working
    └── ⠋ compile                                                   0.0s
        # cc b.o
        # cc c.o
        # link
  --- all finished ---
    # planning
        # fetch index
        # cc a.o
        # cc b.o
    => resolve  0.0s
        # cc c.o
        # link
    # still working
    => compile  0.0s
  => app  0.0s
  [+] Build 0.0s (3/3)

On a short terminal the live region would not fit. Rather than let a redraw
scroll a stale copy into the scrollback (which reads as a duplicated row), the
tree of rows is kept and the oldest log lines are shed, with a marker counting
what was hidden:

  $ COLUMNS=70 LINES=8 frames.exe
  --- tree with one node + a log ---
    # planning
  [-] Build 0.0s (0/1)
  ⠋ app                                                             0.0s
    # planning
  --- two children, each streaming logs ---
    # planning
        # fetch index
        # cc a.o
        # cc b.o
  [-] Build 0.0s (0/3)
    ... (2 more lines)
  ⠋ app                                                             0.0s
    ├── ⠋ resolve                                                   0.0s
    └── ⠋ compile                                                   0.0s
        # cc a.o
        # cc b.o
  --- resolve finished (committed), siblings still live ---
    # planning
        # fetch index
        # cc a.o
        # cc b.o
    => resolve  0.0s
  [-] Build 0.0s (1/3)
  ⠋ app                                                             0.0s
    # planning
    └── ⠋ compile                                                   0.0s
        # cc a.o
        # cc b.o
  --- more logs after the row closed ---
    # planning
        # fetch index
        # cc a.o
        # cc b.o
    => resolve  0.0s
        # cc c.o
        # link
    # still working
  [-] Build 0.0s (1/3)
    ... (2 more lines)
  ⠋ app                                                             0.0s
    └── ⠋ compile                                                   0.0s
        # cc b.o
        # cc c.o
        # link
  --- all finished ---
    # planning
        # fetch index
        # cc a.o
        # cc b.o
    => resolve  0.0s
        # cc c.o
        # link
    # still working
    => compile  0.0s
  => app  0.0s
  [+] Build 0.0s (3/3)
