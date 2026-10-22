Two builds side by side, under a table of their status and above the
summary. On a pipe, each task prints a line when it starts and when it
ends, and the last frame of the view comes last:

  $ columns
  [-] linux
    [-] fetch
  [-] macos
    [-] fetch
    [+] fetch  4/4  0.3s
    [-] compile
    [+] fetch  4/4  0.4s
    [-] compile
    [+] compile  12/12  0.9s
    [-] test
    [+] compile  12/12  1.3s
    [-] test
    [+] test  8/8  0.6s
  [+] linux  1.9s
    [+] test  8/8  0.8s
    [-] sign
    [+] sign  2/2  0.2s
  [+] macos  2.8s
  +-------+--------+
  | Build | Status |
  +-------+--------+
  | linux | done   |
  | macos | done   |
  +-------+--------+
  [+] Building 2.8s (9/9)
