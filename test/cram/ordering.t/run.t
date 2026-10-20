Without a terminal, the display flushes standard output before each line it
writes, so that the two outputs merged with 2>&1 keep the order in which the
program wrote them:

  $ ordering.exe 2>&1 | cat
  result 1
  • fetch
  result 2
    › fetch: fetched
  ✓ fetch  0.0s
  result 3

Under Console_eio.run the same holds for the Logs messages, in a task or not,
and for what the program prints on standard error itself (in ASCII, the test
having no UTF-8 locale):

  $ piped.exe notes 2 2>&1 | cat
  result 1
  [-] step 1
    step 1: working on 1
  [+] step 1  0.0s
  ! after 1
  note 1
  result 2
  [-] step 2
    step 2: working on 2
  [+] step 2  0.0s
  ! after 2
  note 2
