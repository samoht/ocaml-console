let () =
  Alcotest.run "console_eio"
    [
      Test_display.suite;
      Test_spinner.suite;
      Test_anim.suite;
      Test_terminal.suite;
      Test_console_eio.suite;
    ]
