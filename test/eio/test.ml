let () =
  Alcotest.run "console_eio"
    [
      Test_display.suite;
      Test_display.report_domains_suite;
      Test_console_eio.suite;
      Test_auto.suite;
      Test_prompt.suite;
    ]
