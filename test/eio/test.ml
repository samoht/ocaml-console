(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () =
  Alcotest.run "console_eio"
    [
      Test_display.suite;
      Test_display.report_domains_suite;
      Test_console_eio.suite;
      Test_terminal.suite;
      Test_prompt.suite;
      Test_mock.suite;
    ]
