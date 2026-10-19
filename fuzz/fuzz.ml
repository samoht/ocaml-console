(*---------------------------------------------------------------------------
  Copyright (c) 2025 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () =
  Alcobar.run "console"
    [
      Fuzz_console.suite;
      Fuzz_api_rule.suite;
      Fuzz_api_color.suite;
      Fuzz_api_bar.suite;
      Fuzz_api_span_bounds.suite;
      Fuzz_api_width.suite;
      Fuzz_api_display.suite;
      Fuzz_api_display_line.suite;
      Fuzz_api_style.suite;
      Fuzz_api_border.suite;
      Fuzz_api_guide.suite;
      Fuzz_api_spinner.suite;
      Fuzz_api_input.suite;
      Fuzz_api_span.suite;
      Fuzz_api_panel.suite;
      Fuzz_api_table.suite;
      Fuzz_api_tree.suite;
      Fuzz_api_theme.suite;
      Fuzz_api_console_text.suite;
    ]
