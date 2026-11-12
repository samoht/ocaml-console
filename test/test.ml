(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let () =
  Alcotest.run "console"
    [
      Test_color.suite;
      Test_colour.suite;
      Test_style.suite;
      Test_view.suite;
      Test_table.suite;
      Test_tree.suite;
      Test_rule.suite;
      Test_panel.suite;
      Test_border.suite;
      Test_guide.suite;
      Test_bar.suite;
      Test_theme.suite;
      Test_progress.suite;
      Test_progress_driver.suite;
      Test_slot.suite;
      Test_spinner.suite;
      Test_display.suite;
      Test_display_core.suite;
      Test_progress_show.suite;
      Test_progress_row.suite;
      Test_layout.suite;
      Test_display_driver.suite;
      Test_console.suite;
      Test_console_vte.suite;
      Test_recording.suite;
      Test_cast.suite;
    ]
