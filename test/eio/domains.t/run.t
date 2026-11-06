A recording keeps what the program writes after its first domain starts:

  $ export TERM=xterm-256color LANG=en_US.UTF-8 LC_ALL=
  $ CONSOLE_CAST=domain.cast ../piped.exe domain
  $ grep -c "after the first domain" domain.cast
  1
