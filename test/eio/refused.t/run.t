A write can fail without the runtime taking the failure as final. The bytes
then stay in the channel, and every later flush fails again. Here standard
error, the display's output, is a socket that is not connected. As on a closed
pipe, the display's lines end and nothing else: the program exits as it
returns.

  $ ../piped.exe socket 3
  result 1
  result 2
  result 3
