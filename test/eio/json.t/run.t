With --progress=json, what the program prints on standard output while a
task is current becomes an output event of that task when standard output
writes where the display does, as in 2>&1, so that every line of the stream
is a JSON event (README, Known limitations):

  $ ../piped.exe json-stdout 2>&1
  {"time":0.000,"event":"start","id":0,"task":["build"]}
  {"time":0.000,"event":"output","id":0,"task":["build"],"text":"from stdout"}
  {"time":0.000,"event":"output","id":0,"task":["build"],"text":"from stderr"}
  {"time":0.000,"event":"end","id":0,"task":["build"],"status":"succeeded","elapsed":0.000}
