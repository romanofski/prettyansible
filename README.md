# prettyansible

This is a little proof-of-concept pretty parser-pretty printer for
ansible output. It needs more tests and the parser needs to become more
robust to handle various changes in the output.

# Abandoned

*Found a much simpler way when dealing ansible*

* It seems all I need is comparing log files from previous plays
* For this simply use a `vimdiff` to achieve a comparison. Sure it's not
  accurate since the dates will show up as differences in the log files,
  but it's good enough
    * if you need pretty printing for some of the output, simply use a
      vim search and replace.
* Furthermore, for the variance in output I'm doubtful that a parser is
  a good solution.

