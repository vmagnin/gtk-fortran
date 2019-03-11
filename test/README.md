# Testing gtk-fortran

There is no automated tests in gtk-fortran. But you can use the run_all.pl Perl script to launch all the examples one by one:

## Manual

Run all the executables in a directory (other than itself). Or the listed examples.

Usage:

  <path>/run_all   # Run all executables in the same dir as the script

  <path>/run_all <dir>  # Run all executables in <dir>

  <path>/run_all <dir> <exe1> ... # run listed executables in <dir>

E.g.:

  ./tests/run_all plplot   # to run all the plplot examples

  ./tests/run_all examples hl_list_n hl_list_renderers
