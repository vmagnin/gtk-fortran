#!/usr/bin/env perl

# Copyright (C) 2012
# Free Software Foundation, Inc.
#
# This file is part of the gtk-fortran gtk+ Fortran Interface library.
#
# This is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License along with
# this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
# If not, see <http://www.gnu.org/licenses/>.
#
# Contributed by James Tappin 8/7/2012.


# Run all the executables in a directory (other than itself).
# Or the listed examples.
#
# Usage:
#    <path>/run_all   # Run all executables in the same dir as the script
#    <path>/run_all <dir>  # Run all executables in <dir>
#    <path>/run_all <dir> <exe1> ... # run listed executables in <dir>
#
# E.g.:
#     ./tests/run_all plplot   # to run all the plplot examples
#     ./tests/run_all examples hl_list_n hl_list_renderers
#                              # to run 2 specific example codes.

my $dir, $exe, $n;
my @tlist, @list;

if ($#ARGV >= 0) {
    $dir = $ARGV[0];
    if ($0 =~ m{.*/([^/]*)}) {
	$exe = $1;
    } else {
	$exe = $0;
    }
    if ($#ARGV > 0) {
	@tlist = @ARGV[1..$#ARGV];
    }
} elsif ($0 =~ m{(.+)/([^/]*)}) {
    $dir = $1;
    $exe = $2;
} else {
    $dir = ".";
    $exe = $0;
}

chdir($dir);

opendir(DIR,'.');
my @list = readdir(DIR);
closedir(DIR);

foreach (@list) {
    next if (-d $_);                      # Don't try to run a directory.
    next if (! -x $_);                    # Don't try to run a non-executable.
    next if ($_ eq $exe);                 # Don't recurse.
    if ($#tlist >= 0) {                   # If a list is given then check if
	$n = 0;                           # the file is in the list
	for ($i = 0; $i <= $#tlist; $i++) {
	    if ($tlist[$i] eq $_) {
		$n = 1;
		last;
	    }
	}
	next if ($n == 0);
    }
    print "Running: $_\n";
    system "./$_";
}
