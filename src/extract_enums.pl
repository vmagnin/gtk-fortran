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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License along with
# this program; see the files COPYING3 and COPYING.RUNTIME respectively.
# If not, see <http://www.gnu.org/licenses/>.
#
# Contributed by James Tappin 8/14/2012.


# Find the enumerator constants in a fortran source file and write 
# them out to a simple list.

use Getopt::Long;

my $infile="gtkenums-auto.f90";
my $outfile="gtk-enumerators.lis";

my $str;
my $count = 0;

GetOptions('in=s' => \$infile,
	   'out=s' => \$outfile);

open(IN, $infile) || die "Failed to open input $infile: $!\n";
open(OUT, ">$outfile") || die "Failed to open output $outfile: $!\n";

while (<IN>) {
    chomp;
    $_ = lc;
    /\s*enumerator\s*::\s*(\w+).*/ || next;
    $count++;
    print OUT "$1\n";
}

print "Found $count enumerator constants\n";

close(OUT);
close(IN);
