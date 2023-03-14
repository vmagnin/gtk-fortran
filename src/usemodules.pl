#!/usr/bin/env perl

# This file is part of gtk-fortran, a GTK / Fortran interface library.
# Copyright (C) 2012 The gtk-fortran team
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
# this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
# If not, see <http://www.gnu.org/licenses/>.
#
# Contributed by James Tappin 2012-08-14
# Udated: vmagnin 2020-02-12

# Scan Fortran source files for GTK (etc.) routines. It prints
# statistics and create a gtk-modules.txt file containing the USE statements
# that you can copy/paste in your programs.
# This is a somewhat 'tighter' replacement for usemodules.py.
# The building system will copy it and replace the @.*@ strings
# and install it as the gtk-?-fortran-modscan command.

use strict ;
use warnings ;
use Getopt::Long;

my %tokens;
my @enumerators;
my $csvfile='@PREFIX@/@SHARE@/gtk-fortran/gtk-@GTK@-fortran-index.csv';
my $enumfile='@PREFIX@/@SHARE@/gtk-fortran/gtk-@GTK@-enumerators.lis';
# Is this the local script or the gtk-?-fortran-modscan version generated
# and installed by the building system ?
if ($csvfile =~ /gtk-\@GTK/) {
    $csvfile='gtk-fortran-index.csv';
    $enumfile='gtk-enumerators.lis';
}

my $outfile='gtk-modules.txt';
my @sources;
my $filter;
my $help='';
my $link=1;

# Read the options
GetOptions('csv=s' => \$csvfile,
	   'out=s' => \$outfile,
	   'enum=s' => \$enumfile,
	   'help' => \$help,
	   'link!' => \$link);

if ($help) {
    print "\nScan Fortran source files for GTK and related library\n";
    print "functions and generate the required USE statements.\n";
    print "Note that only one set of USE statements is generated for each\n";
    print "source file, even if it contains several program units.\n";
    print "Enumerator constants are found if the list is present.\n\n";
    print "Usage:\n";
    print " $0 [--csv=<file> --out=<file> --enum=<file> --help --[no]link] [files]\n";
    print "\n";
    print "--csv      Specify the CSV file with the function list [$csvfile]\n";
    print "--out      Specify the output file for the use statements [$outfile]\n";
    print "--enum     Specify the file with the list of constants [$enumfile]\n";
    print "--help     Print this message\n";
    print "--[no]link Specify whether to scan symbolic links (only applicable when\n";
    print "           scanning a directory) [yes]\n";
    print "files      A list of files to scan. If empty then look at all Fortran\n";
    print "           sources in the current directory. If a single directory is\n";
    print "           given, then all Fortran files in that directory are checked.\n\n";
    exit(0);
}

# Read the function list
&read_csv;
&read_enum;

foreach (sort keys(%tokens)) {
    print "Found module $_ with $#{$tokens{$_}} functions\n";
}
print "Found $#enumerators enumerators in module gtk\n";

my $i = 0 ;  # Loop counter

# Get the list of source files
if ($#ARGV >= 0) {
    if (($#ARGV == 0) && (-d $ARGV[0])) {
	opendir(CD, $ARGV[0]);
	@sources = readdir(CD);
	closedir(CD);
	$filter = 1;
	# If understand the perl documentation correctly, then
	# / should work in open on non-unix systems.
	for ($i = 0; $i <= $#sources; $i++) {
	    $sources[$i] = $ARGV[0].'/'.$sources[$i];
	}
    } else {	
	@sources = @ARGV;
	$filter = 0;
    }
} else {
    opendir(CD,'.');
    @sources = readdir(CD);
    closedir(CD);
    $filter = 1;
}

print "\n";

open(OUT, ">$outfile") || die "Failed to open output $outfile: $!\n";

# Check each file
my $sfile = "" ;
my $code = "" ;
my $module = "" ;
foreach $sfile (sort @sources) {
    if ($filter) {      # If we are reading a directory skip non-Fortran files.
	$sfile =~ /(.f90$)|(.f95$)|(.f03$)|(.f08$)/i || next;
	next if (-l $sfile & ! $link);
    }
    next if (-d $sfile);

    print "Scanning: $sfile ";
    $code = &read_source($sfile);

    # Scan over all modules

    my $count = 0;
    my $ecount = 0;
    my $fflag = 1;

    foreach $module (sort keys(%tokens)) {
	my @functions = @{$tokens{$module}};
	my @used = (0) x ($#functions+1);
	my @eused = (0) x ($#enumerators+1);

	# Look for function names delimited by non-name characters.
	# N.B. \w is a "word" character i.e. [a-zA-Z0-9_] to which
	# we add a dot so that the Fortran logical constants
	# .true. and .false. do not match the GTK constants TRUE and FALSE.
	# I don't think there is any valid context in which a dot may
	# immediately precede or follow a function/subroutine name.
	# Since any program or module will start with PROGRAM or
	# MODULE statement and end with an END, there should never
	# be a possibility of a function name right at the start
	# or end of the program.
	
	for ($i = 0; $i <= $#functions; $i++) {
	    if ($code =~ /[^\w\.]($functions[$i])[^\w\.]/a) {
		$used[$i] = 1;
		$count ++;
	    }
	}
	if ($module eq 'gtk') {
	    for ($i = 0; $i <= $#enumerators; $i++) {
		if ($code =~ /[^\w\.]($enumerators[$i])[^\w\.]/a) {
		    $eused[$i] = 1;
		    $ecount ++;
		}
	    }
	}
	my $start = 1 ;
	my $colno = 0 ;
	my $oline = "";

	# Build the required "USE" statements.

	for ($i = 0; $i <= $#functions; $i++) {
	    if ($used[$i]) { # The function is used, add it to the list

		# If this is the first function in the module,
		# start to build a new USE.
		if ($start) {
		    # If this is the first USE for the source file put a header.
		    if ($fflag) {
			print OUT "\n!********************************\n";
			print OUT "! GTK modules for $sfile\n";
			$fflag = 0;
		    }
		    $oline = "    use $module, only: ";
		    $colno = length($oline);
		    $start = 0;
		}

		# If we would go past col 80 with the function,
		# add a continuation and reset the column counter.
		if ($colno + length($functions[$i]) > 78) {
		    $oline .= "&\n";
		    $oline .= "        & ";
		    $colno = length("        & ");
		}

		# Append the function name and increment the colun counter.
		$oline .= "$functions[$i], ";
		$colno += length("$functions[$i], ");
	    }
	}
	if ($module eq 'gtk') {
	    for ($i = 0; $i <= $#enumerators; $i++) {
		if ($eused[$i]) { # The enumerator is used, add it to the list
		    # If this is the first function in the module,
		    # start to build a new USE.
		    if ($start) {
			# If this is the first USE for the source file put a header.
			if ($fflag) {
			    print OUT "\n!********************************\n";
			    print OUT "! GTK modules for $sfile\n";
			    $fflag = 0;
			}
			$oline = "    use $module, only: ";
			$colno = length($oline);
			$start = 0;
		    }

		    # If we would go past col 80 with the function,
		    # add a continuation and reset the column counter.
		    if ($colno + length($enumerators[$i]) > 78) {
			$oline .= "&\n";
			$oline .= "        & ";
			$colno = length("        & ");
		    }
		
		    # Append the function name and increment the colun counter.
		    $oline .= uc "$enumerators[$i], ";
		    $colno += length("$enumerators[$i], ");
		}
	    }
	}
	# If we found any functions from this module, strip trailing commas
	# spaces and ampersands and write the use statement.
	if (! $start) {
	    $oline =~ s/[& ,]*$//;
	    print OUT "$oline\n\n";
	}
    }
    print "found $count functions and $ecount enumerators.\n";
}

close (OUT);

exit(0);

# Reads the CSV file generated by cfwrapper.py and extracts the first two fields
# which are the module name and the function name. These are stored as a hash
# of references.
sub read_csv
{
    my @tmp;

    open(CSV, $csvfile) || die "Failed to open $csvfile: $!\n";

    while (<CSV>) {
	chomp;
	@tmp = split /;/, $_, 3;
	$tokens{$tmp[0]} = [] unless exists $tokens{$tmp[0]};
	push @{$tokens{$tmp[0]}}, lc $tmp[1];
    }

    $tokens{gtk} = [] unless exists $tokens{gtk};
    push @{$tokens{gtk}}, ('gtk_init', 'g_signal_connect');

    close(CSV);
}

# Reads the list of enumerators found.
sub read_enum
{
    @enumerators=('true', 'false');

    if (open(ENUM, $enumfile)) {
	while (<ENUM>) {
	    chomp;
	    push @enumerators, $_;
	}
	close(ENUM);
    } else {
	warn "Failed to open enumerators file $enumfile: $!\n";
    }
}

# Reads a fortran source file, strips out quoted strings, comments and
# syntactically irrelevant spaces. Continuations are rejoined, and
# statements are separated by newlines.
sub read_source
{
    my $sfile = $_[0];
    my $jflag = 0;
    my $line = "";
    my $code = "";

    open(SFILE, $sfile) || die "Failed to open $sfile: $!\n";

    while (<SFILE>) {                 # Get each line of the file.
	chomp;                        # Remove the trailing newline
	$_ = lc;                      # Lower case the line (this is much
                                      # quicker than using the /i qualifier).
	s/".+"//g; s/'.+'//g;         # remove quoted strings
	/^([^!]*)!.*/ && ($_ = $1);   # Remove comments
	next if /^\s*$/;              # No further action on blank line

	if ($jflag) {                 # Is a continuation
	    s/^\s*&//;                # Remove leading space and &
	    $line .= $_;              # Concatenate
	} else {                      # Not a continuation
	    s/^\s*//;                 # Remove leading space
	    $line = $_;               # Start new line
	}

	if ($line =~ /&\s*$/) {       # Ends with a continuation marker?
	    $line =~ s/&\s*$//;       # Strip the trailing & (and any
	                              # following space)
	    $jflag = 1;               # Flag as continued
	    next;                     # Get the continuation
	}

	$jflag = 0;                   # Flag as complete line
	next if $line =~ /^use\s/;    # Don't scan USE statements.

	$code .= "$line\n";           # Concatenate into a single string.
    }
    close(SFILE);

    return($code);
}
