#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# This file is part of gtk-fortran, a GTK / Fortran interface library.
# Copyright (C) 2011 The gtk-fortran team
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
# Contributed by Vincent Magnin, 01.28.2011
# Last modification: 2023-03-21 (tested with Python 3.10.7, Ubuntu)
# $ pylint *.py ../tools.py    => 8.51/10

""" Generates the *-auto.* files from the C header files of GLib and GTK.
For help, type: ./cfwrapper.py -h
"""

import re           # Regular expression library
import os
import time
import csv          # To write .csv files
import subprocess   # To launch a shell command
import argparse     # To parse command line
from collections import OrderedDict
import sys

# Project modules:
from globals_const import SRC_DIR
from lib_versions import Version
from errors import Errors
from stats import Statistics
# To use ../tools.py which contains the multiline() function
# (needed to import cleaning and analyze):
sys.path.append('../')
from cleaning import clean_header_file, preprocess_prototypes
from analyze import analyze_prototypes
from scan_types_and_enums import types_enums


# Definition of command line options:
PARSARG = argparse.ArgumentParser(description="Generate gtk-fortran files",
                                  epilog="GPLv3 license, https://github.com/vmagnin/gtk-fortran")
PARSARG.add_argument("-g", "--gtk", action="store", type=int, choices=[2, 3, 4],
                     metavar="2|3|4", nargs=1, required=True,
                     help="GTK major version")
PARSARG.add_argument("-v", "--version", action="store", nargs=1, required=True,
                     help="gtk-fortran semantic versioning")
PARSARG.add_argument("-b", "--build", action="store_true",
                     help="Build gtk-fortran libraries and examples")
PARSARG.add_argument("-d", "--deprecated", action="store_true",
                     help="Remove deprecated functions")
ARGS = PARSARG.parse_args()
GTK_VERSION = "gtk" + str(ARGS.gtk[0])
GTK_FORTRAN_VERSION = ARGS.version[0]

# An instance of the Version class:
my_versions = Version(GTK_VERSION, GTK_FORTRAN_VERSION)

# Define libraries paths and corresponding *-auto.* files.
# Do not change the order of the dictionary keys:
PATH_DICT = OrderedDict([
    ("/usr/include/cairo", "cairo-auto.f90"),
    ("/usr/include/gdk-pixbuf-2.0", "gdk-pixbuf-auto.f90"),
    ("/usr/include/glib-2.0", "glib-auto.f90")])
if GTK_VERSION == "gtk4":
    GTKENUMS_FILE = "gtkenums-auto.in"
    PATH_DICT.update([
        ("/usr/include/gtk-4.0/gdk", "gdk-auto.f90"),
        ("/usr/include/gtk-4.0/gsk", "gsk-auto.f90"),
        ("/usr/include/gtk-4.0/gtk", "gtk-auto.in"),
        ("/usr/include/gtk-4.0/unix-print", "unix-print-auto.f90"),
        ("/usr/include/graphene-1.0", "graphene-auto.f90")])
elif GTK_VERSION == "gtk3":
    GTKENUMS_FILE = "gtkenums-auto.f90"
    PATH_DICT.update([
        ("/usr/include/atk-1.0", "atk-auto.f90"),
        ("/usr/include/gtk-3.0/gdk", "gdk-auto.f90"),
        ("/usr/include/gtk-3.0/gtk", "gtk-auto.f90"),
        ("/usr/include/gtk-3.0/unix-print", "unix-print-auto.f90")])
elif GTK_VERSION == "gtk2":
    GTKENUMS_FILE = "gtkenums-auto.f90"
    PATH_DICT.update([
        ("/usr/include/atk-1.0", "atk-auto.f90"),
        ("/usr/include/gtk-2.0/gdk", "gdk-auto.f90"),
        ("/usr/include/gtk-2.0/gtk", "gtk-auto.f90")])
PATH_DICT.update([("/usr/include/pango-1.0", "pango-auto.f90")])

# To calculate computing time:
T0 = time.time()
# An instance of the Statistics class:
my_stats = Statistics()
# An instance of the Errors class:
my_errors = Errors()

#*************************************************************************
# Pass 1: scan all header files to find all enum types, all pointers to
# functions (funptr) and add derived GTK types
#*************************************************************************
print("\033[1m Pass 1: looking for enumerators, funptr and derived types...\033[0m")
# Just for initializing the class:
types_enums_initialisation = types_enums(PATH_DICT)

#**************************************************************************
# Pass 2: Scan of all header files in the directories and subdirectories to
# generate interfaces
#**************************************************************************
FILE_HEADER = """! Do not modify this file automatically generated by cfwrapper.py using:
! """ + my_versions.string() + """\n! This file is part of the gtk-fortran library, distributed under
! GNU General Public License version 3.
"""

# All enums are written in this file:
enums_file = open(SRC_DIR+GTKENUMS_FILE, "w", encoding='utf-8')
enums_file.write(FILE_HEADER+"\n")

# Index of all the generated Fortran interfaces:
index = []

print("\033[1m Pass 2: looking for C functions...\033[0m ")

# Note that PATH_DICT is an OrderedDict:
for library_path in PATH_DICT:
    # Name of the *-auto.* file:
    f_file_name = PATH_DICT[library_path]
    print(f"{library_path:<32} =>  {f_file_name:<20}", end="")

    # Create the *-auto.* file with its module declaration:
    f_file = open(SRC_DIR+f_file_name, "w", encoding='utf-8')

    # The gtk-auto.* file is a special case, it will be included in
    # the already existing gtk.f90 by an include statement:
    if "gtk-auto." in f_file_name:
        module_name = "gtk"
    else:
        # The module name is derived from the Fortran file name:
        module_name = re.search(r"^(.+)-auto\.f90", f_file_name).group(1)
        module_name = module_name.replace("-", "_")
        if module_name == "glib":
            module_name = "g"
        # Write the beginning of the .f90 file:
        f_file.write(FILE_HEADER+"\nmodule " + module_name +
                        "\nuse, intrinsic :: iso_c_binding\nimplicit none\ninterface\n\n")

    # Analyze each C header file in each subdirectory of that library:
    for directory in os.walk(library_path):
        for c_file_name in directory[2]:
            # Problematic files can be excluded here:
            #if c_file_name in ["this_file.h"]:
            #    continue    # Go to next file

            # Write the file name in comments:
            f_file.write("!" + 50*"-" + "\n")
            f_file.write("! " + directory[0] + "/" + c_file_name + "\n")
            f_file.write("!" + 50*"-" + "\n")

            my_stats.inc_nb_files()
            whole_file_original = open(directory[0] + "/" + c_file_name, 'r',
                                       errors='replace', encoding='utf-8').read()
            # The original file will be used for WIN32 functions
            whole_file = whole_file_original

            whole_file, nb_enums = clean_header_file(c_file_name, whole_file, enums_file)
            my_stats.inc_nb_enumerators(nb_enums)

            # From now each line will be treated separately:
            lines_list = whole_file.splitlines(True)
            preprocessed_list = []
            # Is there any function in lines_list ?
            try:
                preprocessed_list.append(lines_list[0])
            except IndexError:
                my_errors.new_error(directory[0], c_file_name,
                                    "No function to implement in this file", "",
                                    False)
                continue    # Go to next file

            # If true, we process these functions:
            preprocessed_list, nb = preprocess_prototypes(preprocessed_list,
                                                          lines_list)
            my_stats.inc_nb_lines(nb)

            if c_file_name in ["gstdio.h"]:
                # We remove possible duplicated prototypes in this file:
                preprocessed_list = list(set(preprocessed_list))
                preprocessed_list.sort()

            analyze_prototypes(index, module_name, f_file_name, f_file, preprocessed_list,
                               whole_file_original, directory[0], c_file_name,
                               my_stats, my_errors, ARGS)

    # Close that *-auto.f90 file:
    if module_name != "gtk":    # gtk module is included in gtk.f90
        f_file.write("end interface\nend module "+module_name+"\n")
        f_file.close()
    print(f"{os.stat(SRC_DIR+f_file_name).st_size:>10} bytes")
    # Next *-auto.* file


# Close global files:
enums_file.close()

# Write the list of all GTK functions in the index CSV file:
index.sort()
with open(SRC_DIR+"gtk-fortran-index.csv", "w", newline="", encoding="utf-8") as csvfile1:
    index_file = csv.writer(csvfile1, delimiter=";", dialect='excel')
    index_file.writerows(index)

# Write errors in a CSV file:
my_errors.sort()
with open("cfwrapper-errors.csv", "w", newline="", encoding="utf-8") as csvfile2:
    errors_file = csv.writer(csvfile2, delimiter=";", dialect='excel')
    errors_file.writerows(my_errors.errors_list)

print()

# Extracts the structure definitions for Gdk events
# and generate gdkevents_auto?.f90:
if GTK_VERSION != "gtk2":
    subprocess.call(["./extract_events.pl"], cwd=SRC_DIR)

# Write the VERSIONS file in the top directory,
# and update the codemeta.json and fpm.toml files:
my_versions.create_file()
my_versions.update_json_file()
my_versions.update_fpm_file()

# Print the final statistics:
my_stats.print(T0, my_versions.string(), PATH_DICT, GTKENUMS_FILE, my_errors)

if ARGS.build:
    # Build and test gtk-fortran with that interactive script:
    subprocess.run(["cd .. && ./build.sh"], shell=True)
