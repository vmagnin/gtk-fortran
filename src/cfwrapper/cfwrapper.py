#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2011
# Free Software Foundation, Inc.
#
# This file is part of the gtk-fortran GTK / Fortran Interface library.
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
# this program; see the files COPYING3 and COPYING.RUNTIME respectively.
# If not, see <http://www.gnu.org/licenses/>.
#
# Contributed by Vincent Magnin, 01.28.2011
# Last modification: 2019-04-03 (tested with Python 3.6.7, Ubuntu 18.10)
# pylint3 *.py : 8.33/10

""" Generates the *-auto.f90 files from the C header files of GLib and GTK.
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
# To use ../tools.py which contains the multiline() function:
sys.path.append('../')

# Project modules:
from globals_const import SRC_DIR
from lib_versions import gtk_fortran_version
from cleaning import clean_header_file, preprocess_prototypes
from errors import Errors
from stats import Statistics
from analyze import analyze_prototypes


# Definition of command line options:
PARSARG = argparse.ArgumentParser(description="Generate gtk-fortran files",
                                  epilog="GPLv3 license, https://github.com/vmagnin/gtk-fortran")
PARSARG.add_argument("-g", "--gtk", action="store", type=int, choices=[2, 3],
                     metavar="2|3", nargs=1, required=True,
                     help="GTK major version")
PARSARG.add_argument("-b", "--build", action="store_true",
                     help="Build gtk-fortran libraries and examples")
PARSARG.add_argument("-d", "--deprecated", action="store_true",
                     help="Remove deprecated functions")
ARGS = PARSARG.parse_args()
GTK_VERSION = "gtk" + str(ARGS.gtk[0])


# Define libraries paths and corresponding *-auto.f90 files.
# Do not change the order of the dictionary keys:
PATH_DICT = OrderedDict([
    ("/usr/include/atk-1.0", "atk-auto.f90"),
    ("/usr/include/cairo", "cairo-auto.f90"),
    ("/usr/include/gdk-pixbuf-2.0", "gdk-pixbuf-auto.f90"),
    ("/usr/include/glib-2.0", "glib-auto.f90")])
if GTK_VERSION == "gtk3":
    PATH_DICT.update([
        ("/usr/include/gtk-3.0/gdk", "gdk-auto.f90"),
        ("/usr/include/gtk-3.0/gtk", "gtk-auto.f90"),
        ("/usr/include/gtk-3.0/unix-print", "unix-print-auto.f90")])
else:
    PATH_DICT.update([
        ("/usr/include/gtk-2.0/gdk", "gdk-auto.f90"),
        ("/usr/include/gtk-2.0/gtk", "gtk-auto.f90")])
PATH_DICT.update([("/usr/include/pango-1.0", "pango-auto.f90")])


# -------------------------------------------------------------------------
# These dictionaries give the Fortran type and its KIND for each GTK type.
# TYPES_DICT will be completed with other types detected by the algorithm.
# -------------------------------------------------------------------------
# One word types:
TYPES_DICT = {
    "int":("integer(c_int)", "c_int"),
    "gint":("integer(c_int)", "c_int"),
    "guint":("integer(c_int)", "c_int"),
    #define Bool int    => Xlib.h
    "Bool":("integer(c_int)", "c_int"),
    "GPid":("integer(c_int)", "c_int"),
    "gint64":("integer(c_int64_t)", "c_int64_t"),
    "goffset":("integer(c_int64_t)", "c_int64_t"),
    "guint64":("integer(c_int64_t)", "c_int64_t"),
    "gint32":("integer(c_int32_t)", "c_int32_t"),
    "guint32":("integer(c_int32_t)", "c_int32_t"),
    #typedef guint32 GdkWChar;
    "GdkWChar":("integer(c_int32_t)", "c_int32_t"),
    #typedef uint32_t xcb_drawable_t;
    "xcb_drawable_t":("integer(c_int32_t)", "c_int32_t"),
    #typedef uint32_t xcb_pixmap_t;
    "xcb_pixmap_t":("integer(c_int32_t)", "c_int32_t"),
    #typedef __uid_t uid_t;
    "uid_t":("integer(c_int32_t)", "c_int32_t"),
    "gint16":("integer(c_int16_t)", "c_int16_t"),
    "guint16":("integer(c_int16_t)", "c_int16_t"),
    "gint8": ("integer(c_int8_t)", "c_int8_t"),
    "guint8": ("integer(c_int8_t)", "c_int8_t"),
    "long":("integer(c_long)", "c_long"),
    "gulong":("integer(c_long)", "c_long"),
    #typedef __time_t time_t;
    "time_t":("integer(c_long)", "c_long"),
    "short":("integer(c_short)", "c_short"),
    "boolean":("logical(c_bool)", "c_bool"),
    "char":("character(kind=c_char)", "c_char"),
    # For gchar & guchar,
    # see https://github.com/vmagnin/gtk-fortran/issues/41#issuecomment-7337877
    "gchar":("integer(kind=c_int8_t)", "c_int8_t"),
    "guchar":("integer(kind=c_int8_t)", "c_int8_t"),
    "double": ("real(c_double)", "c_double"),
    "float":("real(c_float)", "c_float"),
    #typedef unsigned long gsize;   also GType
    "gsize":  ("integer(c_size_t)", "c_size_t"),
    #typedef signed long gssize;
    "gssize":  ("integer(c_size_t)", "c_size_t"),
    "GType":  ("integer(c_size_t)", "c_size_t"),
    "size_t":  ("integer(c_size_t)", "c_size_t"),
    "va_list":("type(c_ptr)", "c_ptr"),
    #typedef void* gpointer;
    "gpointer":("type(c_ptr)", "c_ptr"),
    #typedef struct _GdkAtom *GdkAtom;
    "GdkAtom":("type(c_ptr)", "c_ptr"),
    # GC (Xlib) is it a pointer ?
    "GC":("type(c_ptr)", "c_ptr"),
    #typedef struct _GIConv *GIConv;
    "GIConv":("type(c_ptr)", "c_ptr"),
    "GSignalCMarshaller":("type(c_ptr)", "c_ptr"),
    #typedef struct FT_FaceRec_*  FT_Face;
    "FT_Face":("type(c_ptr)", "c_ptr"),
    # X11 types (See /usr/include/X11/Xmd.h), unsigned int (64 bits archi.)
    # or unsigned long (32 bits architecture):
    "Window":("integer(c_long)", "c_long"),
    #define Drawable CARD32
    "Drawable":("integer(c_long)", "c_long"),
    "Font":("integer(c_long)", "c_long"),
    "Pixmap":("integer(c_long)", "c_long"),
    "Cursor":("integer(c_long)", "c_long"),
    "Colormap":("integer(c_long)", "c_long"),
    "GContext":("integer(c_long)", "c_long"),
    "Atom":("integer(c_long)", "c_long"),
    "Picture":("integer(c_long)", "c_long"),
    "XID":("integer(c_long)", "c_long"),
    "VisualID":("integer(c_long)", "c_long"),
    "Time":("integer(c_long)", "c_long"),
    #define KeyCode CARD8   => unsigned char
    "KeyCode":("character(kind=c_char)", "c_char"),
    "KeySym":("integer(c_long)", "c_long"),
    # enum GWin32OSType
    "GWin32OSType":("integer(c_int)", "c_int")
}

# Two words types:
TYPES2_DICT = {
    "long double": ("real(c_long_double)", "c_long_double"),
    "unsigned long":("integer(c_long)", "c_long"),
    "unsigned short":("integer(c_short)", "c_short"),
    "unsigned int":("integer(c_int)", "c_int")
}

# An instance of the Statistics class:
my_stats = Statistics()

# An instance of the Errors class:
my_errors = Errors()

#*************************************************************************
# Pass 1: scan all header files to find all enum types, all pointers to
# functions (funptr) and add derived GTK types
#*************************************************************************
print("\033[1m Pass 1: looking for enumerators, funptr and derived types...\033[0m")

gtk_types = []

# These lists will be used by the iso_c_binding() function:
gtk_enums = []
gtk_funptr = []

T0 = time.time()     # To calculate computing time
for library_path in PATH_DICT:
    for directory in os.walk(library_path):
        for c_file_name in directory[2]:
            whole_file = open(directory[0] + "/" + c_file_name, 'r',
                              errors='replace').read()
            gtk_enums += re.findall(r"(?ms)^typedef enum.*?}\s?(\w+);", whole_file)
            gtk_funptr += re.findall(r"(?m)^typedef[ \t]*(?:const)?[ \t]*\w+[ \t]*\*?\s*\(\* ?([\w]*?)\)",
                                     whole_file)
            gtk_types += re.findall(r"(?m)^typedef *?(?:const)? *?(\w+) *\*? *([\w]+);",
                                    whole_file)

# Add derived types:
for each in gtk_types:
    if each[1] not in TYPES_DICT:
        if each[0] in TYPES_DICT:
            TYPES_DICT[each[1]] = TYPES_DICT[each[0]]
        elif each[0] in gtk_funptr:
            TYPES_DICT[each[1]] = ("type(c_funptr)", "c_funptr")

# Sorting (useful only for printing):
gtk_enums.sort()
gtk_funptr.sort()

#**************************************************************************
# Pass 2: Scan of all header files in the directories and subdirectories to
# generate interfaces
#**************************************************************************
FILE_HEADER = """! Do not modify this file automatically generated by cfwrapper.py using:
! """ + gtk_fortran_version(GTK_VERSION) + """\n! This file is part of the gtk-fortran library, distributed under
! GNU General Public License version 3.
"""

# All enums are written in this file:
enums_file = open(SRC_DIR+"gtkenums-auto.f90", "w")
enums_file.write(FILE_HEADER+"\n")

# Files for platform specific functions:
HEAD = "\nmodule " + "gtk_os_dependent" + "\nimplicit none\ninterface\n\n"
unix_only_file = open(SRC_DIR+"unixonly-auto.f90", "w")
unix_only_file.write(FILE_HEADER + HEAD)
mswindows_only_file = open(SRC_DIR+"mswindowsonly-auto.f90", "w")
mswindows_only_file.write(FILE_HEADER + HEAD)

# Index of all the generated Fortran interfaces:
index = []

opened_files = []

print("\033[1m Pass 2: looking for C functions...\033[0m ")

for library_path in PATH_DICT:
    # Name of the *-auto.f90 file:
    f_file_name = PATH_DICT[library_path]
    print('{:<32}{}{:<20}'.format(library_path, " =>  ", f_file_name), end="")

    # Create the *-auto.f90 file with its module declaration:
    if f_file_name not in opened_files:
        f_file = open(SRC_DIR+f_file_name, "w")
        opened_files.append(f_file_name)

        # The module name is derived from the Fortran file name:
        module_name = re.search(r"^(.+)-auto\.f90", f_file_name).group(1)
        module_name = module_name.replace("-", "_")

        # The gtk-auto.f90 file is a special case, it will be included in
        # the already existing gtk.f90 by an include statement:
        if module_name != "gtk":
            if module_name == "glib":
                module_name = "g"
            # Write the beginning of the .f90 file:
            f_file.write(FILE_HEADER+"\nmodule " + module_name +
                         "\nimplicit none\ninterface\n\n")

    # Analyze each C header file in each subdirectory of that library:
    for directory in os.walk(library_path):
        for c_file_name in directory[2]:
            # Problematic files can be excluded here:
            #if c_file_name in ["this_file.h"]:
            #    continue    # Go to next file

            my_stats.inc_nb_files()
            whole_file_original = open(directory[0] + "/" + c_file_name, 'r',
                                       errors='replace').read()
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

            analyze_prototypes(index, module_name, f_file_name, unix_only_file,
                               mswindows_only_file, f_file, preprocessed_list,
                               whole_file_original, directory[0], c_file_name,
                               gtk_enums, gtk_funptr, TYPES_DICT, TYPES2_DICT,
                               my_stats, my_errors, ARGS)

    # Close that *-auto.f90 file:
    if module_name != "gtk":    # gtk module is included in gtk.f90
        f_file.write("end interface\nend module "+module_name+"\n")
        f_file.close()
    print('{:>10}{}'.format(os.stat(SRC_DIR+f_file_name).st_size, " bytes"))
    # Next *-auto.f90 file


# Close global files:
enums_file.close()
TAIL = "end interface\nend module "+"gtk_os_dependent"+"\n"
unix_only_file.write(TAIL)
unix_only_file.close()
mswindows_only_file.write(TAIL)
mswindows_only_file.close()

# Write the list of all GTK functions in the index CSV file:
index.sort()
index_file = csv.writer(open(SRC_DIR+"gtk-fortran-index.csv", "w"), delimiter=";")
index_file.writerows(index)

# Write errors in a CSV file:
my_errors.sort()
errors_file = csv.writer(open("cfwrapper-errors.csv", "w"), delimiter=";")
errors_file.writerows(my_errors.errors_list)

# Print the final statistics:
my_stats.print(T0, GTK_VERSION, PATH_DICT, TYPES_DICT, TYPES2_DICT, my_errors)

if ARGS.build:
    # Extracts the structure definitions for Gdk events
    # and generate gdkevents_auto?.f90:
    if GTK_VERSION != "gtk2":
        subprocess.call(["./extract_events.pl"])
    # Build the gtk-fortran project using CMake:
    subprocess.call(["./build.sh"])
