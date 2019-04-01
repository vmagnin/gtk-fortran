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
# Last modification: 2019-04-01 (tested with Python 3.6.7, Ubuntu 18.10)
# pylint3 score: 7.18/10

""" Generates the *-auto.f90 files from the C header files of GLib and GTK.
For help, type: ./cfwrapper.py -h
"""

import re           # Regular expression library
import os
import time
import csv          # To write .csv files
import platform     # To obtain platform informations
import subprocess   # To launch a shell command
import argparse     # To parse command line
from collections import OrderedDict

# Project modules:
from globals_const import *
from lib_versions import gtk_fortran_version
from stats import print_statistics
from tools import multiline
from enums import translate_enums
from fortran import iso_c_binding
from cleaning import clean_header_file


def write_error(direc, filename, message, proto, type_error):
    """ Write errors in a list and increments the counters """
    global nb_errors
    global nb_type_errors
    global errors_list

    errors_list.append([direc + "/" + filename, message, proto])

    if type_error:
        nb_type_errors += 1
    else:
        nb_errors += 1


def preprocess_prototypes():
    """Clean the list of prototypes before analysis
    """
    global nb_lines
    global preprocessed_list

    i = 0
    for prototype in lines_list:
        nb_lines += 1
        # remove leading and trailing spaces:
        prototype2 = prototype.strip()

        if ";" not in preprocessed_list[i]:
            # Remove line feeds inside a prototype:
            preprocessed_list[i] = preprocessed_list[i].replace("\n", "").strip()
            preprocessed_list[i] += " "+prototype2
        else:
            preprocessed_list.append(prototype2)
            i += 1

        preprocessed_list[i] = preprocessed_list[i].strip()


def analyze_prototypes(gtk_enums, gtk_funptr, TYPES_DICT, TYPES2_DICT):
    """Each prototype is now analyzed
    """
    global nb_variadic, nb_deprecated_functions

    for proto in preprocessed_list:
        error_flag = False

        # Do not treat variadic functions:
        if "..." in proto:
            nb_variadic += 1
            write_error(directory[0], c_file_name,
                        "Variadic function", proto, False)
            continue    # Next prototype

        type_returned = RGX_RETURNED_TYPE.search(proto)
        try:
            function_type = type_returned.group(1)
        except AttributeError:
            write_error(directory[0], c_file_name,
                        "Returned type not found", proto, False)
            continue    # Next prototype
        if function_type == " ":
            write_error(directory[0], c_file_name,
                        "Returned type not found", proto, False)
            continue    # Next prototype

        # Will it be a Fortran function or a subroutine ?
        if ("void" in function_type) and ("*" not in function_type):
            f_procedure = "subroutine "
            f_the_end = "end subroutine"
            isfunction = False
            f_use = ""
            returned_type = ""
        else:
            f_procedure = "function "
            f_the_end = "end function"
            isfunction = True
            returned_type, iso_c = iso_c_binding(function_type, True, gtk_enums, gtk_funptr, TYPES_DICT, TYPES2_DICT)
            f_use = iso_c
            if "?" in returned_type:    # Function type not found
                error_flag = True
                write_error(directory[0], c_file_name, "Unknown type:  "
                            + function_type, proto, True)
                continue

        # f_name will contain the name of the function in gtk-fortran:
        function_name = RGX_FUNCTION_NAME.search(proto)
        try:
            f_name = function_name.group(1)
        except AttributeError:
            write_error(directory[0], c_file_name,
                        "Function name not found", proto, False)
            continue    # Next prototype

        # gtk_init() is already defined in gtk.f90. Other functions
        # can be excluded here in case of problem:
        if f_name in ["gtk_init", "g_io_channel_win32_new_messages"]:
            continue    # Next prototype

        # Functions beginning by an underscore will be excluded:
        if RGX_UNDERSCORE.match(f_name) is not None:
            write_error(directory[0], c_file_name,
                        "Function name beginning by underscore", proto, False)
            continue    # Next prototype

        # What is the status of that function ? (Is the C prototype preceded
        # on the previous line by a DEPRECATED or AVAILABLE statement ?)
        status = re.search(r"(?m)^(.*?(DEPRECATED|AVAILABLE).*?)\n.*?"+f_name+r"\W", whole_file_original)
        if status:
            function_status = status.group(1)
            if "DEPRECATED" in function_status:
                nb_deprecated_functions += 1
                # The `-d` argument can be useful to adapt gtk-fortran for
                # major updates. The `make -i` command will then generate errors
                # when a deprecated function is not found:
                if ARGS.deprecated:
                    continue
        else:
            function_status = ""

        # Searching the function arguments:
        arguments = RGX_ARGUMENTS.search(proto)
        try:
            args = RGX_ARGS.findall(arguments.group(1))
        except AttributeError:
            write_error(directory[0], c_file_name,
                        "Problem determining the arguments", proto, False)
            continue    # Next prototype

        # Each argument of the function is analyzed:
        declarations = ""
        args_list = ""
        for arg in args:
            if arg == "void":
                continue

            # Can we find the type of the argument ? The var_type variable is
            # not used elsewhere, but this test is compulsory. Do not remove.
            try:
                var_type = RGX_VAR_TYPE.search(arg).group(1)
            except AttributeError:
                write_error(directory[0], c_file_name,
                            "Variable type not found", proto, True)
                continue    # Next argument

            # Corresponding Fortran type of the argument:
            f_type, iso_c = iso_c_binding(arg, False, gtk_enums, gtk_funptr, TYPES_DICT, TYPES2_DICT)
            if iso_c not in used_types:
                used_types.append(iso_c)

            if "c_" in f_type:
                # Determine iso_c type to use:
                if f_use == "":
                    f_use = iso_c
                else:
                    # Verify that each iso_c appear only once:
                    RGX_ISO_C = re.compile("("+iso_c+")"+r"([^\w]|$)")
                    if RGX_ISO_C.search(f_use) is None:
                        f_use += ", " + iso_c
            elif "?" in f_type:     # Type not found
                error_flag = True
                write_error(directory[0], c_file_name, "Unknown type:  " + arg,
                            proto, True)

            # Search the variable name:
            try:
                var_name = RGX_VAR_NAME.search(arg).group(1)
            except AttributeError:
                write_error(directory[0], c_file_name,
                            "Variable name not found", proto, False)
                continue    # Next argument

            # Unknown dimension arrays are passed by address, others by value:
            if "(*)" in f_type:
                passvar = ""
            else:
                passvar = ", value"

            # Add this variable:
            if args_list == "":
                args_list = var_name
            else:
                args_list += ", " + var_name

            declarations += 1*TAB + f_type + passvar + " :: " + var_name + "\n"

        # Write the Fortran interface in the .f90 file:
        if not error_flag:
            write_fortran_interface(function_status, proto, f_procedure, f_name, args_list, f_use, declarations, isfunction, returned_type, f_the_end)


def write_fortran_interface(function_status, prototype, f_procedure, f_name, args_list, f_use, declarations, isfunction, returned_type, f_the_end):
    """Write the Fortran interface of a function in the *-auto.f90 file
    """
    global index
    global nb_generated_interfaces
    global nb_win32_utf8

    interface1 = 0*TAB + "! " + function_status + "\n"
    interface1 += 0*TAB + "!" + prototype + "\n"
    first_line = 0*TAB + f_procedure + f_name + "(" + args_list + ") bind(c)"
    interface2 = multiline(first_line, 80) + "\n"
    interface3 = 1*TAB + "use iso_c_binding, only: " + f_use + "\n"
    if isfunction:
        interface3 += 1*TAB + returned_type + " :: " + f_name + "\n"
    interface3 += declarations
    interface3 += 0*TAB + f_the_end + "\n\n"
    interface = interface1+interface2+interface3

    # Names for the gtk-fortran-index.csv file:
    my_module_name = module_name
    my_f_file_name = f_file_name
    my_first_line  = first_line

    # For Win32 _utf8 functions, the normal form and the Windows form must be
    # dispatched in two platform dependent files, with the same module name.
    # Some _utf8 functions are defined by a #define, and others declared.
        # gdk_pixbuf_new_from_file_utf8
        # gdk_pixbuf_new_from_file_at_size_utf8
        # gdk_pixbuf_new_from_file_at_scale_utf8
        # gdk_pixbuf_savev
        # Historically these functions were defined by #define for win32,
        # so we maintain them in the gtk_os_dependent modules because
        # the gdk-pixbuf-hl.f90 uses them.
        # PROBABLY OTHER FUNCTIONS ARE IN THE SAME CASE.
        # IN THE FUTURE, gdk-pixbuf-hl.f90 COULD BE INSTEAD MODIFIED.

    if ((("gdk_pixbuf_new_from_file" in f_name) or ("gdk_pixbuf_savev" in f_name)) and ("_utf8" not in f_name)) or re.search(r"(?m)^#define\s+"+f_name+r"\s+"+f_name+r"_utf8\s*$", whole_file_original):
        # The re.search() test is for GTK 2.
        # In recent versions of GTK 3, there is no more functions defined
        # like this (verified in GTK 3.24.4).

        unix_only_file.write(interface)

        my_module_name = "gtk_os_dependent"
        my_f_file_name = "unixonly-auto.f90/mswindowsonly-auto.f90"

        first_line = 0*TAB + f_procedure + f_name + "(" + args_list + ") bind(c, name='"+f_name+"_utf8')"
        interface2_utf8 = multiline(first_line, 80) + "\n"

        mswindows_only_file.write(interface1+interface2_utf8+interface3)

        nb_generated_interfaces += 2
        nb_win32_utf8 += 1
    else: # Non platform specific functions
        f_file.write(interface)
        nb_generated_interfaces += 1

    # Adds the function in the gtk-fortran-index.csv file:
    index.append([my_module_name, f_name, function_status, my_f_file_name,
                  directory[0]+"/"+c_file_name, prototype, my_first_line])


# ****************************************************************************
# *****************************  MAIN PROGRAM  *******************************
# ****************************************************************************
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
    "Bool":("integer(c_int)", "c_int"),    #define Bool int    => Xlib.h
    "GPid":("integer(c_int)", "c_int"),
    "gint64":("integer(c_int64_t)", "c_int64_t"),
    "goffset":("integer(c_int64_t)", "c_int64_t"),
    "guint64":("integer(c_int64_t)", "c_int64_t"),
    "gint32":("integer(c_int32_t)", "c_int32_t"),
    "guint32":("integer(c_int32_t)", "c_int32_t"),
    "GdkWChar":("integer(c_int32_t)", "c_int32_t"),      #typedef guint32 GdkWChar;
    "xcb_drawable_t":("integer(c_int32_t)", "c_int32_t"),  #typedef uint32_t xcb_drawable_t;
    "xcb_pixmap_t":("integer(c_int32_t)", "c_int32_t"),  #typedef uint32_t xcb_pixmap_t;
    "uid_t":("integer(c_int32_t)", "c_int32_t"),         #typedef __uid_t uid_t;
    "gint16":("integer(c_int16_t)", "c_int16_t"),
    "guint16":("integer(c_int16_t)", "c_int16_t"),
    "gint8": ("integer(c_int8_t)", "c_int8_t"),
    "guint8": ("integer(c_int8_t)", "c_int8_t"),
    "long":("integer(c_long)", "c_long"),
    "gulong":("integer(c_long)", "c_long"),
    "time_t":("integer(c_long)", "c_long"),  #typedef __time_t time_t;
    "short":("integer(c_short)", "c_short"),
    "boolean":("logical(c_bool)", "c_bool"),
    "char":("character(kind=c_char)", "c_char"),
    # For gchar & guchar, see https://github.com/vmagnin/gtk-fortran/issues/41#issuecomment-7337877
    "gchar":("integer(kind=c_int8_t)", "c_int8_t"),   #("character(kind=c_char)", "c_char"),
    "guchar":("integer(kind=c_int8_t)", "c_int8_t"),  #("character(kind=c_char)", "c_char"),
    "double": ("real(c_double)", "c_double"),
    "float":("real(c_float)", "c_float"),
    "gsize":  ("integer(c_size_t)", "c_size_t"),    #typedef unsigned long gsize;   also GType
    "gssize":  ("integer(c_size_t)", "c_size_t"),   #typedef signed long gssize;
    "GType":  ("integer(c_size_t)", "c_size_t"),
    "size_t":  ("integer(c_size_t)", "c_size_t"),
    "va_list":("type(c_ptr)", "c_ptr"),
    "gpointer":("type(c_ptr)", "c_ptr"), #typedef void* gpointer;
    "GdkAtom":("type(c_ptr)", "c_ptr"),  #typedef struct _GdkAtom *GdkAtom;
    "GC":("type(c_ptr)", "c_ptr"),       # GC (Xlib) is it a pointer ?
    "GIConv":("type(c_ptr)", "c_ptr"),   #typedef struct _GIConv *GIConv;
    "GSignalCMarshaller":("type(c_ptr)", "c_ptr"),
    "FT_Face":("type(c_ptr)", "c_ptr"),  #typedef struct FT_FaceRec_*  FT_Face;
    # X11 types (See /usr/include/X11/Xmd.h), unsigned int (64 bits archi.)
    # or unsigned long (32 bits architecture):
    "Window":("integer(c_long)", "c_long"),
    "Drawable":("integer(c_long)", "c_long"),  #define Drawable CARD32
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
    "KeyCode":("character(kind=c_char)", "c_char"),   #define KeyCode CARD8   => unsigned char
    "KeySym":("integer(c_long)", "c_long"),
    "GWin32OSType":("integer(c_int)", "c_int")     # enum GWin32OSType
}

# Two words types:
TYPES2_DICT = {
    "long double": ("real(c_long_double)", "c_long_double"),
    "unsigned long":("integer(c_long)", "c_long"),
    "unsigned short":("integer(c_short)", "c_short"),
    "unsigned int":("integer(c_int)", "c_int")
}

#---------------------------------------------------------------------------
# Regular expressions used to identify the different parts of a C prototype:
#---------------------------------------------------------------------------
# Type of a function:
RGX_RETURNED_TYPE = re.compile(r"^ *([_0-9a-zA-Z ]+ *\**)")
# Name of the function/subroutine:
RGX_FUNCTION_NAME = re.compile(r"([0-9a-zA-Z_]+) *\(")
# All the arguments of the function:
RGX_ARGUMENTS = re.compile(r"\(([0-9a-zA-Z_ ,\*\[\]]*)\).*;$")
# To list each argument:
RGX_ARGS = re.compile(r" *([0-9a-zA-Z_ \*\[\]]+),?")
# To find the type of an argument:
RGX_VAR_TYPE = re.compile(r" *([_0-9a-zA-Z]+)[ |\*]")
# To find the name of an argument:
RGX_VAR_NAME = re.compile(r"[ |\*]([_0-9a-zA-Z]+)(?:\[\])?$")
# Function name beginning by an underscore:
RGX_UNDERSCORE = re.compile(r"^_\w+$")


# Statistics initialization:
nb_lines = 0
nb_generated_interfaces = 0
nb_deprecated_functions = 0
nb_errors = 0
nb_type_errors = 0
nb_variadic = 0
nb_files = 0
nb_enumerators = 0
nb_win32_utf8 = 0


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

index = []
opened_files = []
errors_list = []
used_types = []

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

            nb_files += 1
            whole_file_original = open(directory[0] + "/" + c_file_name, 'r',
                                       errors='replace').read()
            # The original will be used for WIN32 functions
            whole_file = whole_file_original

            whole_file, nb_enums = clean_header_file(c_file_name, whole_file, enums_file)
            nb_enumerators += nb_enums

            # From now each line will be treated separately:
            lines_list = whole_file.splitlines(True)
            preprocessed_list = []
            # Is there any function in lines_list ?
            try:
                preprocessed_list.append(lines_list[0])
            except IndexError:
                write_error(directory[0], c_file_name,
                            "No function to implement in this file", "", False)
                continue    # Go to next file

            # If true, we process these functions:
            preprocess_prototypes()
            if c_file_name in ["gstdio.h"]:
                # We remove possible duplicated prototypes:
                preprocessed_list = list(set(preprocessed_list))
                preprocessed_list.sort()

            analyze_prototypes(gtk_enums, gtk_funptr, TYPES_DICT, TYPES2_DICT)

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
# Write list of GTK functions in a CSV file:
index.sort()
index_file = csv.writer(open(SRC_DIR+"gtk-fortran-index.csv", "w"), delimiter=";")
index_file.writerows(index)
# Write errors in a CSV file:
errors_list.sort()
errors_file = csv.writer(open("cfwrapper-errors.csv", "w"), delimiter=";")
errors_file.writerows(errors_list)


print_statistics(T0, GTK_VERSION, PATH_DICT, TYPES_DICT, TYPES2_DICT, nb_lines,
                 nb_generated_interfaces, nb_deprecated_functions, nb_errors,
                 nb_type_errors, nb_variadic, nb_files, nb_enumerators,
                 nb_win32_utf8, used_types)

if ARGS.build:
    # Extracts the structure definitions for Gdk events
    # and generate gdkevents_auto?.f90:
    if GTK_VERSION != "gtk2":
        subprocess.call(["./extract_events.pl"])
    # Build the gtk-fortran project using CMake:
    subprocess.call(["./build.sh"])
