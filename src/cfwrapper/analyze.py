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
# this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
# If not, see <http://www.gnu.org/licenses/>.
#
# Contributed by Vincent Magnin, 01.28.2011
# Last modification: 2019-04-02

""" This module contains functions to analyze C prototypes 
    and generate Fortran interfaces.
"""

import re           # Regular expression library

# Project modules:
from globals_const import TAB
from fortran import iso_c_binding
from tools import multiline

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

# TODO: these procedures need refactoring !

def analyze_prototypes(index, module_name, f_file_name, unix_only_file,
                       mswindows_only_file, f_file, preprocessed_list,
                       whole_file_original, c_dir, c_file_name, gtk_enums,
                       gtk_funptr, TYPES_DICT, TYPES2_DICT, my_stats, my_errors,
                       ARGS):
    """Each prototype of the preprocessed list is now analyzed.
    """

    for proto in preprocessed_list:
        error_flag = False

        # Do not treat variadic functions:
        if "..." in proto:
            my_stats.inc_nb_variadic()
            my_errors.new_error(c_dir, c_file_name,
                                "Variadic function", proto, False)
            continue    # Next prototype

        type_returned = RGX_RETURNED_TYPE.search(proto)
        try:
            function_type = type_returned.group(1)
        except AttributeError:
            my_errors.new_error(c_dir, c_file_name,
                                "Returned type not found", proto, False)
            continue    # Next prototype
        if function_type == " ":
            my_errors.new_error(c_dir, c_file_name,
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
            returned_type, iso_c = iso_c_binding(function_type, True, gtk_enums,
                                                 gtk_funptr, TYPES_DICT, TYPES2_DICT)
            f_use = iso_c
            if "?" in returned_type:    # Function type not found
                error_flag = True
                my_errors.new_error(c_dir, c_file_name, "Unknown type:  "
                                    + function_type, proto, True)
                continue

        # f_name will contain the name of the function in gtk-fortran:
        function_name = RGX_FUNCTION_NAME.search(proto)
        try:
            f_name = function_name.group(1)
        except AttributeError:
            my_errors.new_error(c_dir, c_file_name,
                                "Function name not found", proto, False)
            continue    # Next prototype

        # gtk_init() is already defined in gtk.f90. Other functions
        # can be excluded here in case of problem:
        if f_name in ["gtk_init", "g_io_channel_win32_new_messages"]:
            continue    # Next prototype

        # Functions beginning by an underscore will be excluded:
        if RGX_UNDERSCORE.match(f_name) is not None:
            my_errors.new_error(c_dir, c_file_name,
                                "Function name beginning by underscore", proto, False)
            continue    # Next prototype

        # What is the status of that function ? (Is the C prototype preceded
        # on the previous line by a DEPRECATED or AVAILABLE statement ?)
        status = re.search(r"(?m)^(.*?(DEPRECATED|AVAILABLE).*?)\n.*?" + f_name
                           + r"\W", whole_file_original)
        if status:
            function_status = status.group(1)
            if "DEPRECATED" in function_status:
                my_stats.inc_nb_deprecated_functions()
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
            my_errors.new_error(c_dir, c_file_name,
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
                my_errors.new_error(c_dir, c_file_name,
                                    "Variable type not found", proto, True)
                continue    # Next argument

            # Corresponding Fortran type of the argument:
            f_type, iso_c = iso_c_binding(arg, False, gtk_enums, gtk_funptr,
                                          TYPES_DICT, TYPES2_DICT)
            if iso_c not in my_stats.used_types:
                my_stats.append_type(iso_c)

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
                my_errors.new_error(c_dir, c_file_name, "Unknown type:  " + arg,
                                    proto, True)

            # Search the variable name:
            try:
                var_name = RGX_VAR_NAME.search(arg).group(1)
            except AttributeError:
                my_errors.new_error(c_dir, c_file_name,
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
            write_fortran_interface(index, module_name, f_file_name,
                                    unix_only_file, mswindows_only_file, f_file,
                                    whole_file_original, c_dir, c_file_name,
                                    function_status, proto, f_procedure, f_name,
                                    args_list, f_use, declarations, isfunction,
                                    returned_type, f_the_end, my_stats)


def write_fortran_interface(index, module_name, f_file_name, unix_only_file,
                            mswindows_only_file, f_file, whole_file_original,
                            c_dir, c_file_name, function_status, prototype,
                            f_procedure, f_name, args_list, f_use, declarations,
                            isfunction, returned_type, f_the_end, my_stats):
    """Write the Fortran interface of a function in the *-auto.f90 file
    """
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
    my_first_line = first_line

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

    if ((("gdk_pixbuf_new_from_file" in f_name) or ("gdk_pixbuf_savev" in f_name))
            and ("_utf8" not in f_name)) or re.search(r"(?m)^#define\s+" + f_name
                                                      + r"\s+"+f_name+r"_utf8\s*$",
                                                      whole_file_original):
        # The re.search() test is for GTK 2.
        # In recent versions of GTK 3, there is no more functions defined
        # like this (verified in GTK 3.24.4).

        unix_only_file.write(interface)

        my_module_name = "gtk_os_dependent"
        my_f_file_name = "unixonly-auto.f90/mswindowsonly-auto.f90"

        first_line = 0*TAB + f_procedure + f_name + "(" + args_list + ") bind(c, name='"+f_name+"_utf8')"
        interface2_utf8 = multiline(first_line, 80) + "\n"

        mswindows_only_file.write(interface1+interface2_utf8+interface3)

        my_stats.inc_nb_generated_interfaces(2)
        my_stats.inc_nb_win32_utf8()
    else: # Non platform specific functions
        f_file.write(interface)
        my_stats.inc_nb_generated_interfaces(1)

    # Adds the function in the gtk-fortran-index.csv file:
    index.append([my_module_name, f_name, function_status, my_f_file_name,
                  c_dir+"/"+c_file_name, prototype, my_first_line])
