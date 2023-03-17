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
# Last modifications: 2023-03-17

""" This module contains functions to analyze C prototypes
    and generate Fortran interfaces.
"""

import re           # Regular expression library

# Project modules:
from tools import multiline
from globals_const import TAB
from fortran import iso_c_binding

#---------------------------------------------------------------------------
# Regular expressions used to identify the different parts of a C prototype:
#---------------------------------------------------------------------------
# Type of a function: at the start of the line, possibly a few spaces, a few
# words possibly followed by a few spaces and/or a few stars (pointers)
RGX_RETURNED_TYPE = re.compile(r"^ *([_0-9a-zA-Z ]+ *\**)")
# Name of the function: a word, possibly between parentheses, possibly followed
# by a few spaces, and a final opening parenthesis
RGX_FUNCTION_NAME = re.compile(r"\(?([0-9a-zA-Z_]+)\)? *\($")
# All the arguments of the function: parentheses at the end of the line,
# possibly preceded by a function name between parentheses,
# possibly followed by characters, inside the parentheses possibly a few
# words separated by commas and possibly followed by stars and/or [] (arrays)
RGX_ARGUMENTS = re.compile(r"\(?.*\)? *\(([0-9a-zA-Z_ ,\*\[\]]*)\).*;$")
# To list each argument: possibly a few spaces, a word possibly followed by
# stars (pointer) and/or [] (arrays), possibly followed by a comma
RGX_ARGS = re.compile(r" *([0-9a-zA-Z_ \*\[\]]+),?")
# To find the type of an argument: possibly a few spaces, a word followed by
# a space or a star
RGX_VAR_TYPE = re.compile(r" *([_0-9a-zA-Z]+)[ |\*]")
# To find the name of an argument: space or a star, a word, possibly followed
# by [], at the end of the string
RGX_VAR_NAME = re.compile(r"[ |\*]([_0-9a-zA-Z]+)(?:\[\])?$")
# Function name beginning by an underscore:
RGX_UNDERSCORE = re.compile(r"^_\w+$")


def split_prototype(prototype):
    """The prototype string is splitted into three parts: the returned type, the name of the
       function and the list of arguments. If a problem occurs, an exception is raised
       with an error message.
    """

    arguments = RGX_ARGUMENTS.search(prototype)
    try:
        args = RGX_ARGS.findall(arguments.group(1))
    except AttributeError:
        raise Exception("Arguments not found")

    # The name is before arguments. The right bound of a string is excluded.
    function_name = RGX_FUNCTION_NAME.search(prototype[0:arguments.start(1)])
    try:
        f_name = function_name.group(1)
    except AttributeError:
        raise Exception("Function name not found")

    # The returned type is before the name. The right bound is excluded.
    type_returned = RGX_RETURNED_TYPE.search(prototype[0:function_name.start(1)])
    try:
        function_type = type_returned.group(1)
    except AttributeError:
        raise Exception("Returned type not found")

    return function_type, f_name, args


def analyze_prototypes(index, module_name, f_file_name, f_file, preprocessed_list,
                       whole_file_original, c_dir, c_file_name, gtk_enums,
                       gtk_funptr, TYPES_DICT, TYPES2_DICT, my_stats, my_errors,
                       ARGS):
    """Each prototype of the preprocessed list is now analyzed.
    """

    for proto in preprocessed_list:
        # C functions must have parentheses:
        if not "(" in proto:
            my_errors.new_error(c_dir, c_file_name,
                                "Not a function (no parentheses)", proto, False)
            continue    # Go to next prototype in the list

        # Do not treat variadic C functions:
        if "..." in proto:
            my_stats.inc_nb_variadic()
            my_errors.new_error(c_dir, c_file_name,
                                "Variadic function", proto, False)
            continue    # Go to next prototype in the list

        # Split the prototype into its three parts:
        try:
            function_type, f_name, args = split_prototype(proto)
        except Exception as exc:
            my_errors.new_error(c_dir, c_file_name, str(exc), proto, False)
            continue    # Go to next prototype in the list

        # gtk_init() is already defined in gtk.f90. Other functions
        # can be excluded here in case of problem:
        if f_name in ["gtk_init", "g_io_channel_win32_new_messages"]:
            continue    # Go to next prototype in the list

        # Functions beginning by an underscore will be excluded:
        if RGX_UNDERSCORE.match(f_name) is not None:
            my_errors.new_error(c_dir, c_file_name,
                                "Function name beginning by underscore", proto, False)
            continue    # Go to next prototype in the list

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
            # The function iso_c_binding() is working on a declaration
            # comprising the type and the name of the entity:
            declaration = function_type + " " + f_name
            returned_type, iso_c = iso_c_binding(declaration, True, gtk_enums,
                                            gtk_funptr, TYPES_DICT, TYPES2_DICT)
            f_use = iso_c
            if "?" in returned_type:    # Function type not found
                my_errors.new_error(c_dir, c_file_name, "Unknown function type:  "
                                    + function_type, proto, True)
                continue    # Go to next prototype in the list

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
                    continue    # Go to next prototype in the list
        else:
            function_status = ""

        # The list `args` of the function arguments is now analyzed:
        args_list, f_use, declarations, error_flag = analyze_arguments(args, f_use, proto, c_dir, c_file_name, gtk_enums,
                                                                       gtk_funptr, TYPES_DICT, TYPES2_DICT, my_stats, my_errors)
        # Write the Fortran interface in the .f90 file:
        if not error_flag:
            write_fortran_interface(index, module_name, f_file_name, f_file,
                                    c_dir, c_file_name,
                                    function_status, proto, f_procedure, f_name,
                                    args_list, f_use, declarations, isfunction,
                                    returned_type, f_the_end, my_stats)



def analyze_arguments(args, f_use, proto, c_dir, c_file_name, gtk_enums, gtk_funptr, TYPES_DICT, TYPES2_DICT, my_stats, my_errors):
    """This function analyzes the list of arguments `args` of the C function.
       It returns the list of Fortran arguments, the iso_c_binding types needed,
       the Fortran declarations of the arguments and an error_flag.
    """
    error_flag = False
    declarations = ""
    args_list = ""

    for arg in args:
        if arg == "void":
            continue    # Next argument in the list

        # Can we find the type of the argument ? The var_type variable is
        # not used elsewhere, but this test is compulsory. Do not remove.
        try:
            var_type = RGX_VAR_TYPE.search(arg).group(1)
        except AttributeError:
            error_flag = True
            my_errors.new_error(c_dir, c_file_name,
                                "Variable type not found", proto, True)
            continue    # Next argument in the list

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
                # Verify that each iso_c appears only once:
                RGX_ISO_C = re.compile("("+iso_c+")"+r"([^\w]|$)")
                if RGX_ISO_C.search(f_use) is None:
                    f_use += ", " + iso_c
        elif "?" in f_type:
            error_flag = True
            my_errors.new_error(c_dir, c_file_name, "Unknown type:  " + arg,
                                proto, True)

        # Unknown dimension arrays are passed by address, others by value:
        if "(*)" in f_type:
            passvar = ""
        else:
            passvar = ", value"

        # Search the variable name:
        try:
            var_name = RGX_VAR_NAME.search(arg).group(1)
        except AttributeError:
            error_flag = True
            my_errors.new_error(c_dir, c_file_name,
                                "Variable name not found", proto, False)
            continue    # Next argument in the list

        # Add this variable:
        if args_list == "":
            args_list = var_name
        else:
            args_list += ", " + var_name

        declarations += 1*TAB + f_type + passvar + " :: " + var_name + "\n"

    return args_list, f_use, declarations, error_flag


def write_fortran_interface(index, module_name, f_file_name, f_file,
                            c_dir, c_file_name, function_status, prototype,
                            f_procedure, f_name, args_list, f_use, declarations,
                            isfunction, returned_type, f_the_end, my_stats):
    """Write the Fortran interface of a function in the *-auto.f90 file
    """
    interface1 = 0*TAB + "! " + function_status + "\n"
    interface1 += 0*TAB + "!" + prototype + "\n"
    first_line = 0*TAB + f_procedure + f_name + "(" + args_list + ") bind(c)"
    interface2 = multiline(first_line, 80) + "\n"
    if f_use != "":
        interface3 = 1*TAB + "import :: " + f_use + "\n"
    else:
        interface3 = ""
    # The 'implicit none' of the Fortran module does not apply to the interfaces,
    # we therefore harden the code by putting one into each interface:
    interface3 += 1*TAB + "implicit none\n"

    if isfunction:
        interface3 += 1*TAB + returned_type + " :: " + f_name + "\n"
    interface3 += declarations
    interface3 += 0*TAB + f_the_end + "\n\n"
    interface = interface1+interface2+interface3

    # Names for the gtk-fortran-index.csv file:
    my_module_name = module_name
    my_f_file_name = f_file_name
    my_first_line = first_line

    f_file.write(interface)
    my_stats.inc_nb_generated_interfaces(1)

    # Adds the function in the gtk-fortran-index.csv file:
    index.append([my_module_name, f_name, function_status, my_f_file_name,
                  c_dir+"/"+c_file_name, prototype, my_first_line])
