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
# Last modification: 2025-02-19

""" This module contains a function to translate C types to Fortran types.
"""

import re           # Regular expression library

from scan_types_and_enums import types_enums

# The regex used to identify a C type:
RGX_TYPE = re.compile(r"^ *((const )?\w+)[ \*]?")
# beginning by zero or several spaces,
# possibly by a "const " statement,
# followed by the name of the type,
# and possibly ending by a space or a star.

def iso_c_binding(declaration, isReturned):
    """ Returns the Fortran type corresponding to a C type in the ISO_C_BINDING
        module (limited to C types used in GTK), and the KIND type,
        for example 'integer(c_int)', 'c_int'.
        The declaration contains the type and the name of the entity,
        for example 'GVariant *value'
        The isReturned flag argument distinguishes types returned by a 
        function (true) or arguments types (false).
    """
    
    # Try to find a C type in the declaration:
    try:
        c_type = RGX_TYPE.search(declaration).group(1)
    except AttributeError:
        return "?", "?"    # No C type found => error

    # Remove a possible "const " statement:
    declaration = re.sub(r"^(const )", "", declaration)

    # Is it a "typedef enum"?
    for item in types_enums.gtk_enums:
        if item in c_type:
            return "integer(c_int)", "c_int"

    # Is it a pointer toward a function?
    for item in types_enums.gtk_funptr:
        if item in c_type:
            return "type(c_funptr)", "c_funptr"

    # Is it a gpointer? (an untyped pointer equivalent to void*)
    if ("gpointer" in c_type) or ("gconstpointer" in c_type):
        return "type(c_ptr)", "c_ptr"

    # Is it a C pointer? (one or more stars)
    if "*" in declaration:
        # GVariant and GVariantType are structures, generally used via pointers
        if ("GVariant" in c_type):
            if declaration.count('*') >= 2:
                return "type(c_ptr), dimension(*)", "c_ptr"
            else:
                return "type(c_ptr)", "c_ptr"
        # Is it a string (char or gchar array) or a list of strings?
        elif ("char" in c_type) and (not isReturned):
            if declaration.count('*') >= 2:
                # An array of C strings:
                return "type(c_ptr), dimension(*)", "c_ptr"
            else:
                return "character(kind=c_char), dimension(*)", "c_char"
        # Other cases:
        else:
            return "type(c_ptr)", "c_ptr"

    # Is it an array?
    if "[" in declaration:
        array = ", dimension(*)"
    else:
        array = ""

    # Other cases defined in the TYPES dictionaries:
    if len(declaration.split()) >= 3:  # Two words type + the name of the entity
        # The TYPES2_DICT contains a list of two words type
        for item in types_enums.TYPES2_DICT:
            # A Python set is an unordered collection of distinct hashable objects
            if set(item.split()).issubset(set(declaration.split())):
                return types_enums.TYPES2_DICT[item][0] + array, types_enums.TYPES2_DICT[item][1]
    else:  # It is therefore a one word type
        for item in types_enums.TYPES_DICT:
            if item in c_type.split():
                return types_enums.TYPES_DICT[item][0] + array, types_enums.TYPES_DICT[item][1]

    # We failed to identify the C type.
    # This print can be used for debugging those cases:
    # print(declaration)
    return "?", "?"
    
