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
# Last modification: 2019-04-01

""" This module contains functions used in the cfwrapper.
"""

import re           # Regular expression library


def iso_c_binding(declaration, returned, gtk_enums, gtk_funptr, TYPES_DICT, TYPES2_DICT):
    """ Returns the Fortran type corresponding to a C type in the
        ISO_C_BINDING module (limited to C types used in GTK),
        and the KIND type
    """
    # Used to identify a C type:
    RGX_TYPE = re.compile(r"^ *((const |G_CONST_RETURN |cairo_public |G_INLINE_FUNC )?\w+)[ \*]?")

    try:
        c_type = RGX_TYPE.search(declaration).group(1)
    except AttributeError:
        return "?", "?"    # error

    # Remove "const " statement:
    declaration = re.sub("^(const )", "", declaration)

    # Is it a "typedef enum" ?
    for item in gtk_enums:
        if item in c_type:
            return "integer(c_int)", "c_int"

    # Is it a pointer toward a function ?
    for item in gtk_funptr:
        if item in c_type:
            return "type(c_funptr)", "c_funptr"

    #typedef void* gpointer;
    if ("gpointer" in c_type) or ("gconstpointer" in c_type):
        return "type(c_ptr)", "c_ptr"

    # Is it a pointer ?
    if "*" in declaration:
        # Is it a string (char or gchar array) ?
        if ("char" in c_type) and (not returned):
            if "**" in declaration:
                return "type(c_ptr), dimension(*)", "c_ptr"
            else:
                return "character(kind=c_char), dimension(*)", "c_char"
        else:
            return "type(c_ptr)", "c_ptr"

    # Is it an array ?
    if "[" in declaration:
        array = ", dimension(*)"
    else:
        array = ""

    # Other cases:
    if len(declaration.split()) >= 3:   # Two words type
        for item in TYPES2_DICT:
            if set(item.split()).issubset(set(declaration.split())):
                return TYPES2_DICT[item][0] + array, TYPES2_DICT[item][1]
    else:  # It is a one word type
        for item in TYPES_DICT:
            if item in c_type.split():
                return TYPES_DICT[item][0] + array, TYPES_DICT[item][1]

    # It is finally an unknown type:
    return "?", "?"

