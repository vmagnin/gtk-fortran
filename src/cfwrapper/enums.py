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
# Last modification: 2021-04-03

""" This module contains functions to determine the versions of the libraries
and programs used in gkt-fortran.
"""

import re           # Regular expression library

# Project modules:
from globals_const import TAB
from tools import multiline


def set_bit_field(match):
    """ Returns the Fortran bitfield from a C enum flag
    """
    return "ISHFTC(1, " + str(int(match.group(1))) + ")"


def translate_enums(c_file_name, enum_list):
    """Receive a list of C enums and returns a text variable containing the
    Fortran enums.
    """
    bit_fields = re.compile(r"1 *<< *(\d+)")

    f_enum = "! " + c_file_name + "\n"
    nb = 0

    for item in enum_list:
        enum = item[0]
        name = item[1]

        # These enums are excluded for some problems... For example,
        # GDBusInterfaceSkeletonFlags contains an item with a too long name :
        if name in ["GSocketFamily", "GSocketMsgFlags", "GdkPixdataType",
                    "GIOCondition", "GDBusInterfaceSkeletonFlags",
                    "GdkSeatCapabilities", "GdkAxisFlags"]:
            continue    # Go to next enum

        parameters = re.findall("(?ms){(.*)}", enum)

        # ********** Cleaning **********
        # Remove lines beginning by #:
        parameters[0] = re.sub("(?m)^#.*$", "", parameters[0])
        # Remove TABs and overnumerous spaces:
        parameters[0] = parameters[0].replace("\t", " ")
        parameters[0] = re.sub("[ ]{2,}", " ", parameters[0])
        parameters[0] = re.sub("(?m) +$", "", parameters[0])
        # Delete characters (   ) and , if they are not between quotes:
        parameters[0] = re.sub(r"(?<!')(\()(?!')", "", parameters[0])
        parameters[0] = re.sub(r"(?<!')(\))(?!')", "", parameters[0])
        parameters[0] = re.sub("(?<!')(,)(?!')", "", parameters[0])
        parameters[0] = re.sub("(?m),$", "", parameters[0])
        # Remove the u for unsigned numbers (rare)
        parameters[0] = re.sub("1u[ ]<<", "1 <<", parameters[0])
        # Remove those preprocessor constants:
        parameters[0] = re.sub("GLIB_AVAILABLE_ENUMERATOR_IN_\d_[\d]+ ", "", parameters[0])

        # ********** Refactoring **********
        # Is it a char ?
        parameters[0] = re.sub("('.?')", r"iachar(\1)", parameters[0])
        # Is it in hexadecimal ?
        parameters[0] = re.sub("0x([0-9A-Fa-f]+)", r"INT(z'\1')",
                               parameters[0])
        # Is it a bit field ?
        parameters[0] = bit_fields.sub(set_bit_field, parameters[0])
        # complement
        parameters[0] = re.sub(r"~(\w+)", r"not(\1)", parameters[0])
        # logical or
        parameters[0] = re.sub(r"([\w\(\)]+)\s*\|\s*([\w\(\), \d]+)",
                               r"ior(\1 , \2)", parameters[0])

        # Renamed flags (have the same name as a GTK function):
        for flag in ["ATK_HYPERLINK_IS_INLINE", "GDK_PROPERTY_DELETE",
                     "GDK_DRAG_STATUS", "GDK_DRAG_MOTION"]:
            parameters[0] = re.sub(r"(?m)^\s*"+flag, flag+"_F", parameters[0])

        # Integer size problem:
        parameters[0] = re.sub(r"(?m)^\s*G_PARAM_DEPRECATED.*$",
                               "", parameters[0])

        # Resulting Fortran enumerator:
        f_enum += "enum, bind(c)    !" + name + "\n"
        enumerators = re.sub(r"(?m)^\s*(\w+)", 1*TAB + r"enumerator :: \1", parameters[0])
        for line in enumerators.splitlines():
            f_enum += multiline(line, 80) + "\n"
        f_enum += "end enum\n\n"
        nb += 1

    # Remove empty lines:
    f_enum = re.sub(r"(?m)^ *\n$", "", f_enum)

    # If enums has been found:
    if f_enum != "! " + c_file_name + "\n":
        f_enum += "\n"

    return f_enum, nb
