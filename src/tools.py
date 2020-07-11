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

""" This module contains various functions used in the cfwrapper.

NOTE: the function "multiline" is duplicated in usemodules.py, to make the 
installation of the latter more straightforward. If you update this function,
please update the corresponding function in usemodules.py as well.
"""


def multiline(line, max_length):
    """Split a long line in a multiline, following Fortran syntax, and trying to
       cut it with elegance.
    """
    max_offset = max_length-1
    result = ""

    while len(line) > max_offset:
        # Remember that character max_length is excluded in such slice:
        string = line[0:max_length]

        cut = max_offset
        # We try to cut before a space, if reasonably possible:
        if string[cut] != " ":
            # Search the last space after the middle of the string:
            last_space = string.rfind(" ", max_length//2)
            if last_space != -1:
                cut = last_space

        result += line[0:cut] + "&\n"
        line = "&"+ line[cut:]

    # Add last line without trailing spaces:
    result += line.rstrip()

    return result
