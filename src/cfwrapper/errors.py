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
# Last modification: 2019-04-02

""" This module contains the class Errors used in the cfwrapper.
"""

class Errors():
    """This class is used to manage the errors that occurs when scannning the
    C header files.
    """

    def __init__(self):
        self.nb_errors = 0
        self.nb_type_errors = 0
        self.errors_list = []

    def inc_nb_errors(self):
        self.nb_errors += 1

    def inc_nb_type_errors(self):
        self.nb_type_errors += 1

    def append_error(self, error):
        self.errors_list.append(error)

    def new_error(self, direc, filename, message, proto, type_error):
        """Write errors in the list and increments the counters.
        """
        self.append_error([direc + "/" + filename, message, proto])

        if type_error:
            self.inc_nb_type_errors()
        else:
            self.inc_nb_errors()

    def sort(self):
        self.errors_list.sort()
