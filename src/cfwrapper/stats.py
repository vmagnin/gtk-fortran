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

""" This module contains functions for printing statistics at the end of the
gtk-fortran generation process.
"""

import hashlib      # To dectect modifications in gtk-fortran files
import pickle       # To save the hash in a persistent way
import platform     # To obtain platform informations
import time
import os

# Project modules:
from globals_const import SRC_DIR
from lib_versions import gtk_fortran_version


def hash_gtk_fortran(PATH_DICT):
    """Compute the SHA1 hash of all *-auto.f90 files to detect modifications
    in gtk-fortran (useful during development)
    """
    hasher = hashlib.sha1()

    files_list = list(PATH_DICT.values())
    files_list.extend(["gtkenums-auto.f90", "unixonly-auto.f90", "mswindowsonly-auto.f90"])

    for file_name in files_list:
        with open(SRC_DIR+file_name, 'rb') as auto_file:
            whole = auto_file.read()
            hasher.update(whole)

    new_hash = hasher.hexdigest()
    # Read previous hash:
    try:
        with open("gtk-fortran-hash.pkl", 'rb') as hash_file:
            previous_hash = pickle.load(hash_file)
    except FileNotFoundError:
        previous_hash = ""
    # Then save the new hash in a file:
    with open("gtk-fortran-hash.pkl", 'wb') as hash_file:
        pickle.dump(new_hash, hash_file)

    # Print new hash and compare with previous hash:
    print("* SHA1: ", new_hash)
    if new_hash != previous_hash:
        print("\033[31m >>>>>> SHA 1 HAS BEEN MODIFIED ! It was ", previous_hash, " <<<<<< \033[0m")
        print()


class Statistics():
    """This class is used to manage the gtk-fortran statistics.
    """
    def __init__(self):
        self.nb_lines = 0
        self.nb_generated_interfaces = 0
        self.nb_deprecated_functions = 0
        self.nb_variadic = 0
        self.nb_files = 0
        self.nb_enumerators = 0
        self.nb_win32_utf8 = 0
        self.used_types = []

    def inc_nb_lines(self, n):
        self.nb_lines += n

    def inc_nb_generated_interfaces(self, n):
        self.nb_generated_interfaces += n

    def inc_nb_deprecated_functions(self):
        self.nb_deprecated_functions += 1

    def inc_nb_variadic(self):
        self.nb_variadic += 1

    def inc_nb_files(self):
        self.nb_files += 1

    def inc_nb_enumerators(self, n):
        self.nb_enumerators += n

    def inc_nb_win32_utf8(self):
        self.nb_win32_utf8 += 1

    def append_type(self, iso_c):
        self.used_types.append(iso_c)

    def print(self, T0, GTK_VERSION, PATH_DICT, TYPES_DICT, TYPES2_DICT, my_errors):
        """Print various statistics about the generation of gtk-fortran
        """

        print("\033[1m\n=== Statistics (ready to paste in the Status wiki page) ===\n\033[0m")

        print("\033[34m## " + gtk_fortran_version(GTK_VERSION) + ", Python "
              + platform.python_version())
        print(os.getlogin() + ", "
              + time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime()) + "\033[0m")

        print('{:<30}{:>6}'.format("* nb_files scanned =", self.nb_files))
        print('{:<30}{:>6}'.format("* nb_generated_interfaces =", self.nb_generated_interfaces))
        print('{:<30}{:>6}'.format("* nb_deprecated_functions =", self.nb_deprecated_functions))
        print('{:<30}{:>6}'.format("* nb_type_errors =", my_errors.nb_type_errors))
        print('{:<30}{:>6}'.format("* nb_errors (others) =", my_errors.nb_errors))
        print('{:<30}{:>6}'.format("* nb_lines treated =", self.nb_lines))
        print('{:<30}{:>6}'.format("* nb_variadic functions =", self.nb_variadic))
        print('{:<30}{:>6}'.format("* nb_enumerators =", self.nb_enumerators))
        print('{:<30}{:>6}'.format("* nb_win32_utf8 =", self.nb_win32_utf8))
        print('{:<30}{:>6}'.format("* Number of types =", len(TYPES_DICT) + len(TYPES2_DICT)))

        print("* Computing time: {0:.2f} s".format(time.time()-T0))

        # Print the SHA1 of all *-auto.f90 files and look for modification:
        hash_gtk_fortran(PATH_DICT)

        print("\n\033[1m Used types:", self.used_types, "\033[0m")
