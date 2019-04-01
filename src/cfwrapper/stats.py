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

""" This module contains functions for printing statistics at the end of the 
gtk-fortran generation process.
"""

import hashlib      # To dectect modifications in gtk-fortran files
import pickle       # To save the hash in a persistent way
import platform     # To obtain platform informations
import time
import os

# Project modules:
from globals_const import *
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


def print_statistics(T0, GTK_VERSION, PATH_DICT, TYPES_DICT, TYPES2_DICT, nb_lines, nb_generated_interfaces, nb_deprecated_functions, nb_errors,  nb_type_errors, nb_variadic, nb_files, nb_enumerators, nb_win32_utf8, used_types):
    """Print various statistics about the generation of gtk-fortran
    """

    print("\033[1m\n=== Statistics (ready to paste in the Status wiki page) ===\n\033[0m")

    print("\033[34m## " + gtk_fortran_version(GTK_VERSION) + ", Python " 
          + platform.python_version())
    print(os.getlogin() + ", "
          + time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime()) + "\033[0m")

    print('{:<30}{:>6}'.format("* nb_files scanned =", nb_files))
    print('{:<30}{:>6}'.format("* nb_generated_interfaces =", nb_generated_interfaces))
    print('{:<30}{:>6}'.format("* nb_deprecated_functions =", nb_deprecated_functions))
    print('{:<30}{:>6}'.format("* nb_type_errors =", nb_type_errors))
    print('{:<30}{:>6}'.format("* nb_errors (others) =", nb_errors))
    print('{:<30}{:>6}'.format("* nb_lines treated =", nb_lines))
    print('{:<30}{:>6}'.format("* nb_variadic functions =", nb_variadic))
    print('{:<30}{:>6}'.format("* nb_enumerators =", nb_enumerators))
    print('{:<30}{:>6}'.format("* nb_win32_utf8 =", nb_win32_utf8))
    print('{:<30}{:>6}'.format("* Number of types =", len(TYPES_DICT) + len(TYPES2_DICT)))

    print("* Computing time: {0:.2f} s".format(time.time()-T0))

    # Print the SHA1 of all *-auto.f90 files and look for modification:
    hash_gtk_fortran(PATH_DICT)

    print("\n\033[1m Used types:", used_types, "\033[0m")
