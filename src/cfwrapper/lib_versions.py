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
# Contributed by Vincent Magnin, 2011-01-28
# Last modification: 2020-02-11

""" This module contains functions to determine the versions of the libraries
and programs used in gkt-fortran.
"""

import os
import re           # Regular expression library
import platform     # To obtain platform informations
import subprocess   # To launch a shell command
import csv          # To write .csv files

from globals_const import TOP_DIR


def lib_version(lib_name, psys):
    """ Receive the name of a library package and the packaging system, and
    returns the version of the library if found, else returns ?.?.?
    """
    common = lib_name + " 2>/dev/null | grep Version"

    if psys == "deb":        # Debian/Ubuntu command line:
        # Try first APT:
        libversion = os.popen("apt-cache show " + common, mode='r').read()
        if libversion == "": # then dpkg:
            libversion = os.popen("dpkg -p " + common, mode='r').read()
    elif psys == "pacman":   # Arch/Manjaro command line
        libversion = os.popen("pacman -Qi " + common, mode='r').read()
    elif psys == "rpm":      # Mageia (& Fedora?) command line
        libversion = os.popen("rpm -qi " + common, mode='r').read()
    else:
        print("Unknown package system: (", psys, "): ", lib_name)
        libversion = ""

    if libversion == "":       # package not found
        # Uncomment the following line and change the command line for the
        # packaging system of your Linux distribution:
        # libversion = os.popen("dpkg -p " + common, mode='r').read()
        pass     # no operation instruction to avoid an empty if statement

    try:
        libversion = re.search(r"(\d{1,2}\.\d{1,2}\.\d{1,2})", libversion).group(1)
    except AttributeError:
        libversion = "?.?.?"

    return libversion


def library_version(tuple_packages):
    """Search and return the version of the library on your system, trying
    several packaging systems, or returns ?.?.? if not found. Each item in
    tuple_packages is a tuple (package name, packaging system).
    """
    for item in tuple_packages:
        libver = lib_version(item[0], item[1])
        if libver != "?.?.?":
            break

    return libver


def list_versions(GTK_VERSION):
    """Returns a tuple containing the gtk-fortran, GTK, GLib and Ubuntu versions,
       for example:
       (3, 3.24.12, 2.62, 19.10)
    """

    # Packages in Ubuntu, Arch/Manjaro, Fedora, Mageia (you can add the names in
    # you distribution and add the command in the function lib_version()):
    pack_gtk4 = (("libgtk-4-0", "deb"), ("gtk4", "pacman"),
                 ("gtk4", "rpm"), ("gtk4.0", "rpm"))
    pack_gtk3 = (("libgtk-3-0", "deb"), ("gtk3", "pacman"),
                 ("gtk3", "rpm"), ("gtk+3.0", "rpm"))
    pack_gtk2 = (("libgtk2.0-0", "deb"), ("gtk2", "pacman"),
                 ("gtk2", "rpm"), ("gtk+2.0", "rpm"))
    pack_glib = (("libglib2.0-0", "deb"), ("glib2", "pacman"),
                 ("glib2", "rpm"), ("libglib2.0_0", "rpm"))

    if GTK_VERSION == "gtk4":
        version_GTK = library_version(pack_gtk4)
    elif GTK_VERSION == "gtk3":
        version_GTK = library_version(pack_gtk3)
    elif GTK_VERSION == "gtk2":
        version_GTK = library_version(pack_gtk2)

    gtk_fortran = GTK_VERSION.replace("gtk", "")

    return (gtk_fortran, version_GTK, library_version(pack_glib),
            subprocess.getoutput("lsb_release -rs"))


def gtk_fortran_version(GTK_VERSION):
    """Returns a string containing the GTK, GLib and Ubuntu versions used to
       generate gtk-fortran, with the names of the libraries, for example :
       GTK 3.24.12, GLib 2.62.1, Ubuntu 19.10 x86_64
    """

    gtk_fortran, version_GTK, version_GLIB, version_Ubuntu = list_versions(GTK_VERSION)

    return ("GTK " + version_GTK + ", GLib "+ version_GLIB
            + ", Ubuntu " + version_Ubuntu + " " + platform.machine())


def create_VERSIONS_file(GTK_VERSION):
    """Create the VERSIONS file a the top of the project. This file is used by
       other parts of the build system.
    """

    gtk_fortran, version_GTK, version_GLIB, version_Ubuntu = list_versions(GTK_VERSION)

    all_versions = []
    all_versions.append(["gtk-fortran", gtk_fortran])
    all_versions.append(["GTK", version_GTK])
    all_versions.append(["GLib", version_GLIB])
    all_versions.append(["Ubuntu", version_Ubuntu])

    VERSIONS_file = csv.writer(open(TOP_DIR+"VERSIONS", "w"), delimiter=";")
    VERSIONS_file.writerows(all_versions)
