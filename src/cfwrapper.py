#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2011
# Free Software Foundation, Inc.
#
# This file is part of the gtk-fortran gtk+ Fortran Interface library.
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
# Last modification: 06-20-2016 (Python 3.5.1, Linux Ubuntu 16.04)
# pylint3 score: 9.04/10

""" Generates the *-auto.f90 files from the C header files of GLlib and GTK+.
For help, type: ./cfwrapper.py -h
"""

import re           # Regular expression library
import os
import time
import csv          # To write .csv files
import platform     # To obtain platform informations
import subprocess   # To launch a shell command
import argparse     # To parse command line
import hashlib      # To dectect modifications in gtk-fortran files
import pickle       # To save the hash in a persistent way
from collections import OrderedDict


def lib_version(lib_name, psys):
    """ Receive the name of a library package and the packaging system, and
    returns the version of the library if found, else returns ?.?.?
    """
    common = " 2>/dev/null | grep Version"
    if psys == "deb":        # Debian/Ubuntu command line:
        libversion = os.popen("dpkg -p " + lib_name
                              + common, mode='r').read()
        if libversion == "": # try APT instead of dpkg:
            libversion = os.popen("apt-cache show " + lib_name
                                  + common, mode='r').read()
    elif psys == "pacman":   # Arch/Manjaro command line
        libversion = os.popen("pacman -Qi " + lib_name
                              + common, mode='r').read()
    elif psys == "rpm":      # Mageia (& Fedora?) command line
        libversion = os.popen("rpm -qi " + lib_name
                              + common, mode='r').read()
    else:
        print("Unknown package system: (", psys, "): ", lib_name)
        libversion = ""

    if libversion == "":       # package not found
        # Uncomment the following line and change the command line for the
        # packaging system of your Linux distribution:
        # libversion = os.popen("dpkg -p "+lib_name+common, mode='r').read()
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


def hash_gtk_fortran():
    """Compute the SHA1 hash of all *-auto.f90 files to detect modifications
    in gtk-fortran (useful during development)"""
    hasher = hashlib.sha1()
    files_list = list(PATH_DICT.values())
    files_list.extend(["gtkenums-auto.f90", "unixonly-auto.f90", "mswindowsonly-auto.f90"])
    for file_name in files_list:
        with open(file_name, 'rb') as auto_file:
            WHOLE = auto_file.read()
            hasher.update(WHOLE)
    NEW_HASH = hasher.hexdigest()
    # Read previous hash:
    try:
        with open("gtk-fortran-hash.pkl", 'rb') as HASH_FILE:
            PREVIOUS_HASH = pickle.load(HASH_FILE)
    except FileNotFoundError:
        PREVIOUS_HASH = ""
    # Then save the new hash in a file:
    with open("gtk-fortran-hash.pkl", 'wb') as HASH_FILE:
        pickle.dump(NEW_HASH, HASH_FILE)
    # Print new hash and compare with previous hash:
    print("\nSHA1: ", NEW_HASH)        
    if NEW_HASH != PREVIOUS_HASH:
        print(">>>>>> SHA 1 HAS BEEN MODIFIED ! It was ", PREVIOUS_HASH, " <<<<<<")
        print()


def iso_c_binding(declaration, returned):
    """ Returns the Fortran type corresponding to a C type in the
        ISO_C_BINDING module (limited to C types used in GTK+),
        and the KIND type
    """
    try:
        c_type = RGX_TYPE.search(declaration).group(1)
    except AttributeError:
        return "?", "?"    # error

    # Is it an array ?
    if declaration.find("[") != -1:
        array = ", dimension(*)"
    else:
        array = ""

    # Is it a "typedef enum" ?
    for item in gtk_enums:
        if c_type.find(item) != -1:
            return "integer(c_int)", "c_int"

    # Is it a pointer toward a function ?
    for item in gtk_funptr:
        if c_type.find(item) != -1:
            return "type(c_funptr)", "c_funptr"

    #typedef void* gpointer;
    if c_type.find("gpointer") != -1 or c_type.find("gconstpointer") != -1:
        return "type(c_ptr)", "c_ptr"

    # Is it a pointer ?
    if declaration.find("*") != -1:
        # Is it a string (char or gchar array) ?
        # TODO: what about "unsigned char"   "guchar" gunichar ?
        if ((c_type.find("char") != -1) or (c_type.find("char*") != -1)) and (not returned):
            if declaration.find("**") != -1:
                return "type(c_ptr), dimension(*)", "c_ptr"
            else:
                return "character(kind=c_char), dimension(*)", "c_char"
        else:
            return "type(c_ptr)", "c_ptr"

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


def write_error(direc, filename, message, proto, type_error):
    """ Write errors in a list and increments the counters """
    global nb_errors
    global nb_type_errors
    global errors_list

    errors_list.append([direc + "/" + filename, message, proto])

    if type_error:
        nb_type_errors += 1
    else:
        nb_errors += 1


def multiline(line, maxlength):
    """Split a long line in a multiline, following Fortran syntax."""
    result = ""
    while len(line) > maxlength-1:
        result += line[0:maxlength-1] + "&\n"
        line = "&"+ line[maxlength-1:]
    result += line
    return result


def set_bit_field(match):
    """ Returns the Fortran bitfield from a C enum flag"""
    return "ISHFTC(1, " + str(int(match.group(1))) + ")"


def translate_enums(enum_list):
    """Receive a C enum and returns a Fortran enum"""
    global nb_enumerators
    f_enum = ""
    bit_fields = re.compile(r"1 *<< *(\d+)")

    for item in enum_list:
        enum = item[0]
        name = item[1]

        # These enums are excluded for some problems...
        # For example GDBusInterfaceSkeletonFlags contains an item with a too long name
        if name in ["GSocketFamily", "GSocketMsgFlags", "GdkPixdataType",
                    "GIOCondition", "GDBusInterfaceSkeletonFlags"]:
            continue    # Go to next enum

        parameters = re.findall("(?ms){(.*)}", enum)

        # Remove lines beginning by #:
        parameters[0] = re.sub("(?m)^#.*$", "", parameters[0])
        # Remove TABs and overnumerous spaces:
        parameters[0] = parameters[0].replace("\t", " ")
        parameters[0] = re.sub("[ ]{2,}", " ", parameters[0])
        # Delete characters (   ) and , if they are not between quotes:
        parameters[0] = re.sub(r"(?<!')(\()(?!')", "", parameters[0])
        parameters[0] = re.sub(r"(?<!')(\))(?!')", "", parameters[0])
        parameters[0] = re.sub("(?<!')(,)(?!')", "", parameters[0])
        parameters[0] = re.sub("(?m),$", "", parameters[0])
        # Remove the u for unsigned numbers (rare)
        parameters[0] = re.sub("1u[ ]<<", "1 <<", parameters[0])

        # Is it a char ?
        parameters[0] = re.sub("('.?')", "iachar(\\1)", parameters[0])
        # Is it in hexadecimal ?
        parameters[0] = re.sub("0x([0-9A-Fa-f]+)", "INT(z'\\1')",
                               parameters[0])
        # Is it a bit field ?
        parameters[0] = bit_fields.sub(set_bit_field, parameters[0])

        # complement
        parameters[0] = re.sub(r"~(\w+)", "not(\\1)", parameters[0])
        # logical or
        parameters[0] = re.sub(r"([\w\(\)]+)\s*\|\s*([\w\(\), \d]+)",
                               "ior(\\1 , \\2)", parameters[0])

        # Renamed flags (have the same name as a GTK+ function):
        for flag in ["ATK_HYPERLINK_IS_INLINE", "GDK_PROPERTY_DELETE",
                     "GDK_DRAG_STATUS", "GDK_DRAG_MOTION"]:
            parameters[0] = re.sub(r"(?m)^\s*"+flag, flag+"_F", parameters[0])

        # Integer size problem:
        parameters[0] = re.sub(r"(?m)^\s*G_PARAM_DEPRECATED.*$",
                               "", parameters[0])

        parameters[0] = re.sub(r"(?m)^\s*(\w+)", "    enumerator :: \\1",
                               parameters[0])

        # Resulting Fortran enumerator:
        f_enum += "enum, bind(c)    !" + name + "\n"
        f_enum += parameters[0]
        f_enum += "end enum\n \n"
        nb_enumerators += 1

    # Remove empty lines:
    f_enum = re.sub(r"(?m)^ *\n$", "", f_enum)

    return f_enum


# **********************************************
# Main program
# **********************************************
# Definition of command line options:
PARSARG = argparse.ArgumentParser(description="Generate gtk-fortran files",
                                  epilog="GPLv3 license, https://github.com/jerryd/gtk-fortran")
PARSARG.add_argument("-g", "--gtk", action="store", type=int, choices=[2, 3],
                     metavar="2|3", nargs=1, required=True,
                     help="GTK+ major version")
PARSARG.add_argument("-b", "--build", action="store_true",
                     help="Build gtk-fortran libraries and examples")
ARGS = PARSARG.parse_args()
GTK_VERSION = "gtk" + str(ARGS.gtk[0])

# -------------------------------------------------------------------------
# These dictionaries give the Fortran type and its KIND for each GTK+ type:
# -------------------------------------------------------------------------
# One word types:
TYPES_DICT = {
    "int":("integer(c_int)", "c_int"),
    "guint":("integer(c_int)", "c_int"),
    "Bool":("integer(c_int)", "c_int"),    #define Bool int    => Xlib.h
    "GPid":("integer(c_int)", "c_int"),
    "gint64":("integer(c_int64_t)", "c_int64_t"),
    "goffset":("integer(c_int64_t)", "c_int64_t"),
    "guint64":("integer(c_int64_t)", "c_int64_t"),
    "gint32":("integer(c_int32_t)", "c_int32_t"),
    "guint32":("integer(c_int32_t)", "c_int32_t"),
    "GdkWChar":("integer(c_int32_t)", "c_int32_t"),      #typedef guint32 GdkWChar;
    "xcb_drawable_t":("integer(c_int32_t)", "c_int32_t"),  #typedef uint32_t xcb_drawable_t;
    "xcb_pixmap_t":("integer(c_int32_t)", "c_int32_t"),  #typedef uint32_t xcb_pixmap_t;
    "uid_t":("integer(c_int32_t)", "c_int32_t"),         #typedef __uid_t uid_t;
    "gint16":("integer(c_int16_t)", "c_int16_t"),
    "guint16":("integer(c_int16_t)", "c_int16_t"),
    "gint8": ("integer(c_int8_t)", "c_int8_t"),
    "guint8": ("integer(c_int8_t)", "c_int8_t"),
    "long":("integer(c_long)", "c_long"),
    "gulong":("integer(c_long)", "c_long"),
    "time_t":("integer(c_long)", "c_long"),  #typedef __time_t time_t;
    "short":("integer(c_short)", "c_short"),
    "boolean":("logical(c_bool)", "c_bool"),
    # For gchar & guchar, see https://github.com/jerryd/gtk-fortran/issues/41#issuecomment-7337877
    "gchar":("integer(kind=c_int8_t)", "c_int8_t"),   #("character(kind=c_char)", "c_char"),
    "guchar":("integer(kind=c_int8_t)", "c_int8_t"),  #("character(kind=c_char)", "c_char"),
    "double": ("real(c_double)", "c_double"),
    "float":("real(c_float)", "c_float"),
    "gsize":  ("integer(c_size_t)", "c_size_t"),    #typedef unsigned long gsize;   also GType
    "gssize":  ("integer(c_size_t)", "c_size_t"),   #typedef signed long gssize;
    "GType":  ("integer(c_size_t)", "c_size_t"),
    "size_t":  ("integer(c_size_t)", "c_size_t"),
    "va_list":("type(c_ptr)", "c_ptr"),
    "gpointer":("type(c_ptr)", "c_ptr"), #typedef void* gpointer;
    "GdkAtom":("type(c_ptr)", "c_ptr"),  #typedef struct _GdkAtom *GdkAtom;
    "GC":("type(c_ptr)", "c_ptr"),       # GC (Xlib) is it a pointer ?
    "GIConv":("type(c_ptr)", "c_ptr"),   #typedef struct _GIConv *GIConv;
    "GSignalCMarshaller":("type(c_ptr)", "c_ptr"),
    "FT_Face":("type(c_ptr)", "c_ptr"),  #typedef struct FT_FaceRec_*  FT_Face;
    # X11 types (See /usr/include/X11/Xmd.h), unsigned int (64 bits archi.)
    # or unsigned long (32 bits architecture):
    "Window":("integer(c_long)", "c_long"),
    "Drawable":("integer(c_long)", "c_long"),  #define Drawable CARD32
    "Font":("integer(c_long)", "c_long"),
    "Pixmap":("integer(c_long)", "c_long"),
    "Cursor":("integer(c_long)", "c_long"),
    "Colormap":("integer(c_long)", "c_long"),
    "GContext":("integer(c_long)", "c_long"),
    "Atom":("integer(c_long)", "c_long"),
    "Picture":("integer(c_long)", "c_long"),
    "XID":("integer(c_long)", "c_long"),
    "VisualID":("integer(c_long)", "c_long"),
    "Time":("integer(c_long)", "c_long"),
    "KeyCode":("character(kind=c_char)", "c_char"),   #define KeyCode CARD8   => unsigned char
    "KeySym":("integer(c_long)", "c_long"),
}

# Two words types:
TYPES2_DICT = {
    "long double": ("real(c_long_double)", "c_long_double"),
    "unsigned long":("integer(c_long)", "c_long"),
    "unsigned short":("integer(c_short)", "c_short"),
    "unsigned int":("integer(c_int)", "c_int")
}

#---------------------------------------------------------------------------
# Regular expressions used to identify the different parts of a C prototype:
#---------------------------------------------------------------------------
RGX_RETURNED_TYPE = re.compile(r"^ *([_0-9a-zA-Z ]+ *\**)")
RGX_FUNCTION_NAME = re.compile(r"([0-9a-zA-Z_]+) *\(")
RGX_ARGUMENTS = re.compile(r"\(([0-9a-zA-Z_ ,\*\[\]]*)\).*;$")
RGX_ARGS = re.compile(r" *([0-9a-zA-Z_ \*\[\]]+),?")
RGX_VAR_TYPE = re.compile(r" *([_0-9a-zA-Z]+)[ |\*]")
RGX_TYPE = re.compile(r"^ *((const |G_CONST_RETURN |cairo_public |G_INLINE_FUNC )?\w+)[ \*]?")
RGX_VAR_NAME = re.compile(r"[ |\*]([_0-9a-zA-Z]+)(?:\[\])?$")
# Function name beginning by an underscore:
RGX_UNDERSCORE = re.compile(r"^_\w+$")

# A tabulation:
TAB = "  "

# Statistics initialization:
nb_lines = 0
nb_generated_interfaces = 0
nb_errors = 0
nb_type_errors = 0
nb_variadic = 0
nb_files = 0
nb_enumerators = 0
nb_win32_utf8 = 0
type_errors_list = []

# Define libraries paths and corresponding *-auto.f90 files.
# Do not change the order of the dictionary keys:
PATH_DICT = OrderedDict([
    ("/usr/include/atk-1.0", "atk-auto.f90"),
    ("/usr/include/cairo", "cairo-auto.f90"),
    ("/usr/include/gdk-pixbuf-2.0", "gdk-pixbuf-auto.f90"),
    ("/usr/include/glib-2.0", "glib-auto.f90")])
if GTK_VERSION == "gtk3":
    PATH_DICT.update([
        ("/usr/include/gtk-3.0/gdk", "gdk-auto.f90"),
        ("/usr/include/gtk-3.0/gtk", "gtk-auto.f90"),
        ("/usr/include/gtk-3.0/unix-print", "unix-print-auto.f90")])
else:
    PATH_DICT.update([
        ("/usr/include/gtk-2.0/gdk", "gdk-auto.f90"),
        ("/usr/include/gtk-2.0/gtk", "gtk-auto.f90")])
PATH_DICT.update([("/usr/include/pango-1.0", "pango-auto.f90")])

#*************************************************************************
# Pass 1: scan all header files to find all enum types, all pointers to
# functions (funptr) and add derived GTK+ types
#*************************************************************************
print("Pass 1: looking for enumerators, funptr and derived types...")

gtk_enums = []
gtk_funptr = []
gtk_types = []

T0 = time.time()     # To calculate computing time
for library_path in PATH_DICT:
    for directory in os.walk(library_path):
        for c_file_name in directory[2]:
            whole_file = open(directory[0] + "/" + c_file_name, 'rU',
                              errors='replace').read()
            gtk_enums += re.findall(r"(?ms)^typedef enum.*?}\s?(\w+);", whole_file)
            gtk_funptr += re.findall(r"(?m)^typedef[ \t]*(?:const)?[ \t]*\w+[ \t]*\*?\s*\(\* ?([\w]*?)\)",
                                     whole_file)
            gtk_types += re.findall(r"(?m)^typedef *?(?:const)? *?(\w+) *\*? *([\w]+);",
                                    whole_file)

# Add derived types:
for each in gtk_types:
    if each[1] not in TYPES_DICT:
        if each[0] in TYPES_DICT:
            TYPES_DICT[each[1]] = TYPES_DICT[each[0]]
        elif each[0] in gtk_funptr:
            TYPES_DICT[each[1]] = ("type(c_funptr)", "c_funptr")

# Sorting (useful only for printing):
gtk_enums.sort()
gtk_funptr.sort()

#**************************************************************************
# Pass 2: Scan of all header files in the directories and subdirectories to
# generate interfaces
#**************************************************************************
print("Pass 2: looking for C functions...")

FILE_HEADER = """! Automatically generated by cfwrapper.py, do not modify !
! This file is part of the gtk-fortran library, distributed under
! GNU General Public License version 3.
"""

# All enums are written in this file:
enums_file = open("gtkenums-auto.f90", "w")
enums_file.write(FILE_HEADER)

# Files for platform specific functions:
HEAD = "\nmodule " + "gtk_os_dependent" + "\nimplicit none\ninterface\n\n"
unix_only_file = open("unixonly-auto.f90", "w")
unix_only_file.write(FILE_HEADER + HEAD)
mswindows_only_file = open("mswindowsonly-auto.f90", "w")
mswindows_only_file.write(FILE_HEADER + HEAD)

index = []
opened_files = []
used_types = []
errors_list = []

for library_path in PATH_DICT:
    F_FILE_NAME = PATH_DICT[library_path]   # Fortran *-auto.f90 file
    print(library_path + "\t => \t" + F_FILE_NAME, end="\t")

    # Create the *-auto.f90 file with its module declaration:
    if F_FILE_NAME not in opened_files:
        f_file = open(F_FILE_NAME, "w")
        opened_files.append(F_FILE_NAME)

        # The module name is derived from the Fortran file name:
        module_name = re.search(r"^(.+)-auto\.f90", F_FILE_NAME).group(1)
        module_name = module_name.replace("-", "_")

        # The gtk-auto.f90 file is a special case (included in gtk.f90)
        if module_name != "gtk":
            if module_name == "glib":
                module_name = "g"

            f_file.write(FILE_HEADER+"\nmodule " + module_name +
                         "\nimplicit none\ninterface\n\n")

    # Each header file in each subdirectory of the library is analyzed:
    for directory in os.walk(library_path):
        for c_file_name in directory[2]:
            # Those files cause problems so we exclude them:
            if c_file_name in ["gstdio.h", "giochannel.h"]:
                continue    # Go to next file

            whole_file_original = open(directory[0] + "/"
                                       + c_file_name, 'rU',
                                       errors='replace').read()
            # The original will be used for WIN32 functions
            whole_file = whole_file_original
            nb_files += 1

            # -----------------------------------------------------
            # Preprocessing and cleaning of the header file.
            # Do not change the order of the regular expressions !
            # -----------------------------------------------------

            # Remove C commentaries:
            whole_file = re.sub(r"(?s)/\*.*?\*/", "", whole_file)

            # Translating C enumerators to Fortran enumerators:
            enum_types = re.findall(r"(?ms)^(typedef enum\s*?(?:\w+)?\s*?{.*?})\s*?(\w+);", whole_file)
            enums_file.write(translate_enums(enum_types))

            # Removing multilines typedef:
            whole_file = re.sub(r"(?m)^typedef([^;]*?\n)+?[^;]*?;$",
                                "", whole_file)
            # Remove C directives (multilines then monoline):
            # Note that in a python regular expression \\\\=\\=\ if raw
            # strings not used
            whole_file = re.sub("(?m)^#(.*[\\\\][\\n])+.*?$", "", whole_file)
            whole_file = re.sub("(?m)^#.*$", "", whole_file)
            # Remove TABs and overnumerous spaces:
            whole_file = whole_file.replace("\t", " ")
            whole_file = re.sub("[ ]{2,}", " ", whole_file)
            # Remove two levels of { } structures:
            whole_file = re.sub("(?ms){[^{]*?}$", "", whole_file)
            whole_file = re.sub("(?ms){[^{]*?}$", "", whole_file)
            # Remove structures like: { } a_name;
            whole_file = re.sub(r"(?ms){[^{]*?}[ \w]*?;", "", whole_file)
            # Remove "available_in" and "deprecated" directives:
            whole_file = re.sub("(?m)^.*(_AVAILABLE_IN_|_DEPRECATED).*$",
                                "", whole_file)
            # Remove different kind of declarations:
            whole_file = re.sub("(?m)^(enum).*$", "", whole_file)
            whole_file = re.sub("(?m)^(typedef|union|struct).*$",
                                "", whole_file)
            whole_file = re.sub("(?m)^.*(G_BEGIN_DECLS|CAIRO_BEGIN_DECLS) *$", "", whole_file)
            whole_file = re.sub("(?m)^.*(G_END_DECLS|CAIRO_END_DECLS) *$",
                                "", whole_file)
            whole_file = re.sub(r"(?m)^.*(G_UNLOCK|G_LOCK|G_LOCK_DEFINE_STATIC)\(.*;$", "", whole_file)
            whole_file = re.sub("(?m)^.*(cairo_public|extern) ", "", whole_file)
            whole_file = re.sub("(?m)^(GLIB_VAR|GTKVAR|GDKVAR|GDK_PIXBUF_VAR|GTKMAIN_C_VAR|G_INLINE_FUNC)"
                                , "", whole_file)   # extern
            # Remove empty lines:
            whole_file = re.sub(r"(?m)^\n$", "", whole_file)

            # These three functions names are the only ones between parentheses,
            # so we remove parentheses (>=GLib 2.48.O):
            if c_file_name == "gutils.h":
                whole_file = whole_file.replace("(g_bit_nth_lsf)", "g_bit_nth_lsf")
                whole_file = whole_file.replace("(g_bit_nth_msf)", "g_bit_nth_msf")
                whole_file = whole_file.replace("(g_bit_storage)", "g_bit_storage")

            # From now each line will be treated separately:
            lines_list = whole_file.splitlines(True)
            corrected_lines_list = []
            try:
                corrected_lines_list.append(lines_list[0])
            except IndexError:
                write_error(directory[0], c_file_name,
                            "No function to implement in this file",
                            "", False)
                continue    # Go to next file

            #------------------------------------
            # Preprocessing of each C prototype:
            #------------------------------------
            i = 0
            for prototype in lines_list:
                nb_lines += 1
                # remove leading and trailing spaces:
                prototype2 = str.strip(prototype)

                if corrected_lines_list[i].find(";") == -1:
                    # Remove line feeds inside a prototype:
                    corrected_lines_list[i] = corrected_lines_list[i].replace("\n", "")
                    corrected_lines_list[i] += " "+prototype2
                else:
                    corrected_lines_list.append(prototype2)
                    i = i + 1

            #------------------------------------
            # Each prototype is now analyzed:
            #------------------------------------
            for prototype in corrected_lines_list:
                error_flag = False

                type_returned = RGX_RETURNED_TYPE.search(prototype)
                try:
                    function_type = type_returned.group(1)
                except AttributeError:
                    write_error(directory[0], c_file_name,
                                "Returned type not found", prototype, False)
                    continue    # Next prototype

                # Will it be a Fortran function or a subroutine ?
                if (function_type.find("void") != -1) and (function_type.find("*") == -1):
                    f_procedure = "subroutine "
                    f_the_end = "end subroutine"
                    isfunction = False
                    f_use = ""
                else:
                    f_procedure = "function "
                    f_the_end = "end function"
                    isfunction = True
                    returned_type, iso_c = iso_c_binding(type_returned.group(1), True)
                    f_use = iso_c
                    if returned_type.find("?") != -1:
                        error_flag = True
                        write_error(directory[0], c_file_name,
                                    "Unknown data type:    " + type_returned.group(1),
                                    prototype, True)
                        type_errors_list.append(type_returned.group(1))

                # f_name is the name of the function in gtk-fortran:
                function_name = RGX_FUNCTION_NAME.search(prototype)
                try:
                    f_name = function_name.group(1)
                except AttributeError:
                    write_error(directory[0], c_file_name,
                                "Function name not found", prototype, False)
                    continue    # Next prototype

                # Functions beginning by an underscore will be excluded:
                if RGX_UNDERSCORE.match(f_name) != None:
                    continue    # Next prototype

                # gtk_init() is already defined in gtk.f90. Other functions
                # can be excluded in case of problem:
                if f_name in ["gtk_init"]:
                    continue    # Next prototype

                arguments = RGX_ARGUMENTS.search(prototype)
                try:
                    args = RGX_ARGS.findall(arguments.group(1))
                except AttributeError:
                    write_error(directory[0], c_file_name,
                                "Arguments not found", prototype, False)
                    if prototype.find("...") != -1:
                        nb_variadic += 1    # Optional arguments are not managed !
                    continue    # Next prototype

                # Each argument of the function is analyzed:
                declarations = ""
                args_list = ""
                for arg in args:
                    if arg != "void":
                        try:
                            var_type = RGX_VAR_TYPE.search(arg).group(1)
                        except AttributeError:
                            write_error(directory[0], c_file_name,
                                        "Variable type not found", prototype, True)
                            continue    # Next argument

                        f_type, iso_c = iso_c_binding(arg, False)
                        if iso_c not in used_types:
                            used_types.append(iso_c)

                        if f_type.find("c_") != -1:
                            if f_use == "":
                                f_use = iso_c
                            else:
                                # each iso_c must appear only once:
                                RGX_ISO_C = re.compile("("+iso_c+")"+r"([^\w]|$)")
                                if RGX_ISO_C.search(f_use) is None:
                                    f_use += ", " + iso_c
                        elif f_type.find("?") != -1:
                            error_flag = True
                            write_error(directory[0], c_file_name,
                                        "Unknown data type:    " + arg,
                                        prototype, True)
                            type_errors_list.append(arg)

                        try:
                            var_name = RGX_VAR_NAME.search(arg).group(1)
                        except AttributeError:
                            write_error(directory[0], c_file_name,
                                        "Variable name not found", prototype, False)
                            continue    # Next argument

                        # Arrays with unknown dimension are passed by adress,
                        # the others by value:
                        if f_type.find("(*)") != -1:
                            passvar = ""
                        else:
                            passvar = ", value"

                        declarations += 1*TAB + f_type + passvar + " :: " + var_name + "\n"
                        if args_list == "":
                            args_list = var_name
                        else:
                            args_list += ", " + var_name

                # Write the Fortran interface in the .f90 file:
                if error_flag is False:
                    interface1 = 0*TAB + "!" + prototype + "\n"
                    first_line = 0*TAB + f_procedure + f_name + "(" + args_list + ") bind(c)"
                    interface2 = multiline(first_line, 80) + "\n"
                    interface3 = 1*TAB + "use iso_c_binding, only: " + f_use + "\n"
                    if isfunction:
                        interface3 += 1*TAB + returned_type + " :: " + f_name + "\n"
                    interface3 += declarations
                    interface3 += 0*TAB + f_the_end + "\n\n"

                    # Deals with the Win32 _utf8 functions: the normal form and
                    # the Windows form are dispatched in two platform dependent
                    # files, although the module name is always "gtk_os_dependent":
                    if re.search(r"(?m)^#define\s+"+f_name+r"\s+"+f_name+r"_utf8\s*$",
                                 whole_file_original):
                        unix_only_file.write(interface1+interface2+interface3)
                        index.append(["gtk_os_dependent", f_name,
                                      "unixonly-auto.f90/mswindowsonly-auto.f90",
                                      directory[0]+"/"+c_file_name, prototype])
                        first_line = 0*TAB + f_procedure + f_name + "(" + args_list + ") bind(c, name='"+f_name+"_utf8')"
                        interface2_utf8 = multiline(first_line, 80) + "\n"
                        mswindows_only_file.write(interface1+interface2_utf8+interface3)
                        nb_generated_interfaces += 2
                        nb_win32_utf8 += 1
                    else: # Non platform specific functions
                        f_file.write(interface1+interface2+interface3)
                        index.append([module_name, f_name, F_FILE_NAME,
                                      directory[0]+"/"+c_file_name, prototype])
                        nb_generated_interfaces += 1

    if module_name != "gtk":
        f_file.write("end interface\nend module "+module_name+"\n")
        f_file.close()
    print(os.stat(F_FILE_NAME).st_size, " bytes")

# **********************************
# End of the header files processing
# **********************************
# Write list of GTK+ functions in a CSV file:
index.sort()
index_file = csv.writer(open("gtk-fortran-index.csv", "w"), delimiter=";")
index_file.writerows(index)
# Write errors in a CSV file:
errors_list.sort()
errors_file = csv.writer(open("cfwrapper-errors.csv", "w"), delimiter=";")
errors_file.writerows(errors_list)
# Save remaining error types:
type_errors_list.sort()
TYPE_ERRORS_FILE = open("cfwrapper-type_errors.txt", "w")
for a_type in type_errors_list:
    TYPE_ERRORS_FILE.write(a_type+"\n")

# Close global files:
TYPE_ERRORS_FILE.close()
enums_file.close()
TAIL = "end interface\nend module "+"gtk_os_dependent"+"\n"
unix_only_file.write(TAIL)
unix_only_file.close()
mswindows_only_file.write(TAIL)
mswindows_only_file.close()

# Packages in Ubuntu, Arch/Manjaro, Fedora, Mageia (you can add the names in
# you distribution and add the command in the function lib_version()):
PACK_GTK3 = (("libgtk-3-0", "deb"), ("gtk3", "pacman"),
             ("gtk3", "rpm"), ("gtk+3.0", "rpm"))
PACK_GTK2 = (("libgtk2.0-0", "deb"), ("gtk2", "pacman"),
             ("gtk2", "rpm"), ("gtk+2.0", "rpm"))
PACK_GLIB = (("libglib2.0-0", "deb"), ("glib2", "pacman"),
             ("glib2", "rpm"), ("libglib2.0_0", "rpm"))

print("\n=== Statistics (ready to paste in the Status wiki page) ===\n")
if GTK_VERSION == "gtk3":
    VERSION = library_version(PACK_GTK3)
else:
    VERSION = library_version(PACK_GTK2)
print("##GTK+ " + VERSION + ", GLib "+ library_version(PACK_GLIB) + ", "
      + " " + subprocess.getoutput("lsb_release -ds")
      + " " + platform.machine() + ", Python " + platform.python_version())
print(os.getlogin() + ", "
      + time.strftime("%a, %d %b %Y %H:%M:%S +0000", time.gmtime()))
print("* nb_files scanned =", nb_files)
print("* nb_generated_interfaces =", nb_generated_interfaces)
print("* nb_type_errors =", nb_type_errors)
print("* nb_errors (others) =", nb_errors)
print("* nb_lines treated =", nb_lines)
print("* nb_variadic functions =", nb_variadic)
print("* nb_enumerators =", nb_enumerators)
print("* nb_win32_utf8 =", nb_win32_utf8)
print("* Number of types =", len(TYPES_DICT) + len(TYPES2_DICT))
print("* Computing time: {0:.2f} s".format(time.time()-T0))
print()
print(used_types)

# Print the SHA1 of all *-auto.f90 files:
hash_gtk_fortran()

if ARGS.build:
    # Build the gtk-fortran project using CMake:
    subprocess.call(["./build.sh"])
