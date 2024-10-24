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
# Contributed by Vincent Magnin, 2023-03-21
# Last modification: 2024-10-24

import os
import re           # Regular expression library
import csv          # To write .csv files

# Project modules:
from globals_const import SRC_DIR


class types_enums():
    """ This class contains two dictionnaries with the GLib/GTK types,
    a list of the enums and a list of funptr types. They are class attributes
    but dynamically completed when an instance is created.
    """
    # -------------------------------------------------------------------------
    # These dictionaries give the Fortran type and its KIND for each GTK type.
    # TYPES_DICT will be completed with other types detected by the algorithm.
    # -------------------------------------------------------------------------
    # One word types:
    TYPES_DICT = {
        "int":("integer(c_int)", "c_int"),
        "gint":("integer(c_int)", "c_int"),
        "guint":("integer(c_int)", "c_int"),
        #define Bool int    => Xlib.h
        "Bool":("integer(c_int)", "c_int"),
        "gboolean":("integer(c_int)", "c_int"),
        "GPid":("integer(c_int)", "c_int"),
        "gint64":("integer(c_int64_t)", "c_int64_t"),
        "goffset":("integer(c_int64_t)", "c_int64_t"),
        "guint64":("integer(c_int64_t)", "c_int64_t"),
        "gint32":("integer(c_int32_t)", "c_int32_t"),
        "guint32":("integer(c_int32_t)", "c_int32_t"),
        "uint32_t":("integer(c_int32_t)", "c_int32_t"),
        #typedef guint32 GdkWChar;
        "GdkWChar":("integer(c_int32_t)", "c_int32_t"),
        #typedef uint32_t xcb_drawable_t;
        "xcb_drawable_t":("integer(c_int32_t)", "c_int32_t"),
        #typedef uint32_t xcb_pixmap_t;
        "xcb_pixmap_t":("integer(c_int32_t)", "c_int32_t"),
        #typedef __uid_t uid_t;
        "uid_t":("integer(c_int32_t)", "c_int32_t"),
        "gint16":("integer(c_int16_t)", "c_int16_t"),
        "guint16":("integer(c_int16_t)", "c_int16_t"),
        "gint8": ("integer(c_int8_t)", "c_int8_t"),
        "guint8": ("integer(c_int8_t)", "c_int8_t"),
        "long":("integer(c_long)", "c_long"),
        "gulong":("integer(c_long)", "c_long"),
        #typedef __time_t time_t;
        "time_t":("integer(c_long)", "c_long"),
        "short":("integer(c_short)", "c_short"),
        "boolean":("logical(c_bool)", "c_bool"),
        "char":("character(kind=c_char)", "c_char"),
        # For gchar & guchar,
        # see https://github.com/vmagnin/gtk-fortran/issues/41#issuecomment-7337877
        "gchar":("integer(kind=c_int8_t)", "c_int8_t"),
        "guchar":("integer(kind=c_int8_t)", "c_int8_t"),
        # typedef gchar** GStrv
        "GStrv":("type(c_ptr)", "c_ptr"),
        # GVariant and GVariantType are structures, generally used with pointers.
        # We don't define the types and kinds but this declaration is
        # needed for GVariant and GVariantType to be treated in Fortran.py:
        "GVariant":("", ""),
        "GVariantType":("", ""),
        "double": ("real(c_double)", "c_double"),
        "gdouble": ("real(c_double)", "c_double"),
        "float":("real(c_float)", "c_float"),
        "gfloat":("real(c_float)", "c_float"),
        "size_t":  ("integer(c_size_t)", "c_size_t"),
        # gsize is the same size than size_t:
        "gsize":  ("integer(c_size_t)", "c_size_t"),
        # GLib asserts that gssize is the same size as gsize (size_t) but signed:
        # see https://discourse.gnome.org/t/where-are-defined-glib-types-in-the-new-doc/14473/4
        "gssize":  ("integer(c_size_t)", "c_size_t"),
        # typedef gsize GType;
        "GType":  ("integer(c_size_t)", "c_size_t"),
        "va_list":("type(c_ptr)", "c_ptr"),
        #typedef void* gpointer;
        "gpointer":("type(c_ptr)", "c_ptr"),
        #typedef struct _GdkAtom *GdkAtom;
        "GdkAtom":("type(c_ptr)", "c_ptr"),
        # GC (Xlib) is it a pointer ?
        "GC":("type(c_ptr)", "c_ptr"),
        #typedef struct _GIConv *GIConv;
        "GIConv":("type(c_ptr)", "c_ptr"),
        "GSignalCMarshaller":("type(c_ptr)", "c_ptr"),
        # typedef gint32 GTime (Deprecated since: 2.62)
        "GTime":("integer(c_int32_t)", "c_int32_t"),
        "GQuark":("integer(c_int32_t)", "c_int32_t"),
        #typedef struct FT_FaceRec_*  FT_Face;
        "FT_Face":("type(c_ptr)", "c_ptr"),
        # X11 types (See /usr/include/X11/Xmd.h), unsigned int (64 bits archi.)
        # or unsigned long (32 bits architecture):
        "Window":("integer(c_long)", "c_long"),
        #define Drawable CARD32
        "Drawable":("integer(c_long)", "c_long"),
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
        #define KeyCode CARD8   => unsigned char
        "KeyCode":("character(kind=c_char)", "c_char"),
        "KeySym":("integer(c_long)", "c_long"),
        # enums:
        "GWin32OSType":("integer(c_int)", "c_int"),
        "GtkLicense":("integer(c_int)", "c_int"),
        "GApplicationFlags":("integer(c_int)", "c_int"),
        "GtkInputHints":("integer(c_int)", "c_int"),
        "GtkInputPurpose":("integer(c_int)", "c_int"),
        "GtkPackType":("integer(c_int)", "c_int"),
        "GtkArrowType":("integer(c_int)", "c_int"),
        "GdkDragAction":("integer(c_int)", "c_int")
    }

    # Two words types:
    TYPES2_DICT = {
        "long double": ("real(c_long_double)", "c_long_double"),
        "unsigned long":("integer(c_long)", "c_long"),
        "unsigned short":("integer(c_short)", "c_short"),
        "unsigned int":("integer(c_int)", "c_int")
    }

    # These lists will be used by the iso_c_binding() function:
    gtk_enums = []
    gtk_funptr = ["GDestroyNotify", "GAsyncReadyCallback"]


    def __init__(self, PATH_DICT):
        """ When an instance is created in cfwrapper.py, the class
            dictionnaries and lists are filled or completed by
            this constructor.
        """
        gtk_types = []

        # Scan all header files to find all enum types, all pointers to
        # functions (funptr) and add derived GTK types:
        for library_path in PATH_DICT:
            for directory in os.walk(library_path):
                for c_file_name in directory[2]:
                    whole_file = open(directory[0] + "/" + c_file_name, 'r',
                                    errors='replace', encoding='utf-8').read()
                    types_enums.gtk_enums += re.findall(r"(?ms)^typedef enum.*?}\s?(\w+);", whole_file)
                    types_enums.gtk_funptr += re.findall(r"(?m)^typedef[ \t]*(?:const)?[ \t]*\w+[ \t]*\*?\s*\(\* ?([\w]*?)\)",
                                            whole_file)
                    gtk_types += re.findall(r"(?m)^typedef *?(?:const)? *?(\w+) *\*? *([\w]+);",
                                            whole_file)

        # Remove duplicated items in types_enums.gtk_funptr:
        converted_to_set = set(types_enums.gtk_funptr)
        types_enums.gtk_funptr = list(converted_to_set)

        # Add one word derived types in the TYPES_DICT:
        for each in gtk_types:
            # Is it already in the dictionnary?
            if each[1] not in types_enums.TYPES_DICT:
                # Is it a variable type or a pointer to a function?
                if each[0] in types_enums.TYPES_DICT:
                    types_enums.TYPES_DICT[each[1]] = types_enums.TYPES_DICT[each[0]]
                elif each[0] in types_enums.gtk_funptr:
                    types_enums.TYPES_DICT[each[1]] = ("type(c_funptr)", "c_funptr")

        # Write all the types in a CSV file:
        with open(SRC_DIR+"gtk-fortran_types.csv", "w", newline="", encoding="utf-8") as csvfile3:
            index_file = csv.writer(csvfile3, delimiter=";", dialect='excel')
            index_file.writerow(["C type", "Fortran type", "Fortran KIND"])
            for each in types_enums.TYPES_DICT:
                index_file.writerow([each, types_enums.TYPES_DICT[each][0], types_enums.TYPES_DICT[each][1]])
            for each in types_enums.TYPES2_DICT:
                index_file.writerow([each, types_enums.TYPES2_DICT[each][0], types_enums.TYPES2_DICT[each][1]])

        # Write all the funptr in a CSV file:
        types_enums.gtk_funptr.sort()
        with open(SRC_DIR+"gtk-fortran_funptr.csv", "w", newline="", encoding="utf-8") as csvfile4:
            index_file = csv.writer(csvfile4, delimiter=";", dialect='excel')
            index_file.writerow(["C type"])
            for each in types_enums.gtk_funptr:
                index_file.writerow([each])
