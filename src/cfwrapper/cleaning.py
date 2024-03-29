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
# Last modification: 2023-04-07

""" This module contains functions used to clean header files in the cfwrapper.
"""

import re           # Regular expression library

# Project modules:
from enums import translate_enums


def clean_header_file(c_file_name, whole_file, enums_file):
    """Preprocessing and cleaning of the header file. It also gathers the enums.
       Do not change the order of the regular expressions!
    """

    nb_enums = 0

    # Remove C commentaries:
    whole_file = re.sub(r"(?s)/\*.*?\*/", "", whole_file)

    # Remove static inline functions, because it causes problems
    # to the wrapper (which is looking for prototypes until a ";"):
    whole_file = re.sub(r"(?m)^static inline(.*?\n)+?}", "", whole_file)

    # Remove Deprecated statements (necessary before treating enumerators):
    whole_file = re.sub(r"[ ]\w*_DEPRECATED_TYPE_[\w()]*;", ";", whole_file)
    whole_file = re.sub(r"[ ]\w*_DEPRECATED_ENUMERATOR_IN_[\w()]*[ ]", " ", whole_file)

    # Gather and translate C enumerators to Fortran enumerators,
    # and write them to gtkenums-auto.* file:
    enum_types = re.findall(r"(?ms)^(typedef enum\s*?(?:\w+)?\s*?{.*?})\s*?(\w+);", whole_file)
    f_enum, nb = translate_enums(c_file_name, enum_types)
    nb_enums += nb
    enums_file.write(f_enum)

    # Removing multilines typedef:
    whole_file = re.sub(r"(?m)^typedef([^;]*?\n)+?[^;]*?;$", "", whole_file)
    # Remove C directives (multilines then monoline):
    whole_file = re.sub(r"(?m)^#(.*[\\][\n])+.*?$", "", whole_file)
    whole_file = re.sub(r"(?m)^#.*$", "", whole_file)
    # Remove TABs and overnumerous spaces:
    whole_file = whole_file.replace("\t", " ")
    whole_file = re.sub(r"[ ]{2,}", " ", whole_file)
    # Remove two levels of { } structures:
    for i in [1, 2]:
        whole_file = re.sub(r"(?ms){[^{]*?}$", "", whole_file)
    # Remove structures like: { } a_name;
    whole_file = re.sub(r"(?ms){[^{]*?}[ \w]*?;", "", whole_file)
    # Remove "available_in" and "deprecated" directives:
    whole_file = re.sub(r"(?m)^.*(_AVAILABLE_IN_|_DEPRECATED).*$", "", whole_file)
    whole_file = re.sub(r"G_GNUC_(BEGIN|END)_IGNORE_DEPRECATIONS", "", whole_file)
    # Remove different kind of declarations:
    whole_file = re.sub(r"(?m)^(extern|enum|typedef|union|struct).*$", "", whole_file)
    whole_file = re.sub(r"(?m)^.*(G|CAIRO|GRAPHENE)_(BEGIN|END)_DECLS *$", "", whole_file)
    whole_file = re.sub(r"(?m)^.*(G_UNLOCK|G_LOCK|G_LOCK_DEFINE_STATIC)\(.*;$", "", whole_file)
    whole_file = re.sub(r"(?m)^.*(cairo_public) ", "", whole_file)
    whole_file = re.sub(r"(?m)^(GLIB_VAR|GTKVAR|GDKVAR|GDK_PIXBUF_VAR|GTKMAIN_C_VAR|G_INLINE_FUNC|G_GNUC_WARN_UNUSED_RESULT|_GDK_PIXBUF_EXTERN)"
                        , "", whole_file)   # extern
    whole_file = re.sub(r"(?ms)^(G_DECLARE_INTERFACE|G_DECLARE_DERIVABLE_TYPE) ?\(.*?\)", "", whole_file)
    # Remove GNU macros at the end of declarations (functions prototypes):
    whole_file = re.sub(r"G_GNUC_[\w (),]*;", ";", whole_file)
    whole_file = re.sub(r"G_ANALYZER_NORETURN *;", ";", whole_file)
    # Remove some GNU macros at the beginning of functions prototypes:
    whole_file = re.sub(r"(G_DECLARE_FINAL_TYPE|G_DEFINE_AUTOPTR_CLEANUP_FUNC|GDK_DECLARE_INTERNAL_TYPE) ?\(.*?\)", "", whole_file)
    # Remove empty lines:
    whole_file = re.sub(r"(?m)^\n$", "", whole_file)

    return whole_file, nb_enums


def preprocess_prototypes(preprocessed_list, lines_list):
    """Clean the list of prototypes before analysis
    """
    nb = 0
    i = 0

    for prototype in lines_list:
        nb += 1
        # remove leading and trailing spaces:
        prototype2 = prototype.strip()

        if ";" not in preprocessed_list[i]:
            # Remove line feeds inside a prototype:
            preprocessed_list[i] = preprocessed_list[i].replace("\n", "").strip()
            preprocessed_list[i] += " "+prototype2
        else:
            preprocessed_list.append(prototype2)
            i += 1

        preprocessed_list[i] = preprocessed_list[i].strip()

    return preprocessed_list, nb
