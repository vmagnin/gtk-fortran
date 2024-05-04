#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# This file is part of gtk-fortran, a GTK / Fortran interface library.
# Copyright (C) 2023 The gtk-fortran team
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
# Contributed by Vincent Magnin 2023-03-16
# Last modifications: 2023-03-17

""" This script if for testing some functions of the cfwrapper
"""

import sys
# To use ../tools.py which contains the multiline() function
# (needed to import cleaning and analyze):
sys.path.append('../')

# Modules of the cfwrapper:
from analyze import split_prototype

#-------------------------------------
# Testing the analyze of C prototypes:
#-------------------------------------
prototype_list = ["gboolean g_module_close (GModule *module);"]
prototype_list.append("void g_scanner_unexp_token (GScanner *scanner, GTokenType expected_token, const gchar *identifier_spec, const gchar *symbol_spec, const gchar *symbol_name, const gchar *message, gint is_error);")
prototype_list.append("GOBJECT_VAR GType *g_param_spec_types;")
prototype_list.append("const GtkCssLocation * gtk_css_section_get_start_location (const GtkCssSection *section);")
prototype_list.append("gboolean (g_str_has_suffix) (const gchar *str, const gchar *suffix);")
prototype_list.append("GdkPixbufAnimation *gdk_pixbuf_animation_new_from_resource(const char *resource_path, GError **error);")
prototype_list.append("unsigned int cairo_device_get_reference_count (cairo_device_t *device);")

for prototype in prototype_list:
    print(prototype)
    try:
        function_type, f_name, args = split_prototype(prototype)
        print(function_type, " | ", f_name, " | ", args)
    except Exception as exc:
        print(">>> " + str(exc))
    finally:
        print()
