#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Copyright (C) 2011
# Free Software Foundation, Inc.

# This file is part of the gtk-fortran gtk+ Fortran Interface library.

# This is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.

# You should have received a copy of the GNU General Public License along with
# this program; see the files COPYING3 and COPYING.RUNTIME respectively.
# If not, see <http://www.gnu.org/licenses/>.
#
# Contributed by Vincent Magnin, 28.01.2011, Python 2.6.6, Linux Ubuntu 10.10
# Last modification:  15.02.2011

""" This program helps you writing a GTK+ Fortran interface from C prototypes,
    using ISO_C_BINDING. The result file must be verified visually, 
    errors corrected and unknown types replaced by the user.
    Beware, enum statements are not yet managed.
    Beware, this program is still under heavy development ! 
    It will need some cleanup.  
    Command line:         python cfwrapper.py
    """

import re        # Regular expression library
import os

def iso_c_binding(declaration, returned):
    """ Returns the Fortran type corresponding to a C type in the 
        ISO_C_BINDING module (limited to C types used in GTK+) """
    global gtk_enums
    global gtk_funptr
    global RGX_TYPE
    global TYPES_DICT

    try:
        c_type = RGX_TYPE.search(declaration).group(1)
    except AttributeError:
        #print declaration
        return "?", "?"    # error

    # Is it a "typedef enum" ?
    for each in gtk_enums:
        if c_type.find(each) != -1:
            return "integer(c_int)", "c_int"

    # Is it a "typedef      (*     )" ?
    for each in gtk_funptr:
        if c_type.find(each) != -1:
            return "type(c_funptr)", "c_funptr"

    #?????????????????
    # Is it sure ????
    #?????????????????
    if c_type.find("gpointer")!=-1 or c_type.find("gconstpointer")!=-1:
        if returned:
            return "type(c_funptr)", "c_funptr"
        else:
            return "type(c_ptr)", "c_ptr"
    
    # Is it a pointer ?
    if declaration.find("*") != -1:
        # Is it a string (char or gchar array) ? "unsigned char"   "guchar" gunichar ?
        if (((c_type.find("char") != -1) or (c_type.find("char*") != -1))) and (not returned):
            return "character(kind=c_char), dimension(*)", "c_char"
        else:
            return "type(c_ptr)", "c_ptr"

    # Other cases:
    if len(declaration.split()) >= 3:   # Two words type
        for each in TYPES2_DICT.keys():
            if set(each.split()).issubset(set(declaration.split())):
                return TYPES2_DICT[each][0], TYPES2_DICT[each][1]
    else:  # It is a one word type
        for each in TYPES_DICT.keys():
            if each in c_type.split():
                return TYPES_DICT[each][0], TYPES_DICT[each][1]

    # It is finally an unknown type:
    return "?", "?"


def write_error(errorsfile, direc, filename, message, proto, type_error): 
    """ Write errors in the file cfwrapper-errors.txt and increments the counters """
    global nb_errors
    global nb_type_errors
    
    errorsfile.write(direc + "/" + filename + "\n")
    errorsfile.write(message + "\n")
    errorsfile.write(proto + "\n\n")
    if type_error:
        nb_type_errors += 1
    else:
        nb_errors += 1


# **********************************************
# Main program
# **********************************************
# One word types:
TYPES_DICT = { 
    "int":("integer(c_int)","c_int"), 
    "gint":("integer(c_int)","c_int"),
    "guint":("integer(c_int)","c_int"),          
    "cairo_bool_t":("integer(c_int)","c_int"), 
    "gint64":("integer(c_int64_t)","c_int64_t"), 
    "goffset":("integer(c_int64_t)","c_int64_t"), 
    "guint64":("integer(c_int64_t)","c_int64_t"),
    "gint32":("integer(c_int32_t)","c_int32_t"), 
    "guint32":("integer(c_int32_t)","c_int32_t"),
    "GQuark":("integer(c_int32_t)","c_int32_t"),  #typedef guint32 GQuark;
    "gunichar":("integer(c_int32_t)","c_int32_t"),
    "gint16":("integer(c_int16_t)","c_int16_t"), 
    "guint16":("integer(c_int16_t)","c_int16_t"),
    "gint8": ("integer(c_int8_t)","c_int8_t"),   
    "guint8": ("integer(c_int8_t)","c_int8_t"),  
    "glong":("integer(c_long)","c_long"),        
    "gulong":("integer(c_long)","c_long"),
    "short":("integer(c_short)","c_short"),
    "gshort":("integer(c_short)","c_short"),
    "boolean":("logical(c_bool)","c_bool"),
    "gchar":("character(c_char)","c_char"),
    "guchar":("character(c_char)","c_char"),
    "gboolean":("logical(c_bool)","c_bool"),  
    "double": ("real(c_double)","c_double"),     
    "gdouble": ("real(c_double)","c_double"),
    "float":("real(c_float)","c_float"),         
    "gfloat":("real(c_float)","c_float"),
    "gsize":  ("integer(c_size_t)","c_size_t"),  
    "gssize":  ("integer(c_size_t)","c_size_t"),
    "GType":  ("integer(c_size_t)","c_size_t"),  
    "size_t":  ("integer(c_size_t)","c_size_t"),
    "gpointer":("type(c_ptr)","c_ptr"),
    "GdkAtom":("type(c_ptr)","c_ptr"),
    "GC":("type(c_ptr)","c_ptr"),
    "PangoGlyph":("integer(c_int32_t)","c_int32_t"),   #typedef guint32 PangoGlyph;
    # X11 types (See /usr/include/X11/Xmd.h), unsigned int (64 bits archi.)
    # or unsigned long (32 bits architecture) :
    "Window":("integer(c_long)","c_long"),
    "Drawable":("integer(c_long)","c_long"),  #define Drawable CARD32
    "Font":("integer(c_long)","c_long"),
    "Pixmap":("integer(c_long)","c_long"),
    "Cursor":("integer(c_long)","c_long"),
    "Colormap":("integer(c_long)","c_long"),
    "GContext":("integer(c_long)","c_long"),
    "Atom":("integer(c_long)","c_long"),
    "VisualID":("integer(c_long)","c_long"),
    "Time":("integer(c_long)","c_long"),
    "KeyCode":("character(c_char)","c_char"),   #define KeyCode CARD8   => unsigned char
    "KeySym":("integer(c_long)","c_long"),
     }

# TODO: Add or verify these types:
# "long"   
# int"gboolean" ?
#   "gushort"
#typedef unsigned long gsize;   also GType
#typedef signed long gssize;
#typedef void* gpointer;
#typedef const void *gconstpointer;
# GC (Xlib) is it a pointer ?
#typedef struct _GdkAtom            *GdkAtom;  So GdkAtom is a pointer ?
#typedef gint32  GTime;
#typedef guint16 GDateYear;
#typedef guint8  GDateDay;   /* day of the month */
#typedef struct _GDate GDate;
#**************************************

# Two words types
TYPES2_DICT = {
    "long double": ("real(c_long_double)","c_long_double"),
    "unsigned long":("integer(c_long)","c_long"),
    "unsigned short":("integer(c_short)","c_short"),
    "unsigned long":("integer(c_long)","c_long"),
    "unsigned int":("integer(c_int)","c_int")
}

# Regular expressions used to identify the different parts of a C prototype:
RGX_RETURNED_TYPE = re.compile( "^ *([_0-9a-zA-Z ]+ *\**)" )
RGX_FUNCTION_NAME = re.compile( "([0-9a-zA-Z_]+) *\(" )
RGX_ARGUMENTS = re.compile( "\(([0-9a-zA-Z_ ,\*\[\]]*)\).*;$" )
RGX_ARGS = re.compile( " *([0-9a-zA-Z_ \*\[\]]+),?" )
RGX_VAR_TYPE = re.compile( " *([_0-9a-zA-Z]+)[ |\*]" )
RGX_TYPE = re.compile( "^ *((const |G_CONST_RETURN |cairo_public )?\w+)[ \*]?" )
RGX_VAR_NAME = re.compile( "[ |\*]([_0-9a-zA-Z]+)$" )
RGX_UNDERSCORE = re.compile( "^_\w+$" )

# Errors will be written in that file:
ERRORS_FILE = open("cfwrapper-errors.txt", "w")
# A tabulation:
TAB = "  "

# For statistics:
nb_lines = 0
nb_generated_interfaces = 0
nb_errors = 0
nb_type_errors = 0
nb_files = 0
type_errors_list = []

# Libraries to parse and resulting Fortran files: 
PATH_DICT = {"/usr/include/gtk-2.0":"gtk-auto.f90", "/usr/include/cairo":"gtk-auto.f90",
             "/usr/include/pango-1.0":"gtk-auto.f90",  "/usr/include/glib-2.0":"gtk-auto.f90",
             "/usr/include/gdk-pixbuf-2.0":"gtk-auto.f90"}

#*************************************************************************
# Pass 1 : to find all enum types, and all pointers to functions (funptr)
#*************************************************************************
gtk_enums = []
gtk_funptr = []
for library_path in PATH_DICT.keys():
    tree = os.walk(library_path)    # A generator
    for directory in tree:
        for c_file_name in directory[2]:
            whole_file = open(directory[0] + "/" + c_file_name, 'rU').read()        
            enum_types = re.findall("(?ms)^typedef enum.*?} (\w+);", whole_file)
            gtk_enums += enum_types
            funptr = re.findall("(?m)^typedef \w+ *\*? *\(\* ?([A-Z][\w]*?)\)", whole_file)
            gtk_funptr += funptr
# Useful only for printing:
gtk_enums.sort()
gtk_funptr.sort()

#**************************************************************************************
# Pass 2 : Scan of all files in the directory and subdirectories to generate interfaces
#**************************************************************************************
opened_files = []
for library_path in PATH_DICT.keys():
    tree = os.walk(library_path)    # A generator
    # This file will be automatically included in another .f90: 
    F_FILE_NAME = PATH_DICT[library_path]
    if not (F_FILE_NAME in opened_files):
        f_file = open(F_FILE_NAME, "w")
        opened_files.append(F_FILE_NAME)
        f_file.write("! This file is automatically generated by cfwrapper.py \n")
        f_file.write("! Please do not modify \n")
        f_file.write("! GNU General Public License version 3 \n\n")

    for directory in tree:
        for c_file_name in directory[2]:
            # Those files cause problems so we exclude them:
            if c_file_name in ["gstdio.h", "giochannel.h"]: #, "gmessages.h"] :
                continue    # Go to next file

            whole_file = open(directory[0] + "/" + c_file_name, 'rU').read()
            nb_files += 1            
            # *************************
            # Preprocessing of the file
            # *************************
            # Remove C commentaries:
            whole_file = re.sub("(?s)/\*.*?\*/", "", whole_file)
            # Remove C directives (multilines then monoline):
            # in a python regular expression \\\\=\\=\
            whole_file = re.sub("(?m)^#(.*[\\\\][\\n])+.*?$", "", whole_file)
            whole_file = re.sub("(?m)^#.*$", "", whole_file)
            # Remove TABs and overnumerous spaces:
            whole_file = whole_file.replace("\t", " ")
            whole_file = re.sub("[ ]{2,}", " ", whole_file)
            # Remove C structures
            whole_file = re.sub("(?ms){.*?}[ \w]*;", "", whole_file)
            whole_file = re.sub("(?m)^(enum).*$", "", whole_file)
            
            #whole_file = re.sub("(?m)^typedef([^;]*?\n)*?[^;]*?;", "", whole_file)
            
            whole_file = re.sub("(?m)^(typedef|union|struct).*$", "", whole_file)
            whole_file = re.sub("(?m)^.*(G_BEGIN_DECLS|CAIRO_BEGIN_DECLS) *$", "", whole_file)
            whole_file = re.sub("(?m)^.*(G_END_DECLS|CAIRO_END_DECLS) *$", "", whole_file)
            # Remove empty lines:
            whole_file = re.sub("(?m)^\n$", "", whole_file)
            #whole_file = re.sub("(?m)^$", "", whole_file)
            # End of preprocessing
            # *************************
                        
            lines_list = whole_file.splitlines(True)
            corrected_lines_list = []
            try:
                corrected_lines_list.append(lines_list[0])
            except IndexError:
                write_error(ERRORS_FILE, directory[0], c_file_name, 
                            "No function to implement in this file", "", False)
                continue    # Next file

            # Preprocessing of the C prototypes:
            i = 0
            for prototype in lines_list:
                nb_lines += 1
                # remove leading and trailing spaces
                prototype2 = str.strip(prototype)

                if corrected_lines_list[i].find(";") == -1:
                    # Remove line feeds inside a prototype
                    corrected_lines_list[i] = corrected_lines_list[i].replace("\n", "")
                    corrected_lines_list[i] += " "+prototype2
                else:
                    corrected_lines_list.append(prototype2)
                    i = i + 1

            # Each prototype is now analyzed:
            for prototype in corrected_lines_list:
                error_flag = False

                type_returned = RGX_RETURNED_TYPE.search(prototype)
                try:
                    function_type = type_returned.group(1)
                except AttributeError:
                    write_error(ERRORS_FILE, directory[0], c_file_name, 
                                "Returned type not found", prototype, False)
                    continue    # Next prototype

                # Is it a function or a subroutine ?
                if function_type.find("void") != -1:
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
                        write_error(ERRORS_FILE, directory[0], c_file_name, 
                            "Unknown data type:    " + type_returned.group(1), prototype, True)
                        type_errors_list.append(type_returned.group(1))
                    
                function_name = RGX_FUNCTION_NAME.search(prototype)
                try:
                    f_name = function_name.group(1)
                except AttributeError:
                    write_error(ERRORS_FILE, directory[0], c_file_name, 
                                "Function name not found", prototype, False)
                    continue    # Next prototype
                
                # A problem to solve with this function/subroutine
                if f_name.find("g_signal_connect_data") != -1:
                    write_error(ERRORS_FILE, directory[0], c_file_name, "", prototype, False)
                    continue
                    
                # Functions with a name beginning by an underscore will be excluded:
                if RGX_UNDERSCORE.match(function_name.group(1)) != None:
                    continue    # Next prototype
                
                if function_name.group(1) == "g_signal_connect" or function_name.group(1) == "gtk_init":
                    continue    # Next prototype
                    
                arguments = RGX_ARGUMENTS.search(prototype)
                # Optional arguments are not managed !
                try:
                    args = RGX_ARGS.findall(arguments.group(1))
                except AttributeError:
                    write_error(ERRORS_FILE, directory[0], c_file_name, 
                                "Arguments not found", prototype, False)
                    continue    # Next prototype
            
                # Each argument of the function is analyzed:
                declarations = ""
                args_list = ""
                for arg in args:
                    if arg != "void":
                        try:
                            var_type = RGX_VAR_TYPE.search(arg).group(1)
                        except AttributeError:
                            write_error(ERRORS_FILE, directory[0], c_file_name, 
                                        "Variable type not found", prototype, True)
                            continue    # Next argument

                        f_type, iso_c = iso_c_binding(arg, False)
                        if f_type.find("c_") != -1:
                            if f_use == "":
                                f_use = iso_c
                            else:
                                RGX_ISO_C = re.compile( "("+iso_c+")"+"([^\w]|$)" )
                                if RGX_ISO_C.search(f_use) == None:
                                    f_use += ", " + iso_c       # each iso appears only once
                        elif f_type.find("?") != -1:
                            error_flag = True
                            write_error(ERRORS_FILE, directory[0], c_file_name, 
                                        "Unknown data type:    " + arg, prototype, True)
                            type_errors_list.append(arg)
                        
                        try:
                            var_name = RGX_VAR_NAME.search(arg).group(1)
                        except AttributeError:
                            write_error(ERRORS_FILE, directory[0], c_file_name, 
                                        "Variable name not found", prototype, False)
                            continue    # Next argument
            
                        if f_type.find("(*)") != -1:    # character array with unknown dimension
                            passvar = ""
                        else:
                            passvar = ", value"
                        declarations += 3*TAB + f_type + passvar + " :: " + var_name + "\n"
                        if args_list == "":
                            args_list = var_name
                        else:
                            args_list += ", " + var_name
                
                # Write the Fortran interface in the .f90 file:
                if (error_flag == False):
                    interface = 2*TAB + "! " + prototype + "\n"
                    
                    # The first line must not be too long (<132):
                    first_line = 2*TAB + f_procedure + f_name + "(" + args_list + ") bind(c)"
                    while len(first_line) > 80:
                        interface += first_line[0:80] + "&\n"
                        first_line = 5*TAB + "&" + first_line[80:] 
                    interface += first_line + " \n"
                    
                    interface += 3*TAB + "use iso_c_binding, only: " + f_use + "\n"
                    if isfunction:
                        interface += 3*TAB + returned_type + " :: " + f_name + "\n"
                    interface += declarations
                    interface += 2*TAB + f_the_end + "\n\n" 
                    
                    f_file.write(interface)
                    nb_generated_interfaces += 1
          
# *********************
# End of the processing
# *********************

f_file.close()
ERRORS_FILE.close()

# Print remaining error types:
type_errors_list.sort()
previous = ""
print("=== Unkown types ===")
for a_type in type_errors_list:
    if a_type != previous:
        #print(a_type)
        previous = a_type
        
# Some statistics:
print
print("=== Statistics ===")
print("nb_files scanned = " + str(nb_files))
print("nb_generated_interfaces = " + str(nb_generated_interfaces))
print("nb_type_errors = " + str(nb_type_errors))
print("nb_errors (others) = " + str(nb_errors))
print("nb_lines treated = " + str(nb_lines))
print

# Test: do the interfaces and the examples compile with gfortran ?
print("=== Trying now to compile the interface and examples... ===")
os.system("gfortran -c gtk.f90 `pkg-config --cflags --libs gtk+-2.0`")
os.system("gfortran gtk.o ../examples/cairo-basics.f90 -ocairo-basics.out `pkg-config --cflags --libs gtk+-2.0`")
os.system("gfortran gtk.o ../examples/gtkhello2.f90 -ogtkhello2.out `pkg-config --cflags --libs gtk+-2.0`")
os.system("gfortran gtk.o ../examples/mandelbrot_pixbuf.f90 -omandelbrot_pixbuf.out `pkg-config --cflags --libs gtk+-2.0`")
