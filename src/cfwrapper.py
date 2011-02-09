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
# Last modification:  09.02.2011

""" This program helps you writing a GTK+ Fortran interface from C prototypes,
    using ISO_C_BINDING. The result file must be verified visually, 
    errors corrected and unknown types replaced by the user.
    Beware, enum statements are not yet managed.
    Beware, this program is still under heavy development ! 
    It will need some cleanup.  
    Command line:         python cfwrapper.py
    """

import re        # Regular expression library
import sys
import os

def iso_c_binding(declaration, returned):
    """ Returns the Fortran type corresponding to a C type in the 
        ISO_C_BINDING module (limited to C types used in GTK+) """
    global gtk_enums
    global gtk_funptr
    global RGX_TYPE
    global types_dict
       
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
    
    # Is it a pointer ?
    if declaration.find("*") != -1:
        # Is it a string (char or gchar array) ? "unsigned char"   "guchar" gunichar ?
        if (((c_type.find("char") != -1) or (c_type.find("char*") != -1))) and (not returned):
            return "character(kind=c_char), dimension(*)", "c_char"
        else:
            return "type(c_ptr)", "c_ptr"
            
    # Other cases:
    for each in types_dict.keys():
        if each in c_type.split():
            return types_dict[each][0], types_dict[each][1]

    # It is finally an unknown type:
    return "?", "?"


# **********************************************
# Main program
# **********************************************
types_dict = { "int":("integer(c_int)","c_int"), 
    "gint":("integer(c_int)","c_int"),
    "guint":("integer(c_int)","c_int"),          
    "cairo_bool_t":("integer(c_int)","c_int"), 
    "gint64":("integer(c_int64_t)","c_int64_t"), 
    "guint64":("integer(c_int64_t)","c_int64_t"),
    "gint32":("integer(c_int32_t)","c_int32_t"), 
    "guint32":("integer(c_int32_t)","c_int32_t"),
    "gint16":("integer(c_int16_t)","c_int16_t"), 
    "guint16":("integer(c_int16_t)","c_int16_t"),
    "gint8": ("integer(c_int8_t)","c_int8_t"),   
    "guint8": ("integer(c_int8_t)","c_int8_t"),  
    "glong":("integer(c_long)","c_long"),        
    "gulong":("integer(c_long)","c_long"),
    "boolean":("logical(c_bool)","c_bool"),      
    "gboolean":("logical(c_bool)","c_bool"),  
    "double": ("real(c_double)","c_double"),     
    "gdouble": ("real(c_double)","c_double"),     
    "float":("real(c_float)","c_float"),         
    "gfloat":("real(c_float)","c_float"),
    "gsize":  ("integer(c_size_t)","c_size_t"),  
    "gssize":  ("integer(c_size_t)","c_size_t"),
    "GType":  ("integer(c_size_t)","c_size_t"),  
    "size_t":  ("integer(c_size_t)","c_size_t"),
    "gpointer":("type(c_ptr)","c_ptr") }
# TODO: Add these types:
# "short"  "gshort"       
# "long"   
# int"gboolean" ?
# "unsigned short"  "gushort"
# "unsigned long"   "gulong"
# "unsigned int"    "guint"
#typedef unsigned long gsize;   also GType
#typedef signed long gssize;
#typedef void* gpointer;
#typedef const void *gconstpointer;
#**************************************
    
# Regular expressions used to identify the different parts of a C prototype:
RGX_RETURNED_TYPE = re.compile( "^ *([_0-9a-zA-Z ]+ *\**)" )
RGX_FUNCTION_NAME = re.compile( "([0-9a-zA-Z_]+) *\(" )
RGX_ARGUMENTS = re.compile( "\(([0-9a-zA-Z_ ,\*\[\]]*)\).*;$" )
RGX_ARGS = re.compile( " *([0-9a-zA-Z_ \*\[\]]+),?" )
RGX_VAR_TYPE = re.compile( " *([_0-9a-zA-Z]+)[ |\*]" )
RGX_TYPE = re.compile( "^ *((const |G_CONST_RETURN |cairo_public )?\w+)[ \*]?" )
RGX_VAR_NAME = re.compile( "[ |\*]([_0-9a-zA-Z]+)$" )
RGX_UNDERSCORE = re.compile( "^_\w+$" )

# A tabulation:
TAB = "  "

# Errors will be written in that file:
errors_file = open("cfwrapper-errors.txt", "w")

# For statistics:
nb_lines = 0
nb_generated_interfaces = 0
nb_errors = 0
nb_type_errors = 0
nb_files = 0
type_errors_list = []

# This file will be automatically included in gtk.f90: 
F_FILE_NAME = "gtk-auto.f90"
f_file = open(F_FILE_NAME, "w")
f_file.write("! This file is automatically generated by cfwrapper.py \n")
f_file.write("! Please do not modify \n")
f_file.write("! GNU General Public License version 3 \n\n")

path_dict = {"/usr/include/gtk-2.0":"gtk-auto.f90", "/usr/include/cairo":"gtk-auto.f90",
             "/usr/include/pango-1.0":"gtk-auto.f90",  "/usr/include/glib-2.0":"gtk-auto.f90",
             "/usr/include/gdk-pixbuf-2.0":"gtk-auto.f90"}

# First scan to find all enum types, and all pointers to functions (funptr):
gtk_enums = []
gtk_funptr = []
for library_path in path_dict.keys():
    tree = os.walk(library_path)    # A generator
    for directory in tree:
        for c_file_name in directory[2]:
            whole_file = open(directory[0] + "/" + c_file_name, 'rU').read()        
            enum_types = re.findall("(?ms)^typedef enum.*?} (\w+);", whole_file)
            gtk_enums += enum_types
            funptr = re.findall("(?m)^typedef \w+ *\*? *\(\* ?([A-Z][\w]*?)\)", whole_file)
            gtk_funptr += funptr
gtk_enums.sort()
gtk_funptr.sort()

# Scan of all files in the directory and subdirectories:
for library_path in path_dict.keys():
    tree = os.walk(library_path)    # A generator
    for directory in tree:
        for c_file_name in directory[2]:
            # Those files contains two declaration for the same prototypes 
            # (unix, win32 for example) so we exclude them:
            if c_file_name == "gstdio.h" or c_file_name == "giochannel.h":
                continue    # Go to next file

            whole_file = open(directory[0] + "/" + c_file_name, 'rU').read()
            nb_files += 1            
            # *************************
            # Preprocessing of the file
            # *************************
            # Remove C commentaries:
            whole_file = re.sub("(?s)/\*.*?\*/", "", whole_file)
            # Remove C directives:
            whole_file = re.sub("(?m)^#.*$", "", whole_file)
            # Remove TABs and overnumerous spaces:
            whole_file = whole_file.replace("\t", " ")
            whole_file = re.sub("[ ]{2,}", " ", whole_file)
            # Remove C structures
            whole_file = re.sub("(?ms){.*?}[ \w]*;", "", whole_file)
            whole_file = re.sub("(?m)^(enum).*$", "", whole_file)
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
                errors_file.write(directory[0] + "/" + c_file_name + "\n")
                errors_file.write("No function to implement in this file \n\n")
                nb_errors += 1
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
                    errors_file.write(directory[0] + "/" + c_file_name + "\n")
                    errors_file.write("Returned type not found \n")
                    errors_file.write(prototype + "\n\n")
                    nb_errors += 1
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
                        errors_file.write(directory[0] + "/" + c_file_name + "\n")
                        errors_file.write("Unknown data type:    " + type_returned.group(1) +"\n\n")
                        nb_type_errors += 1
                        type_errors_list.append(type_returned.group(1))
                    
                function_name = RGX_FUNCTION_NAME.search(prototype)
                try:
                    f_name = function_name.group(1)
                except AttributeError:
                    errors_file.write(directory[0] + "/" + c_file_name + "\n")
                    errors_file.write("Function name not found \n")
                    errors_file.write(prototype + "\n\n")
                    nb_errors += 1
                    continue    # Next prototype
                
                # A problem to solve with this function/subroutine
                if f_name.find("g_signal_connect_data") != -1:
                    errors_file.write(directory[0] + "/" + c_file_name + "\n")
                    errors_file.write(prototype + "\n\n")
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
                    errors_file.write(directory[0] + "/" + c_file_name + "\n")
                    errors_file.write("Arguments not found \n")
                    errors_file.write(prototype + "\n\n")
                    nb_errors += 1
                    continue    # Next prototype
            
                # Each argument of the function is analyzed:
                declarations = ""
                args_list = ""
                for arg in args:
                    if arg != "void":
                        try:
                            var_type = RGX_VAR_TYPE.search(arg).group(1)
                        except AttributeError:
                            errors_file.write(directory[0] + "/" + c_file_name + "\n")
                            errors_file.write("Variable type not found \n")
                            errors_file.write(prototype + "\n\n")
                            nb_errors += 1
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
                            errors_file.write(directory[0] + "/" + c_file_name + "\n")
                            errors_file.write("Unknown data type:    " + arg +"\n\n")
                            nb_type_errors += 1
                            type_errors_list.append(arg)
                        
                        try:
                            var_name = RGX_VAR_NAME.search(arg).group(1)
                        except AttributeError:
                            errors_file.write(directory[0] + "/" + c_file_name + "\n")
                            errors_file.write("Variable name not found \n")
                            errors_file.write(prototype + "\n\n")
                            nb_errors += 1
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
errors_file.close()

# Print remaining error types:
type_errors_list.sort()
previous = ""
print("   Unkown types:")
for a_type in type_errors_list:
    if a_type != previous:
        print(a_type)
        previous = a_type
        
# Some statistics:
print
print("   Statistics:")
print("nb_files scanned = " + str(nb_files))
print("nb_generated_interfaces = " + str(nb_generated_interfaces))
print("nb_type_errors = " + str(nb_type_errors))
print("nb_errors (others) = " + str(nb_errors))
print("nb_lines treated = " + str(nb_lines))
print

# Test: do the interfaces and the examples compile with gfortran ?
print("Trying now to compile the interface and examples... Please wait...")
#os.system("gfortran -g gtk.f90 ../examples/cairo-tests.f90 `pkg-config --cflags --libs gtk+-2.0`")
os.system("gfortran -g gtk.f90 ../examples/gtkhello2.f90  `pkg-config --cflags --libs gtk+-2.0` -o../examples/gtkhello2.out")
os.system("gfortran -g gtk.f90 ../examples/mandelbrot.f90  `pkg-config --cflags --libs gtk+-2.0` -o../examples/mandelbrot.out")


