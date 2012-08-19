#!/usr/bin/env python
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
# Last modification:  19.05.2012 (Python 2.7.3, Linux Ubuntu 12.04)

""" This program generates the *-auto.f90 files
    from the C header files of GTK+ in Linux.
    Command line:         python cfwrapper.py
    """

import re        # Regular expression library
import os
import time
import csv

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
        return "?", "?"    # error

    # Is it an array ?
    if declaration.find("[") != -1:
        array = ", dimension(*)"
    else:
        array = ""

    # Is it a "typedef enum" ?
    for each in gtk_enums:
        if c_type.find(each) != -1:
            return "integer(c_int)", "c_int"

    # Is it a pointer toward a function ?
    for each in gtk_funptr:
        if c_type.find(each) != -1:
            return "type(c_funptr)", "c_funptr"

    #typedef void* gpointer;
    if c_type.find("gpointer")!=-1 or c_type.find("gconstpointer")!=-1:
        return "type(c_ptr)", "c_ptr"

    # Is it a pointer ?
    if declaration.find("*") != -1:
        # Is it a string (char or gchar array) ? "unsigned char"   "guchar" gunichar ?
        if (((c_type.find("char") != -1) or (c_type.find("char*") != -1))) and (not returned):
            if declaration.find("**") != -1:
                return "type(c_ptr), dimension(*)", "c_ptr"
            else:
                return "character(kind=c_char), dimension(*)", "c_char"
        else:
            return "type(c_ptr)", "c_ptr"

    # Other cases:
    if len(declaration.split()) >= 3:   # Two words type
        for each in TYPES2_DICT.keys():
            if set(each.split()).issubset(set(declaration.split())):
                return TYPES2_DICT[each][0] + array, TYPES2_DICT[each][1]
    else:  # It is a one word type
        for each in TYPES_DICT.keys():
            if each in c_type.split():
                return TYPES_DICT[each][0] + array, TYPES_DICT[each][1]

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


def multiline(ch, maxlength): 
    """Split a long line in a multiline, following Fortran syntax."""
    result = ""
    while len(ch) > maxlength-1:
        result += ch[0:maxlength-1] + "&\n"
        ch = "&"+ ch[maxlength-1:]
    result += ch
    return result


def set_bit_field(match):
    """ Returns the Fortran bitfield from a C enum flag"""
    b = int(match.group(1))
    #field = "1"
    #for i in range(0, b):
    #    field += "0"
    #return "b'"+field+"'"
    s = "ISHFTC(1, " + str(b) + ")"
    return s


def translate_enums(errorsfile, enum_list):
    """Receive a C enum and returns a Fortran enum"""
    f_enum = ""
    BIT_FIELDS = re.compile("1 *<< *(\d+)")
    if enum_list != []:
        for each in enum_list:
            enum = each[0]
            name = each[1]
            
            if name in ["GSocketFamily", "GSocketMsgFlags", "GdkPixdataType"]:
                return ""
                
            parameters = re.findall("(?ms){(.*)}", enum)
            parameters[0] = re.sub("(?m)^#.*$", "", parameters[0])
            # Remove TABs and overnumerous spaces:
            parameters[0] = parameters[0].replace("\t", " ")
            parameters[0] = re.sub("[ ]{2,}", " ", parameters[0])

            # Delete characters (   ) and , if they are not between quotes:
            parameters[0] = re.sub("(?<!')(\()(?!')", "", parameters[0])
            parameters[0] = re.sub("(?<!')(\))(?!')", "", parameters[0])
            parameters[0] = re.sub("(?<!')(,)(?!')", "", parameters[0])
            parameters[0] = re.sub("(?m),$", "", parameters[0])
            
            # Is it in hexadecimal ?
            parameters[0] = re.sub("0x([0-9A-Fa-f]+)", "INT(z'\\1')", parameters[0])
            # Is it a char ?
            # Est-ce que ça marche ??????? (voir entiers)
            parameters[0] = re.sub("('.?')", "iachar(\\1)", parameters[0])
            # Is it a bit field ?
            # on ne sait pas comment sont codés les entiers !!!!!
            # Utiliser les fonctions sur les bits ??????
            #parameters[0] = re.sub("1 *<< *(\d+)", "2**\\1", parameters[0])
            parameters[0] = BIT_FIELDS.sub(set_bit_field, parameters[0])

            # complement
            parameters[0] = re.sub("~(\w+)", "not(\\1)", parameters[0])
            # logical or
            parameters[0] = re.sub("([\w\(\)]+)\s*\|\s*([\w\(\)]+)", "ior(\\1 , \\2)", parameters[0])

            # Renamed flags (have the same name as a GTK+ function):
            parameters[0] = re.sub("(?m)^\s*ATK_HYPERLINK_IS_INLINE", "ATK_HYPERLINK_IS_INLINE_F", parameters[0])
            parameters[0] = re.sub("(?m)^\s*GDK_PROPERTY_DELETE", "GDK_PROPERTY_DELETE_F", parameters[0])
            parameters[0] = re.sub("(?m)^\s*GDK_DRAG_STATUS", "GDK_DRAG_STATUS_F", parameters[0])
            parameters[0] = re.sub("(?m)^\s*GDK_DRAG_MOTION", "GDK_DRAG_MOTION_F", parameters[0])
            # Integer size problem:
            parameters[0] = re.sub("(?m)^\s*G_PARAM_DEPRECATED.*$", "", parameters[0])

            parameters[0] = re.sub("(?m)^\s*(\w+)", "    enumerator :: \\1", parameters[0])

            f_enum += "enum, bind(c)    !" + name + "\n"
            f_enum += parameters[0]
            f_enum += "end enum\n \n"
    # Remove empty lines:
    f_enum = re.sub("(?m)^ *\n$", "", f_enum)
    return f_enum
    
    
# **********************************************
# Main program
# **********************************************
# One word types:
TYPES_DICT = { 
    "int":("integer(c_int)","c_int"), 
    "guint":("integer(c_int)","c_int"),          
    "Bool":("integer(c_int)","c_int"),    #define Bool int    => Xlib.h
    #  On UNIX, processes are identified by a process id (an integer),
    # while Windows uses process handles (which are pointers).
    "GPid":("integer(c_int)","c_int"),
    "gint64":("integer(c_int64_t)","c_int64_t"), 
    "goffset":("integer(c_int64_t)","c_int64_t"), 
    "guint64":("integer(c_int64_t)","c_int64_t"),
    "gint32":("integer(c_int32_t)","c_int32_t"), 
    "guint32":("integer(c_int32_t)","c_int32_t"),
    "GdkWChar":("integer(c_int32_t)","c_int32_t"),  #typedef guint32 GdkWChar;
    "xcb_drawable_t":("integer(c_int32_t)","c_int32_t"),  #typedef uint32_t xcb_drawable_t;
    "xcb_pixmap_t":("integer(c_int32_t)","c_int32_t"),  #typedef uint32_t xcb_pixmap_t;
    "uid_t":("integer(c_int32_t)","c_int32_t"),  #typedef __uid_t uid_t;
    "gint16":("integer(c_int16_t)","c_int16_t"), 
    "guint16":("integer(c_int16_t)","c_int16_t"),
    "gint8": ("integer(c_int8_t)","c_int8_t"),
    "guint8": ("integer(c_int8_t)","c_int8_t"),
    "long":("integer(c_long)","c_long"),
    "gulong":("integer(c_long)","c_long"),
    "time_t":("integer(c_long)","c_long"),  #typedef __time_t time_t;
    "short":("integer(c_short)","c_short"),
    "boolean":("logical(c_bool)","c_bool"),
    "gchar": ("integer(kind=c_int8_t)", "c_int8_t"), # ("character(kind=c_char)","c_char"),
    "guchar":("integer(kind=c_int8_t)", "c_int8_t"), # ("character(kind=c_char)","c_char"),
#    "gboolean":("logical(c_bool)","c_bool"), typedef int gint; typedef gint gboolean;
    "double": ("real(c_double)","c_double"),
    "float":("real(c_float)","c_float"),
    "gsize":  ("integer(c_size_t)","c_size_t"),    #typedef unsigned long gsize;   also GType  
    "gssize":  ("integer(c_size_t)","c_size_t"),   #typedef signed long gssize;
    "GType":  ("integer(c_size_t)","c_size_t"),
    "size_t":  ("integer(c_size_t)","c_size_t"),
    "va_list":("type(c_ptr)","c_ptr"),
    "gpointer":("type(c_ptr)","c_ptr"), #typedef void* gpointer;
    "GdkAtom":("type(c_ptr)","c_ptr"),  #typedef struct _GdkAtom *GdkAtom; 
    "GC":("type(c_ptr)","c_ptr"),       # GC (Xlib) is it a pointer ?
    "GIConv":("type(c_ptr)","c_ptr"),   #typedef struct _GIConv *GIConv;
    "GSignalCMarshaller":("type(c_ptr)","c_ptr"), 
    "FT_Face":("type(c_ptr)","c_ptr"),  #typedef struct FT_FaceRec_*  FT_Face;  
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
    "Picture":("integer(c_long)","c_long"),
    "XID":("integer(c_long)","c_long"),
    "VisualID":("integer(c_long)","c_long"),
    "Time":("integer(c_long)","c_long"),
    "KeyCode":("character(kind=c_char)","c_char"),   #define KeyCode CARD8   => unsigned char
    "KeySym":("integer(c_long)","c_long"),
     }
# TODO:
#typedef union  _GTokenValue     GTokenValue;
#typedef struct _GtkPropertyMark   GtkPropertyMark;

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
RGX_TYPE = re.compile( "^ *((const |G_CONST_RETURN |cairo_public |G_INLINE_FUNC )?\w+)[ \*]?" )
RGX_VAR_NAME = re.compile( "[ |\*]([_0-9a-zA-Z]+)(?:\[\])?$" )
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
nb_variadic = 0
nb_files = 0
type_errors_list = []

# Libraries to parse and resulting Fortran files (must have a -auto.f90 termination): 
PATH_DICT = { "/usr/include/gtk-3.0/gtk":"gtk-auto.f90",
              "/usr/include/gtk-3.0/gdk":"gdk-auto.f90",
              "/usr/include/gtk-unix-print-2.0/gtk":"unix-print-auto.f90",
              "/usr/include/cairo":"cairo-auto.f90",
              "/usr/include/pango-1.0":"pango-auto.f90",
              "/usr/include/glib-2.0":"glib-auto.f90",
              "/usr/include/gdk-pixbuf-2.0":"gdk-pixbuf-auto.f90",
              "/usr/include/atk-1.0":"atk-auto.f90",}

#*************************************************************************
# Pass 1 : to find all enum types, all pointers to functions (funptr)
# and add derived GTK+ types
#*************************************************************************
gtk_enums = []
gtk_funptr = []
gtk_types = []
for library_path in PATH_DICT.keys():
    tree = os.walk(library_path)    # A generator
    for directory in tree:
        for c_file_name in directory[2]:
            whole_file = open(directory[0] + "/" + c_file_name, 'rU').read()
            enum_types = re.findall("(?ms)^typedef enum.*?}\s?(\w+);", whole_file)
            gtk_enums += enum_types
            funptr = re.findall("(?m)^typedef[ \t]*(?:const)?[ \t]*\w+[ \t]*\*?\s*\(\* ?([\w]*?)\)", whole_file)
            gtk_funptr += funptr
            types = re.findall("(?m)^typedef *?(?:const)? *?(\w+) *\*? *([\w]+);", whole_file)
            gtk_types += types

for each in gtk_types:
    if each[1] not in TYPES_DICT.keys():
        if each[0] in TYPES_DICT.keys():
            TYPES_DICT[each[1]] = TYPES_DICT[each[0]]
        elif each[0] in gtk_funptr:
            TYPES_DICT[each[1]] = ("type(c_funptr)", "c_funptr")

# Useful only for printing:
gtk_enums.sort()
gtk_funptr.sort()

#**************************************************************************************
# Pass 2 : Scan of all files in the directory and subdirectories to generate interfaces
#**************************************************************************************
file_header = """! Automatically generated by cfwrapper.py on """ + time.asctime(time.localtime()) + """
! Please do not modify.
! This file is part of the gtk-fortran GTK+ Fortran Interface library.
! GNU General Public License version 3
"""

# TODO: split enums in their respective files
enums_file = open("gtkenums-auto.f90", "w")
enums_file.write(file_header)

index = []

opened_files = []
used_types = []
for library_path in PATH_DICT.keys():
    tree = os.walk(library_path)    # A generator

    F_FILE_NAME = PATH_DICT[library_path]
    if not (F_FILE_NAME in opened_files):
        f_file = open(F_FILE_NAME, "w")
        opened_files.append(F_FILE_NAME)
        module_name = re.search("^(.+)-auto\.f90", F_FILE_NAME).group(1)
        module_name = module_name.replace("-", "_")
        if module_name != "gtk":
            if module_name == "glib":
                module_name = "g"
            f_file.write(file_header+"\nmodule " + module_name + 
                        "\nimplicit none\ninterface\n\n")

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
            
            
            enum_types = re.findall("(?ms)^(typedef enum\s*?(?:\w+)?\s*?{.*?})\s*?(\w+);", whole_file)
            enums_file.write(translate_enums(ERRORS_FILE, enum_types))
            
            # removing multilines typedef:
            whole_file = re.sub("(?m)^typedef([^;]*?\n)+?[^;]*?;$", "", whole_file)
            # Remove C directives (multilines then monoline):
            # in a python regular expression \\\\=\\=\
            #whole_file = re.sub("(?ms)#ifdef .*?#endif.*?$", "", whole_file)
            whole_file = re.sub("(?m)^#(.*[\\\\][\\n])+.*?$", "", whole_file)
            whole_file = re.sub("(?m)^#.*$", "", whole_file)
            # Remove TABs and overnumerous spaces:
            whole_file = whole_file.replace("\t", " ")
            whole_file = re.sub("[ ]{2,}", " ", whole_file)
            # Remove C structures
            #whole_file = re.sub("(?ms)^static.*}$", "", whole_file)
            
            whole_file = re.sub("(?ms){.*?}[ \w]*;", "", whole_file)
            whole_file = re.sub("(?m)^(enum).*$", "", whole_file)
            whole_file = re.sub("(?m)^(typedef|union|struct).*$", "", whole_file)
            whole_file = re.sub("(?m)^.*(G_BEGIN_DECLS|CAIRO_BEGIN_DECLS) *$", "", whole_file)
            whole_file = re.sub("(?m)^.*(G_END_DECLS|CAIRO_END_DECLS) *$", "", whole_file)
            whole_file = re.sub("(?m)^.*(G_UNLOCK|G_LOCK|G_LOCK_DEFINE_STATIC)\(.*;$", "", whole_file)
            
            whole_file = re.sub("(?m)^.*(cairo_public|extern) ", "", whole_file)
            whole_file = re.sub("(?m)^(GLIB_VAR|GTKVAR|GDKVAR|GDK_PIXBUF_VAR|GTKMAIN_C_VAR|G_INLINE_FUNC)"
                                , "", whole_file)   # extern
            # Remove empty lines:
            whole_file = re.sub("(?m)^\n$", "", whole_file)
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
                                    
                # Functions with a name beginning by an underscore will be excluded:
                if RGX_UNDERSCORE.match(f_name) != None:
                    continue    # Next prototype
                
                if f_name in ["gtk_init","g_bit_nth_lsf","g_once_init_enter","g_trash_stack_push"]:
                    continue    # Next prototype
                
                    
                arguments = RGX_ARGUMENTS.search(prototype)
                try:
                    args = RGX_ARGS.findall(arguments.group(1))
                except AttributeError:
                    write_error(ERRORS_FILE, directory[0], c_file_name, 
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
                            write_error(ERRORS_FILE, directory[0], c_file_name, 
                                        "Variable type not found", prototype, True)
                            continue    # Next argument

                        f_type, iso_c = iso_c_binding(arg, False)
                        if iso_c not in used_types:
                                    used_types.append(iso_c)
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
            
                        if f_type.find("(*)") != -1:    #array with unknown dimension
                            passvar = ""                #passed by adress
                        else:
                            passvar = ", value"
                            
                        declarations += 1*TAB + f_type + passvar + " :: " + var_name + "\n"
                        if args_list == "":
                            args_list = var_name
                        else:
                            args_list += ", " + var_name
                
                # Write the Fortran interface in the .f90 file:
                if (error_flag == False):
                    interface = 0*TAB + "! " + prototype + "\n"
                    first_line = 0*TAB + f_procedure + f_name + "(" + args_list + ") bind(c)"
                    interface += multiline(first_line, 80) + " \n"
                    
                    interface += 1*TAB + "use iso_c_binding, only: " + f_use + "\n"
                    if isfunction:
                        interface += 1*TAB + returned_type + " :: " + f_name + "\n"
                    interface += declarations
                    interface += 0*TAB + f_the_end + "\n\n" 
                    
                    f_file.write(interface)
                    index.append([module_name, f_name, F_FILE_NAME, directory[0]+"/"+c_file_name, prototype])
                    nb_generated_interfaces += 1
            
    if module_name != "gtk":
        f_file.write("end interface\nend module "+module_name+"\n")
        f_file.close()
          
# *********************
# End of the processing
# *********************

index_file = csv.writer(open("gtk-fortran-index.csv", "w"), delimiter=";")
index.sort()
index_file.writerows(index)

ERRORS_FILE.close()
enums_file.close()

# Print remaining error types:
type_errors_list.sort()
TYPE_ERRORS_FILE = open("cfwrapper-type_errors.txt", "w")
previous = ""
for a_type in type_errors_list:
    #if a_type != previous:
    TYPE_ERRORS_FILE.write(a_type+"\n")
    previous = a_type
TYPE_ERRORS_FILE.close()

# Some statistics:
print
print("=== Statistics ===")
print("nb_files scanned = " + str(nb_files))
print("nb_generated_interfaces = " + str(nb_generated_interfaces))
print("nb_type_errors = " + str(nb_type_errors))
print("nb_errors (others) = " + str(nb_errors))
print("nb_lines treated = " + str(nb_lines))
print("nb_variadic functions = " + str(nb_variadic)) 
print
#print(used_types)

# Test: do the interfaces and the examples compile with gfortran ?
#print("=== Trying now to compile the interface and examples... ===")
#os.system("gfortran -c gtk.f90 `pkg-config --cflags --libs gtk+-2.0`")
#os.system("gfortran gtk.o ../examples/cairo-basics.f90 -ocairo-basics.out `pkg-config --cflags --libs gtk+-2.0`")
#os.system("gfortran gtk.o ../examples/gtkhello2.f90 -ogtkhello2.out `pkg-config --cflags --libs gtk+-2.0`")
#os.system("gfortran gtk.o ../examples/mandelbrot_pixbuf.f90 -omandelbrot_pixbuf.out `pkg-config --cflags --libs gtk+-2.0`")
