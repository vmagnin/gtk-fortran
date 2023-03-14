#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# This file is part of gtk-fortran, a GTK / Fortran interface library.
# Copyright (C) 2022 The gtk-fortran team
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
#-------------------------------------------------------------------------------
# Contributed by Vincent Magnin, 2022-04-26
# Last modification: 2022-04-28
# Tested with Python 3.10.4, Kubuntu 22.04
# Pylint rate: 9.30/10
#-------------------------------------------------------------------------------

""" Generates markdown files for the HL gtk-fortran documentation.
    Usage: ./extract_hl_doc.py
"""

# Regular expression library:
import re

def write_in_adequate_file(source_file_name, string):
    """ Writes the string in the adequate markdown file
    """
    if source_file_name == "gtk-sup.f90":
        sup_routines_file.write(string)
    elif source_file_name in ["gtk-draw-hl.f90", "gdk-pixbuf-hl.f90"]:
        hl_drawing_api_file.write(string)
    else:
        hl_api_file.write(string)

#*************************************************************************
# Main program
#*************************************************************************

# Note: the content of gtk-hl.f90 is not used in the documentation.
FILES_LIST = [  "gtk-hl-container.f90",
                "gtk-hl-button.f90",
                "gtk-hl-combobox.f90",
                "gtk-hl-entry.f90",
                "gtk-hl-spin-slider.f90",
                "gtk-hl-infobar.f90",
                "gtk-hl-dialog.f90",
                "gtk-hl-progress.f90",
                "gtk-hl-assistant.f90",
                "gtk-hl-chooser.f90",
                "gtk-hl-tree.f90",
                "gtk-hl-misc.f90",
                "gtk-draw-hl.f90",
                "gdk-pixbuf-hl.f90",
                "gtk-sup.f90"]

INPUT_DIR = "./"
OUTPUT_DIR = "../../gtk-fortran.wiki/"

hl_api_file = open(OUTPUT_DIR+"Highlevel-api.md", "w", encoding='utf-8')
hl_api_file.write("# gtk-fortran High Level API\n\n")

hl_drawing_api_file = open(OUTPUT_DIR+"High-level-drawing-api.md", "w", encoding='utf-8')
hl_drawing_api_file.write("""\
# gtk-fortran High Level API

The graphics specific modules provide a high level interface to the
GtkDrawingArea widget which is compatible with the `plplot` library and
at least in simple cases relieves the user of the hassle of managing
redraws. There is also a high level interface to the gdk-pixbuf library
that allows the easy mapping of 2 and 3 dimensional Fortran arrays to
GdkPixbuf objects.
""")

sup_routines_file = open(OUTPUT_DIR+"Supplementary-routines.md", "w", encoding='utf-8')
sup_routines_file.write("# gtk-fortran High Level API\n\n")

#*************************************************************************
# Regular expressions for parsing the Fortran files
#*************************************************************************
# There should be only one module header per file,
# delimited by the !* and !/ tags:
SECTION_HEADER = re.compile(r"(?ms)!\*.*!/")
# Fortran code lines are beginning with spaces and an alphabetic character:
FORTRAN_BLOCK = re.compile(r"(?m)^[ \t]*[a-z]+.*$")
# Finds both !* and !/ tags for deletion:
HEADER_START_AND_END = re.compile(r"(?m)^[ \t]*(!\*|!/)")
# The routines interaces are delimited by !+ and !-
SECTION_ROUTINE = re.compile(r"(?ms)!\+.*?!-")
# Fortran routines lines are beginning by an alphabetic character after spaces
# and should not contain comments :
FORTRAN_ROUTINE = re.compile(r"[ \t]*[a-z]+[^!]*")
# Finds both !+ and !- tags for deletion:
ROUTINE_START_AND_END = re.compile(r"(?m)^[ \t]*(!\+|!-)")
# Finds the ! introducing Fortran comments:
UNCOMMENT = re.compile(r"[ \t]*!")
# Finds the first line of a table:
TABLE_1ST_LINE = re.compile(r"[ \t]*!.+\|.+\|.+\|")
# Finds the first line of a Fortran routine or a type declaration:
ROUTINE_NAME = re.compile(r".*(subroutine|function|type, bind\(c\) ::)([^(]*)\(")

print("\033[1m Scanning and parsing HL files... \033[0m")

for fortran_file in FILES_LIST:
    print(INPUT_DIR+fortran_file)

    whole_file = open(INPUT_DIR+fortran_file, 'r', errors='replace',
                      encoding='utf-8').read()

    #***************************************
    # Fortran module header
    #***************************************
    headers = SECTION_HEADER.search(whole_file).group()
    # Fortran module declaration:
    header1 = FORTRAN_BLOCK.search(headers).group()
    # Fortran code block in markdown:
    header2 = headers.replace(header1, "\n```fortran\n"+header1+"\n```\n\n")
    header3 = HEADER_START_AND_END.sub("", header2)
    # The first line will be the title:
    header4 = UNCOMMENT.sub("##", header3, 1)
    # Uncomment also the other lines:
    header5 = UNCOMMENT.sub("", header4)

    write_in_adequate_file(fortran_file, header5)

    #***************************************
    # Fortran routines
    #***************************************
    routines = SECTION_ROUTINE.findall(whole_file)

    for routine in routines:
        routine1 = FORTRAN_ROUTINE.search(routine).group()

        try_to_find_name = ROUTINE_NAME.search(routine1)
        if try_to_find_name is not None:
            name = try_to_find_name.group(2)
            # The title is the name of the routine, then comes the code:
            routine2 =  routine.replace(routine1, "\n### "+name+
                            "\n\n```fortran\n"+routine1.rstrip()+"\n```\n\n")
            routine3 = ROUTINE_START_AND_END.sub("", routine2)
        else:
            # This it not a routine but a type declaration
            routine2 = ROUTINE_START_AND_END.sub("", routine)
            # Adds a markdown section tag and removes the character zero "\n" of routine2:
            routine3 = "\n### " + routine2[1:] + "\n"

        # Is there a dummy arguments table to transform in markdown?
        try_table_first_line = TABLE_1ST_LINE.search(routine3)
        if try_table_first_line is not None:
            table_first_line = try_table_first_line.group()
            routine4 = routine3.replace(table_first_line,
                                        "\nArgument | Type | Required? | Description\n"+
                                        "---------|------|-----------|------------\n"+
                                        table_first_line)
        else:
            routine4 = routine3

        # Uncomment and write in the markdown file:
        routine5 = UNCOMMENT.sub("", routine4)
        write_in_adequate_file(fortran_file, routine5)

# Closing all files:
hl_api_file.close()
hl_drawing_api_file.close()
sup_routines_file.close()

print("\n \033[1m The .md files have been written in the "+OUTPUT_DIR+" directory \033[0m")
