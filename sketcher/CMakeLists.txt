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
#=============================================================================
# Contributed by James Tappin 05/20/2011
# Last modifications: Jens Hunger: 03/07/2013, vmagnin 2022-09-30
#
# CMAKE build file for gtkf-sketcher

include_directories("${CMAKE_BINARY_DIR}/src/modules")

# configure_file() copies and renames a file and substitutes @VAR@ or ${VAR}
# https://cmake.org/cmake/help/latest/command/configure_file.html
set(GTKF_PROG_PREFIX "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}/${gtk_V_fortran}")
configure_file(gtkf-sketcher.f90 gtkf-sketcher.out.f90)

add_executable(gtkf-sketcher "gtkf-sketcher.out.f90")
target_link_libraries(gtkf-sketcher gtk-fortran_static ${GTK_LIBRARIES})
# No test as the glade stuff is too sensitive to location

#==========================================
# Copy other files needed by gtkf-sketcher
#==========================================
configure_file(gtkf-sketcher.glade gtkf-sketcher.glade COPYONLY)
configure_file(example.glade example.glade COPYONLY)
configure_file(default.options default.options COPYONLY)
# Copy the directory 'data' (configure_file() works only with files):
file(COPY data DESTINATION ${CMAKE_CURRENT_BINARY_DIR} )

configure_file(${CMAKE_SOURCE_DIR}/src/usemodules.py ${CMAKE_CURRENT_BINARY_DIR}/example/usemodules.py COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/src/tools.py ${CMAKE_CURRENT_BINARY_DIR}/example/tools.py COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/src/gtk-fortran-index.csv ${CMAKE_CURRENT_BINARY_DIR}/example/gtk-fortran-index.csv COPYONLY)
