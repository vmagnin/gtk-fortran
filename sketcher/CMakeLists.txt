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
# Contributed by James Tappin 05/20/2011

# CMAKE build file for GtkFortran

include_directories("/usr/include")
include_directories("/usr/local/include")
include_directories("${CMAKE_BINARY_DIR}/src")
include_directories(${GTK_INCLUDES})

add_executable(gtkf-sketcher "gtkf-sketcher.f90")
target_link_libraries(gtkf-sketcher gtk-fortran_static ${GTK_LIBRARIES})
# No test as the glade stuff is too sensitive to location

# This command is for Unix/Linux, please add a windows equivalent in an
# Else section
if (UNIX)
   add_custom_command(
     OUTPUT gtkf-sketcher.glade
     COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/gtkf-sketcher.glade ${CMAKE_CURRENT_BINARY_DIR}/gtkf-sketcher.glade
     DEPENDS  ${CMAKE_CURRENT_SOURCE_DIR}/gtkf-sketcher.glade
   )
 add_custom_command(
   OUTPUT example.glade
   COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/example.glade ${CMAKE_CURRENT_BINARY_DIR}/example.glade
   DEPENDS  ${CMAKE_CURRENT_SOURCE_DIR}/example.glade
   )
 add_custom_command(
   OUTPUT data
   COMMAND mkdir ${CMAKE_CURRENT_BINARY_DIR}/data
   )
 add_custom_command(
   OUTPUT data/apache2.0.lic  data/gnu-gpl-v2.lic     data/gnu-lgpl-v2.lic
   data/bsd.lic        data/gnu-gpl-v3.lic     data/zlib.lic
   data/freeware.lic   data/gnu-lgpl-v2.1.lic
   COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/data/*.lic ${CMAKE_CURRENT_BINARY_DIR}/data 
   DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/data/*.lic
   )
endif()

add_custom_target(sketcher-glade ALL DEPENDS "gtkf-sketcher.glade")
add_custom_target(example-glade ALL DEPENDS "example.glade")
add_custom_target(licenses ALL DEPENDS data data/apache2.0.lic  data/gnu-gpl-v2.lic     data/gnu-lgpl-v2.lic
  data/bsd.lic        data/gnu-gpl-v3.lic     data/zlib.lic
  data/freeware.lic   data/gnu-lgpl-v2.1.lic)