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
#=============================================================================
# Contributed by James Tappin 05/20/2011
# Last modifications: Jens Hunger: 03/07/2013, vmagnin 2019-04-05
#
# CMAKE build file for gtkf-sketcher

include_directories("${CMAKE_BINARY_DIR}/src/modules")

add_executable(gtkf-sketcher "gtkf-sketcher.F90")
target_link_libraries(gtkf-sketcher gtk-fortran_static ${GTK_LIBRARIES})
# No test as the glade stuff is too sensitive to location

#==========================================
# Copy other files needed by gtkf-sketcher:
#==========================================
file(COPY gtkf-sketcher.glade DESTINATION ${CMAKE_CURRENT_BINARY_DIR} )
file(COPY example.glade DESTINATION ${CMAKE_CURRENT_BINARY_DIR} )
file(COPY default.options DESTINATION ${CMAKE_CURRENT_BINARY_DIR} )
file(COPY data DESTINATION ${CMAKE_CURRENT_BINARY_DIR} )
file(COPY ${CMAKE_SOURCE_DIR}/src/usemodules.py DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/example/ )
file(COPY ${CMAKE_SOURCE_DIR}/src/tools.py DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/example/ )
file(COPY ${CMAKE_SOURCE_DIR}/src/gtk-fortran-index.csv DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/example/ )
