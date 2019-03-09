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
# Contributed by Kyle Horne: 05.11.2011
# Last modifications: Jens Hunger: 03/06/2013, ArmstrongJ: 07/04/2016,
#   vmagnin: 07/17/2016

# CMAKE build file for GtkFortran

set(sources
    "atk-auto.f90"
    "cairo-auto.f90"
    "gdk-auto.f90"
    "gdk-pixbuf-auto.f90"
    "glib-auto.f90"
    "gtk.f90"
    "gtk-hl.f90"
    "gtk-hl-container.f90"
    "gtk-hl-button.f90"
    "gtk-hl-entry.f90"
    "gtk-hl-tree.f90"
    "gtk-hl-menu.f90"
    "gtk-hl-combobox.f90"
    "gtk-hl-spin-slider.f90"
    "gtk-hl-chooser.f90"
    "gtk-hl-dialog.f90"
    "gtk-hl-progress.f90"
    "gtk-hl-accelerator.f90"
    "gtk-hl-infobar.f90"
    "gtk-hl-assistant.f90"
    "gtk-hl-misc.f90"
    "gtk-draw-hl.f90"
    "gtk-sup.f90"
    "gdk-pixbuf-hl.f90"
    "pango-auto.f90"
    "gdkevents-auto${GTKv}.f90"
)

if (CMAKE_HOST_WIN32)
  set(sources
    ${sources}
    "mswindowsonly-auto.f90")
else()
  set(sources
    ${sources}
    "unixonly-auto.f90")
endif()

#include_directories("/usr/include")
include_directories(${GTK_INCLUDES})
#include_directories("/usr/local/include")

add_library(gtk-fortran_object OBJECT ${sources})
set_property(TARGET gtk-fortran_object PROPERTY POSITION_INDEPENDENT_CODE 1)

add_library(gtk-fortran_static STATIC $<TARGET_OBJECTS:gtk-fortran_object>)
add_library(gtk-fortran_shared SHARED $<TARGET_OBJECTS:gtk-fortran_object>)
target_link_libraries(gtk-fortran_shared ${GTK_LIBRARIES})

set_target_properties(gtk-fortran_static gtk-fortran_shared PROPERTIES
OUTPUT_NAME gtk-${GTKv}-fortran)
set_target_properties(gtk-fortran_static
gtk-fortran_shared PROPERTIES CLEAN_DIRECT_OUTPUT 1)

set_target_properties(gtk-fortran_static gtk-fortran_shared PROPERTIES
VERSION "0.1")
set_target_properties(gtk-fortran_static gtk-fortran_shared PROPERTIES
SOVERSION "0.1")

set_target_properties(gtk-fortran_object PROPERTIES
Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules/ )

# Does pkg-config have an equivalent in Windows? If so, then please
# add a suitable generator and installation.

# Copy gtk-${GTKv}-fortran.pc.in and replace strings @prefix@ and @version@ by their values:
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/gtk-${GTKv}-fortran.pc.in GTK${GTKv}PCIN)
string(REPLACE "@prefix@" "${CMAKE_INSTALL_PREFIX}" GTK${GTKv}PCOUT1 "${GTK${GTKv}PCIN}")
string(REPLACE "@version@" "${CPACK_PACKAGE_VERSION}" GTK${GTKv}PCOUT2 "${GTK${GTKv}PCOUT1}")
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-fortran.pc "${GTK${GTKv}PCOUT2}")
add_custom_target(pkgconfig ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-fortran.pc)

# The USE generator system. Copy usemodules.pl and replace strings @PREFIX@, @SHARE@ 
# and @GTK@ by their values:
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/usemodules.pl USERMODULE_IN)
string(REPLACE "@PREFIX@" "${CMAKE_INSTALL_PREFIX}" USERMODULE_OUT1 "${USERMODULE_IN}")
string(REPLACE "@SHARE@" "${CMAKE_INSTALL_DATAROOTDIR}" USERMODULE_OUT2 "${USERMODULE_OUT1}")
string(REPLACE "@GTK@" "${GTKv}" USERMODULE_OUT3 "${USERMODULE_OUT2}")
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-fortran-modscan "${USERMODULE_OUT3}")

# Copy those files:
add_custom_command(
    OUTPUT gtk-${GTKv}-fortran-index.csv
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/gtk-fortran-index.csv ${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-fortran-index.csv
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/gtk-fortran-index.csv
)
add_custom_command(
    OUTPUT gtk-${GTKv}-enumerators.lis
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/gtk-enumerators.lis ${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-enumerators.lis
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/gtk-enumerators.lis
)
add_custom_target(usemodules ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-fortran-modscan
  ${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-fortran-index.csv ${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-enumerators.lis
)

# Man page(s)
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/gtk-fortran-modscan.man MODSCAN_IN)
string(REPLACE "@GTK@" "${GTKv}" MODSCAN_OUT "${MODSCAN_IN}")
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-fortran-modscan.1 "${MODSCAN_OUT}")
add_custom_target(manpage ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-fortran-modscan.1)

# TODO: comment
if (CMAKE_HOST_WIN32)
  install(TARGETS gtk-fortran_static ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_LIBDIR})
  install(TARGETS gtk-fortran_shared LIBRARY DESTINATION ${CMAKE_INSTALL_BINDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
else()
  install(TARGETS gtk-fortran_static ARCHIVE DESTINATION 
    ${CMAKE_INSTALL_LIBDIR})
  install(TARGETS gtk-fortran_shared LIBRARY DESTINATION 
    ${CMAKE_INSTALL_LIBDIR})
endif()

install(DIRECTORY
  ${CMAKE_CURRENT_BINARY_DIR}/modules/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gtk-${GTKv}-fortran
)

install(PROGRAMS
  "${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-fortran-modscan"
  DESTINATION ${CMAKE_INSTALL_BINDIR}
  PERMISSIONS WORLD_EXECUTE
)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-fortran-index.csv"
  "${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-enumerators.lis"
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/gtk-fortran
)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-fortran.pc"
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-fortran-modscan.1"
  DESTINATION ${CMAKE_INSTALL_MANDIR}/man1
)
