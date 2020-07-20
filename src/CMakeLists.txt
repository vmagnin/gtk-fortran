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
# Contributed by Kyle Horne: 05.11.2011
# Last modifications: Jens Hunger: 03/06/2013, ArmstrongJ: 07/04/2016,
#   vmagnin: 2020-06-18
#
# CMAKE build file for GtkFortran

#=================================================
# Fortran files composing the gtk-fortran library:
#=================================================
set(sources
    "atk-auto.f90"
    "cairo-auto.f90"
    "gdk-auto.f90"
    "gdk-pixbuf-auto.f90"
    "glib-auto.f90"
    "gtk.f90"
    "gtk-sup.f90"
    "pango-auto.f90"
    "gdkevents-auto.f90"
)

if(NOT NO_BUILD_HL)
    set(sources ${sources} 
        "gtk-hl.f90"
        "gtk-hl-container.f90"
        "gtk-hl-button.f90"
        "gtk-hl-entry.f90"
        "gtk-hl-tree.f90"
        "gtk-hl-combobox.f90"
        "gtk-hl-spin-slider.f90"
        "gtk-hl-chooser.f90"
        "gtk-hl-dialog.f90"
        "gtk-hl-progress.f90"
        "gtk-hl-infobar.f90"
        "gtk-hl-assistant.f90"
        "gtk-hl-misc.f90"
        "gtk-draw-hl.f90"
        "gdk-pixbuf-hl.f90"
    )
endif(NOT NO_BUILD_HL)

#==================================
# Adding OS specific Fortran files:
#==================================
if (CMAKE_HOST_WIN32)
  set(sources ${sources} "mswindowsonly-auto.f90")
else()
  set(sources ${sources} "unixonly-auto.f90")
endif()

#======================================================
# Defining the static and shared gtk-fortran libraries:
#======================================================
add_library(gtk-fortran_object OBJECT ${sources})
# To build position independent shared libraries:
set_property(TARGET gtk-fortran_object PROPERTY POSITION_INDEPENDENT_CODE TRUE)

add_library(gtk-fortran_static STATIC $<TARGET_OBJECTS:gtk-fortran_object>)
add_library(gtk-fortran_shared SHARED $<TARGET_OBJECTS:gtk-fortran_object>)
target_link_libraries(gtk-fortran_shared ${GTK_LIBRARIES})

set_target_properties(gtk-fortran_static gtk-fortran_shared PROPERTIES 
  OUTPUT_NAME ${gtk_V_fortran}
  VERSION "0.1"
  SOVERSION "0.1")

set_target_properties(gtk-fortran_object PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules/ )

#==========================
# Copy some specific files:
#==========================
# Copy gtk-fortran.pc.in and replace strings @.*@ by their values:
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/gtk-fortran.pc.in GTK${GTKv}PCIN)
string(REPLACE "@gtkfortranname@" "${gtk_V_fortran}" GTK${GTKv}PCOUT1 "${GTK${GTKv}PCIN}")
string(REPLACE "@prefix@" "${CMAKE_INSTALL_PREFIX}" GTK${GTKv}PCOUT2 "${GTK${GTKv}PCOUT1}")
string(REPLACE "@CMAKE_INSTALL_LIBDIR@" "${CMAKE_INSTALL_LIBDIR}" GTK${GTKv}PCOUT3 "${GTK${GTKv}PCOUT2}")
string(REPLACE "@CMAKE_INSTALL_INCLUDEDIR@" "${CMAKE_INSTALL_INCLUDEDIR}" GTK${GTKv}PCOUT4 "${GTK${GTKv}PCOUT3}")
string(REPLACE "@version@" "${CPACK_PACKAGE_VERSION}" GTK${GTKv}PCOUT5 "${GTK${GTKv}PCOUT4}")
string(REPLACE "@gtkname@" "${GTKname}" GTK${GTKv}PCOUT6 "${GTK${GTKv}PCOUT5}")
string(REPLACE "@linkerflags@" "${CMAKE_EXE_LINKER_FLAGS}" GTK${GTKv}PCOUT7 "${GTK${GTKv}PCOUT6}")
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/${gtk_V_fortran}.pc "${GTK${GTKv}PCOUT7}")
add_custom_target(pkgconfig ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${gtk_V_fortran}.pc)

# The USE generator system. Copy usemodules.pl and replace strings @PREFIX@, @SHARE@ 
# and @GTK@ by their values:
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/usemodules.pl USERMODULE_IN)
string(REPLACE "@PREFIX@" "${CMAKE_INSTALL_PREFIX}" USERMODULE_OUT1 "${USERMODULE_IN}")
string(REPLACE "@SHARE@" "${CMAKE_INSTALL_DATAROOTDIR}" USERMODULE_OUT2 "${USERMODULE_OUT1}")
string(REPLACE "@GTK@" "${GTKv}" USERMODULE_OUT3 "${USERMODULE_OUT2}")
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/${gtk_V_fortran}-modscan "${USERMODULE_OUT3}")

# Add the install path to usemodules.py as one of the places it will search for the gtk-fortran csv. 
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/usemodules.py PYUSERMODULE_IN)
string(REPLACE "@PATH_TO_CSV@" "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATAROOTDIR}/gtk-fortran/${gtk_V_fortran}-index.csv" PYUSERMODULE_OUT "${PYUSERMODULE_IN}")
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/${gtk_V_fortran}-pymodscan "${PYUSERMODULE_OUT}")

# Copy those files:
file(COPY gtk-fortran-index.csv DESTINATION ${CMAKE_CURRENT_BINARY_DIR} )
file(RENAME ${CMAKE_CURRENT_BINARY_DIR}/gtk-fortran-index.csv ${CMAKE_CURRENT_BINARY_DIR}/${gtk_V_fortran}-index.csv )
file(COPY gtk-enumerators.lis DESTINATION ${CMAKE_CURRENT_BINARY_DIR} )
file(RENAME ${CMAKE_CURRENT_BINARY_DIR}/gtk-enumerators.lis ${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-enumerators.lis )
add_custom_target(usemodules ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${gtk_V_fortran}-modscan
  ${CMAKE_CURRENT_BINARY_DIR}/${gtk_V_fortran}-index.csv ${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-enumerators.lis )

# Man page(s)
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/gtk-fortran-modscan.man MODSCAN_IN)
string(REPLACE "@GTK@" "${GTKv}" MODSCAN_OUT "${MODSCAN_IN}")
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/${gtk_V_fortran}-modscan.1 "${MODSCAN_OUT}")
add_custom_target(manpage ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${gtk_V_fortran}-modscan.1)

#==================
# Files to install:
#==================
install(TARGETS gtk-fortran_static gtk-fortran_shared
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

install(DIRECTORY
  ${CMAKE_CURRENT_BINARY_DIR}/modules/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${gtk_V_fortran}
)

install(PROGRAMS
  "${CMAKE_CURRENT_BINARY_DIR}/${gtk_V_fortran}-modscan"
  DESTINATION ${CMAKE_INSTALL_BINDIR}
  PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install(PROGRAMS
  "${CMAKE_CURRENT_BINARY_DIR}/${gtk_V_fortran}-pymodscan"
  DESTINATION ${CMAKE_INSTALL_BINDIR}
  PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${gtk_V_fortran}-index.csv"
  "${CMAKE_CURRENT_BINARY_DIR}/gtk-${GTKv}-enumerators.lis"
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/gtk-fortran
)

install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${gtk_V_fortran}.pc"
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
)

install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${gtk_V_fortran}-modscan.1"
  DESTINATION ${CMAKE_INSTALL_MANDIR}/man1
)
