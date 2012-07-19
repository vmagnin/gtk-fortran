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
# Last modifications: James Tappin: 07.19.2012

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
	"gtk-hl-misc.f90"
	"gtk-draw-hl.f90"
	"gtk-sup.f90"
	"gdk-pixbuf-hl.f90"
	"pango-auto.f90"
	"gdkevents-auto2.f90")

include_directories("/usr/include")
include_directories(${GTK_INCLUDES})
include_directories("/usr/local/include")
include_directories("/usr/local/include/gtk-2-fortran")

add_library(gtk-fortran_static STATIC ${sources})
add_library(gtk-fortran_shared SHARED ${sources})
target_link_libraries(gtk-fortran_shared ${GTK_LIBRARIES})

set_target_properties(gtk-fortran_static gtk-fortran_shared PROPERTIES
OUTPUT_NAME gtk-2-fortran)
set_target_properties(gtk-fortran_static
gtk-fortran_shared PROPERTIES CLEAN_DIRECT_OUTPUT 1)

set_target_properties(gtk-fortran_static gtk-fortran_shared PROPERTIES
VERSION "0.1")
set_target_properties(gtk-fortran_static
gtk-fortran_shared PROPERTIES VERSION "0.1")
set_target_properties(gtk-fortran_static gtk-fortran_shared PROPERTIES
SOVERSION "0.1")

# Does pkg-config have an equivalent in Windows? If so, then please
# add a suitable generator and installation.
if (UNIX)
  add_custom_command(
    OUTPUT gtk-2-fortran.pc
    COMMAND sed -e s+@prefix@+${CMAKE_INSTALL_PREFIX}+ -e s/@version@/${CPACK_PACKAGE_VERSION}/ ${CMAKE_CURRENT_SOURCE_DIR}/gtk-2-fortran.pc.in > ${CMAKE_CURRENT_BINARY_DIR}/gtk-2-fortran.pc
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/gtk-2-fortran.pc.in
    )
  add_custom_target(pkgconfig ALL DEPENDS "gtk-2-fortran.pc")
endif()

install(TARGETS gtk-fortran_static ARCHIVE DESTINATION lib)
install(TARGETS gtk-fortran_shared LIBRARY DESTINATION lib)
install(FILES
	"${CMAKE_CURRENT_BINARY_DIR}/atk.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/cairo.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gdk.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gdk_pixbuf.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/g.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_hl.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_hl_container.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_hl_button.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_hl_entry.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_hl_tree.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_hl_menu.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_hl_combobox.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_hl_spin_slider.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_hl_chooser.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_hl_dialog.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_hl_progress.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_hl_accelerator.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_hl_misc.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_draw_hl.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gdk_pixbuf_hl.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gtk_sup.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/pango.mod"
	"${CMAKE_CURRENT_BINARY_DIR}/gdk_events.mod"
	DESTINATION include/gtk-2-fortran)
if (UNIX)
  install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/gtk-2-fortran.pc"
    DESTINATION lib/pkgconfig
    )
endif()
