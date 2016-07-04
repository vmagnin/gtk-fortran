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
# Tests redesigned, James Tappin 05/17/2011
# Last modifications: Jens Hunger: 03/06/2013, Vincent Magnin: 07/04/2016

# CMAKE build file for GtkFortran

#include_directories("/usr/include")
#include_directories("/usr/local/include")
include_directories("${CMAKE_BINARY_DIR}/src")
include_directories(${GTK_INCLUDES})

if (NOT NO_BUILD_EXAMPLES)
  # Build the two gtkbuilders examples:
  add_custom_command(
    OUTPUT gtkbuilder.glade
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/gtkbuilder.glade ${CMAKE_CURRENT_BINARY_DIR}/gtkbuilder.glade
    DEPENDS  ${CMAKE_CURRENT_SOURCE_DIR}/gtkbuilder.glade
  )
  add_custom_target(glade ALL DEPENDS "gtkbuilder.glade")

  add_executable(gtkbuilder "gtkbuilder.f90")
  target_link_libraries(gtkbuilder gtk-fortran_static ${GTK_LIBRARIES})
  # This test is excluded because the directories don't match up.
  #add_test(gtkbuilder gtkbuilder)
  add_executable(gtkbuilder2 "gtkbuilder2.f90" "gtkbuilder.glade")
  target_link_libraries(gtkbuilder2 gtk-fortran_static ${GTK_LIBRARIES})
  # This test is excluded because the directories don't match up.
  # add_test(gtkbuilder2 gtkbuilder2)

  # Build the other examples:
  foreach(example bazaar cairo-tests cairo-basics cairo-basics-click gtkhello2
          hl_assistant hl_choosers hl_combo hl_containers hl_dialog hl_list1
          hl_list_n hl_list_renderers hl_menu hl_pbar hl_pbar_p hl_radio
          hl_sliders hl_sliders2 hl_textview hl_tree hl_cairo1 hl_cairo_clock
          hl_cairo_viewer hl_infobar julia_pixbuf list_demo mandelbrot_pixbuf
          mandelbrot_pixbuf_zoom menu notebooks gio_demo tests)
    add_executable(${example} "${example}.f90")
    target_link_libraries(${example} gtk-fortran_static ${GTK_LIBRARIES})
    add_test(${example} ./${example})
  endforeach(example)
endif(NOT NO_BUILD_EXAMPLES)

# If the INSTALL_EXAMPLES variable is set, then copy the examples
# to ${CMAKE_INSTALL_DATAROOTDIR/gtk-fortran/examples<gtkversion>
# useful (e.g.) for building binary packages.
if (INSTALL_EXAMPLES)
  install(FILES
    "${CMAKE_CURRENT_SOURCE_DIR}/bazaar.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/cairo-tests.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/cairo-basics.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/cairo-basics-click.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/gtkhello2.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/gtkbuilder.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/gtkbuilder2.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/gtkbuilder.glade"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_assistant.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_choosers.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_combo.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_containers.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_dialog.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_list1.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_list_n.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_list_renderers.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_menu.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_pbar.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_pbar_p.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_radio.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_sliders.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_sliders2.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_textview.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_tree.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_cairo1.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_cairo_clock.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_infobar.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/julia_pixbuf.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/list_demo.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/mandelbrot_pixbuf.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/mandelbrot_pixbuf_zoom.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/menu.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/notebooks.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/gio_demo.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/tests.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/README"
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/gtk-fortran/examples3
  )
endif(INSTALL_EXAMPLES)
