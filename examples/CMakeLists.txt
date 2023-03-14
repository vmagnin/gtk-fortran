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
# Contributed by Kyle Horne: 05.11.2011
# Tests redesigned, James Tappin 05/17/2011
# Last modifications: Jens Hunger: 03/06/2013, Vincent Magnin: 2022-09-30
#
# CMAKE build file for gtk-fortran

include_directories("${CMAKE_BINARY_DIR}/src/modules")

if (NOT NO_BUILD_EXAMPLES)
  #==========================
  # Build the main examples:
  #==========================
  set(examples_list gtkzero_gapp gtkhello list_demo gio_demo tests tests_gtk_sup
    notebooks julia_pixbuf  mandelbrot_pixbuf mandelbrot_pixbuf_zoom
    menubar cairo-tests cairo-basics cairo-basics-click bazaar
    pixbuf_without_gui regex
  )

  if(NOT NO_BUILD_HL)
    set(examples_list ${examples_list}
        hl_assistant hl_choosers hl_combo hl_containers hl_dialog hl_list1
        hl_list_n hl_list_renderers hl_pbar
        hl_sliders hl_sliders2 hl_textview hl_tree hl_cairo1 hl_cairo_clock
        hl_cairo_viewer hl_infobar
    )
  endif(NOT NO_BUILD_HL)

  foreach(example ${examples_list})
    add_executable(${example} "${example}.f90")
    target_link_libraries(${example} gtk-fortran_static ${GTK_LIBRARIES})
    set_target_properties(${example} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${example}_mod/ )
    add_test(${example} ./${example})
  endforeach(example)

  #====================================
  # Build the gtkbuilder example:
  #====================================
  # With this command, CMake will detect the updates of gtkbuilder.ui:
  configure_file(gtkbuilder.ui gtkbuilder.ui COPYONLY)

  add_executable(gtkbuilder2 "gtkbuilder2.f90")
  target_link_libraries(gtkbuilder2 gtk-fortran_static ${GTK_LIBRARIES})
  set_target_properties(gtkbuilder2 PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/gtkbuilder2_mod/ )
  # This test is excluded because the directories don't match up.
  # add_test(gtkbuilder2 gtkbuilder2)
endif(NOT NO_BUILD_EXAMPLES)

# If the INSTALL_EXAMPLES variable is set, then copy the examples
# to ${CMAKE_INSTALL_DATAROOTDIR/gtk-fortran/examples<gtkversion>
# useful (e.g.) for building binary packages.
if (INSTALL_EXAMPLES)
  install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/gtk-fortran/examples${GTKv}
    PATTERN "CMakeLists.txt" EXCLUDE
    REGEX "\.backup$" EXCLUDE
    REGEX "~$" EXCLUDE
    REGEX "/Testing" EXCLUDE
  )
endif(INSTALL_EXAMPLES)
