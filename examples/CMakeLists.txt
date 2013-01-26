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
# Last modifications, James Tappin: 01/21/2013

# CMAKE build file for GtkFortran

include_directories("/usr/include")
include_directories("/usr/local/include")
include_directories("${CMAKE_BINARY_DIR}/src")
include_directories(${GTK_INCLUDES})

if (NOT NO_BUILD_EXAMPLES) 
  add_executable(bazaar "bazaar.f90")
  target_link_libraries(bazaar gtk-fortran_static ${GTK_LIBRARIES})
  add_test(bazaar ./bazaar)

  add_executable(cairo-tests "cairo-tests.f90")
  target_link_libraries(cairo-tests gtk-fortran_static ${GTK_LIBRARIES})
  add_test(cairo-tests ./cairo-tests)

  add_executable(cairo-basics "cairo-basics.f90")
  target_link_libraries(cairo-basics gtk-fortran_static ${GTK_LIBRARIES})
  add_test(cairo-basics ./cairo-basics)

  add_executable(cairo-basics-click "cairo-basics-click.f90")
  target_link_libraries(cairo-basics-click gtk-fortran_static ${GTK_LIBRARIES})
  add_test(cairo-basics ./cairo-basics-click)

  add_executable(gtkhello2 "gtkhello2.f90")
  target_link_libraries(gtkhello2 gtk-fortran_static ${GTK_LIBRARIES})
  add_test(gtkhello2 ./gtkhello2)

  # This command is for Unix/Linux, please add a windows equivalent in an
  # Else section
  if (UNIX)
    add_custom_command(
      OUTPUT gtkbuilder.glade
      COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/gtkbuilder.glade ${CMAKE_CURRENT_BINARY_DIR}/gtkbuilder.glade
      DEPENDS  ${CMAKE_CURRENT_SOURCE_DIR}/gtkbuilder.glade
      )
  endif()

  add_custom_target(glade ALL DEPENDS "gtkbuilder.glade")

  add_executable(gtkbuilder "gtkbuilder.f90")
  target_link_libraries(gtkbuilder gtk-fortran_static ${GTK_LIBRARIES})
  # This test is excluded because the directories don't match up.
  #add_test(gtkbuilder gtkbuilder)

  add_executable(gtkbuilder2 "gtkbuilder2.f90" "gtkbuilder.glade")
  target_link_libraries(gtkbuilder2 gtk-fortran_static ${GTK_LIBRARIES})
  # This test is excluded because the directories don't match up.
  # add_test(gtkbuilder2 gtkbuilder2)

  add_executable(hl_assistant "hl_assistant.f90")
  target_link_libraries(hl_assistant gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_assistant ./hl_assistant)

  add_executable(hl_cairo1 "hl_cairo1.f90")
  target_link_libraries(hl_cairo1 gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_cairo1 ./hl_cairo1)

  add_executable(hl_cairo_viewer "hl_cairo_viewer.f90")
  target_link_libraries(hl_cairo_viewer gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_cairo_viewer ./hl_cairo_viewer)

  add_executable(hl_cairo_clock "hl_cairo_clock.f90")
  target_link_libraries(hl_cairo_clock gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_cairo_clock ./hl_cairo_clock)

  add_executable(hl_choosers "hl_choosers.f90")
  target_link_libraries(hl_choosers gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_choosers ./hl_choosers)

  add_executable(hl_combo "hl_combo.f90")
  target_link_libraries(hl_combo gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_combo ./hl_combo)

  add_executable(hl_containers "hl_containers.f90")
  target_link_libraries(hl_containers gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_containers ./hl_containers)

  add_executable(hl_dialog "hl_dialog.f90")
  target_link_libraries(hl_dialog gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_dialog ./hl_dialog)

  add_executable(hl_list1 "hl_list1.f90")
  target_link_libraries(hl_list1 gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_list1 ./hl_list1)

  add_executable(hl_list_n "hl_list_n.f90")
  target_link_libraries(hl_list_n gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_list_n ./hl_list_n)

  add_executable(hl_list_renderers "hl_list_renderers.f90")
  target_link_libraries(hl_list_renderers gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_list_renderers ./hl_list_renderers)

  add_executable(hl_menu "hl_menu.f90")
  target_link_libraries(hl_menu gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_menu ./hl_menu)

  add_executable(hl_pbar "hl_pbar.f90")
  target_link_libraries(hl_pbar gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_pbar ./hl_pbar)

  add_executable(hl_pbar_p "hl_pbar_p.f90")
  target_link_libraries(hl_pbar_p gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_pbar_p ./hl_pbar_p)

  add_executable(hl_radio "hl_radio.f90")
  target_link_libraries(hl_radio gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_radio ./hl_radio)

  add_executable(hl_sliders "hl_sliders.f90")
  target_link_libraries(hl_sliders gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_sliders ./hl_sliders)

  add_executable(hl_sliders2 "hl_sliders2.f90")
  target_link_libraries(hl_sliders2 gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_sliders2 ./hl_sliders2)

  add_executable(hl_textview "hl_textview.f90")
  target_link_libraries(hl_textview gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_textview ./hl_textview)

  add_executable(hl_tree "hl_tree.f90")
  target_link_libraries(hl_tree gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_tree ./hl_tree)

  add_executable(hl_infobar "hl_infobar.f90")
  target_link_libraries(hl_infobar gtk-fortran_static ${GTK_LIBRARIES})
  add_test(hl_infobar ./hl_infobar)

  if (GTK2_MINOR_VERSION GREATER 23)
    add_executable(julia_pixbuf "julia_pixbuf.f90")
    target_link_libraries(julia_pixbuf gtk-fortran_static ${GTK_LIBRARIES})
    add_test(julia_pixbuf ./julia_pixbuf)
  endif()

  add_executable(list_demo "list_demo.f90")
  target_link_libraries(list_demo gtk-fortran_static ${GTK_LIBRARIES})
  add_test(list_demo ./list_demo)

  add_executable(mandelbrot_pixbuf "mandelbrot_pixbuf.f90")
  target_link_libraries(mandelbrot_pixbuf gtk-fortran_static
    ${GTK_LIBRARIES})
  add_test(mandelbrot_pixbuf ./mandelbrot_pixbuf)

  add_executable(mandelbrot_pixbuf_zoom "mandelbrot_pixbuf_zoom.f90")
  target_link_libraries(mandelbrot_pixbuf_zoom gtk-fortran_static
    ${GTK_LIBRARIES})
  add_test(mandelbrot_pixbuf_zoom ./mandelbrot_pixbuf_zoom)

  add_executable(mandelbrot "mandelbrot.f90")
  target_link_libraries(mandelbrot gtk-fortran_static
    ${GTK_LIBRARIES})
  add_test(mandelbrot ./mandelbrot)

  add_executable(menu "menu.f90")
  target_link_libraries(menu gtk-fortran_static ${GTK_LIBRARIES})
  add_test(menu ./menu)

  add_executable(notebooks "notebooks.f90")
  target_link_libraries(notebooks gtk-fortran_static ${GTK_LIBRARIES})
  add_test(notebooks ./notebooks)

  add_executable(tests "tests.f90")
  target_link_libraries(tests gtk-fortran_static ${GTK_LIBRARIES})
  add_test(tests ./tests)
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
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_cairo1.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_cairo_clock.f90"
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
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_infobar.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/julia_pixbuf.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/list_demo.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/mandelbrot_pixbuf.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/mandelbrot_pixbuf_zoom.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/mandelbrot.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/menu.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/notebooks.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/tests.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/README"
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/gtk-fortran/examples2)
endif(INSTALL_EXAMPLES)
