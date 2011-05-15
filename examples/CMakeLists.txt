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

# CMAKE build file for GtkFortran

include_directories("/usr/include")
include_directories("/usr/local/include")
include_directories("${CMAKE_BINARY_DIR}/src")
include_directories(${GTK_INCLUDES})

add_executable(bazaar "bazaar.f90")
target_link_libraries(bazaar gtk-fortran_static ${GTK_LIBRARIES})

add_executable(cairo-tests "cairo-tests.f90")
target_link_libraries(cairo-tests gtk-fortran_static ${GTK_LIBRARIES})

add_executable(gtkhello2 "gtkhello2.f90")
target_link_libraries(gtkhello2 gtk-fortran_static ${GTK_LIBRARIES})

add_executable(hl_choosers "hl_choosers.f90")
target_link_libraries(hl_choosers gtk-fortran_static ${GTK_LIBRARIES})

add_executable(hl_combo "hl_combo.f90")
target_link_libraries(hl_combo gtk-fortran_static ${GTK_LIBRARIES})

add_executable(hl_containers "hl_containers.f90")
target_link_libraries(hl_containers gtk-fortran_static ${GTK_LIBRARIES})

add_executable(hl_dialog "hl_dialog.f90")
target_link_libraries(hl_dialog gtk-fortran_static ${GTK_LIBRARIES})

add_executable(hl_list1 "hl_list1.f90")
target_link_libraries(hl_list1 gtk-fortran_static ${GTK_LIBRARIES})

add_executable(hl_list_n "hl_list_n.f90")
target_link_libraries(hl_list_n gtk-fortran_static ${GTK_LIBRARIES})

add_executable(hl_menu "hl_menu.f90")
target_link_libraries(hl_menu gtk-fortran_static ${GTK_LIBRARIES})

add_executable(hl_pbar "hl_pbar.f90")
target_link_libraries(hl_pbar gtk-fortran_static ${GTK_LIBRARIES})

add_executable(hl_pbar_p "hl_pbar_p.f90")
target_link_libraries(hl_pbar_p gtk-fortran_static ${GTK_LIBRARIES})

add_executable(hl_radio "hl_radio.f90")
target_link_libraries(hl_radio gtk-fortran_static ${GTK_LIBRARIES})

add_executable(hl_sliders "hl_sliders.f90")
target_link_libraries(hl_sliders gtk-fortran_static ${GTK_LIBRARIES})

add_executable(hl_textview "hl_textview.f90")
target_link_libraries(hl_textview gtk-fortran_static ${GTK_LIBRARIES})

add_executable(hl_tree "hl_tree.f90")
target_link_libraries(hl_tree gtk-fortran_static ${GTK_LIBRARIES})

#add_executable(julia_pixbuf "julia_pixbuf.f90")
#target_link_libraries(julia_pixbuf gtk-fortran_static ${GTK_LIBRARIES})

add_executable(list_demo "list_demo.f90")
target_link_libraries(list_demo gtk-fortran_static ${GTK_LIBRARIES})

add_executable(mandelbrot_pixbuf "mandelbrot_pixbuf.f90")
target_link_libraries(mandelbrot_pixbuf gtk-fortran_static
${GTK_LIBRARIES})

add_executable(menu "menu.f90")
target_link_libraries(menu gtk-fortran_static ${GTK_LIBRARIES})

add_executable(notebooks "notebooks.f90")
target_link_libraries(notebooks gtk-fortran_static ${GTK_LIBRARIES})

add_executable(tests "tests.f90")
target_link_libraries(tests gtk-fortran_static ${GTK_LIBRARIES})
