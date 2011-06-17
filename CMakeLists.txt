# Copyright (C) 2011
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
# Contributed by Kyle Horne: 05.11.2011 (Gtk3 conversion James Tappin)

# CMAKE build file for GtkFortran

cmake_minimum_required(VERSION 2.6)
project(gtk-fortran C Fortran)

# Uncomment this for a debug build
# SET(CMAKE_BUILD_TYPE debug)

set(CMAKE_Fortran_FLAGS_DEBUG 
  "-g -pthread -Wall -Wtabs -fcheck-array-temporaries -fbacktrace -fbounds-check -ffpe-trap=invalid,zero,overflow -Wno-unused-dummy-argument") 
set(CMAKE_Fortran_FLAGS_RELEASE
"-pthread -O3 -mtune=native -march=native")

set(CPACK_PACKAGE_VERSION "0.1.0")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "gtk-fortran")
set(CPACK_GENERATOR "STGZ;TGZ")
set(CPACK_SOURCE_GENERATOR "STGZ;TGZ")
include(CPack)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

# uninstall target
configure_file(
	"${PROJECT_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
	"${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
	IMMEDIATE @ONLY)
add_custom_target(uninstall COMMAND ${CMAKE_COMMAND} -P
${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)

enable_testing()

find_package(GTK3 COMPONENTS gtk)
set(GTK_INCLUDES
	${GTK3_ATK_INCLUDE_DIR}
	${GTK3_CAIRO_INCLUDE_DIR}
	${GTK3_GDK_INCLUDE_DIR}
	${GTK3_GDK_PIXBUF_INCLUDE_DIR}
	${GTK3_GLIB_INCLUDE_DIR}
	${GTK3_GOBJECT_INCLUDE_DIR}
	${GTK3_GTK_INCLUDE_DIR}
	${GTK3_PANGO_INCLUDE_DIR} CACHE STRING "" FORCE)
set(GTK_LIBRARIES
	${GTK3_ATK_LIBRARY}
	${GTK3_CAIRO_LIBRARY}
	${GTK3_GDK_LIBRARY}
	${GTK3_GDK_PIXBUF_LIBRARY}
	${GTK3_GLIB_LIBRARY}
	${GTK3_GOBJECT_LIBRARY}
	${GTK3_GTK_LIBRARY}
	${GTK3_PANGO_LIBRARY} CACHE STRING "" FORCE)

configure_file("${PROJECT_SOURCE_DIR}/Doxyfile"
	"${CMAKE_CURRENT_BINARY_DIR}/Doxyfile" IMMEDIATE)

find_package(Doxygen)
set_property(DIRECTORY APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES
"latex" "html") 
add_custom_target(doxygen
	COMMAND ${DOXYGEN_EXECUTABLE}
"${CMAKE_CURRENT_BINARY_DIR}/Doxyfile" COMMENT "Writing
documentation..." WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
add_custom_target(doc)
add_dependencies(doc doxygen)

add_subdirectory(src)
add_subdirectory(examples)
add_subdirectory(sketcher)