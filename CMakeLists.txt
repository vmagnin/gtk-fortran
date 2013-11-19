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
# Contributed by Kyle Horne: 05.11.2011
# Last modifications: James Tappin 8/17/2012.

# CMAKE build file for GtkFortran

cmake_minimum_required(VERSION 2.8.5)
project(gtk-fortran C Fortran)

# CMAKE versions up to and including 2.8.10 fail to add the GIO libs for GTK2
if (CMAKE_VERSION VERSION_LESS 2.8.11)
  message(STATUS "CMAKE Version before 2.8.11 -- GIO example will not be built")
endif()

include(CheckCSourceCompiles)

# Note that we override the lib directory here because most
# 64-bit systems don't actually work if we use the default.
include(GNUInstallDirs)
set(CMAKE_INSTALL_LIBDIR lib)

# Uncomment this to debug or use "cmake -D CMAKE_BUILD_TYPE=debug .."
# set(CMAKE_BUILD_TYPE debug)
if (NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE release)
endif()
message(STATUS "Build type is: ${CMAKE_BUILD_TYPE}")

set(CMAKE_Fortran_FLAGS_DEBUG
"-g -pthread -Wall -Wtabs -fcheck-array-temporaries -fbacktrace -fbounds-check -ffpe-trap=invalid,zero,overflow -Wno-unused-dummy-argument")
# Not all of the above are understood by g95, the following is a minimal 
# setting.
# "-g -pthread  -fbounds-check")
set(CMAKE_Fortran_FLAGS_RELEASE
"-pthread -O3 -mtune=native -march=native")

set(CPACK_PACKAGE_VERSION "0.1.0")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "gtk2-fortran")
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

find_package(GTK2 COMPONENTS gtk)
set(GTK_INCLUDES
	${GTK2_ATK_INCLUDE_DIR}
	${GTK2_CAIRO_INCLUDE_DIR}
	${GTK2_GDK_INCLUDE_DIR}
	${GTK2_GDK_PIXBUF_INCLUDE_DIR}
	${GTK2_GLIB_INCLUDE_DIR}
	${GTK2_GOBJECT_INCLUDE_DIR}
	${GTK2_GTK_INCLUDE_DIR}
	${GTK2_PANGO_INCLUDE_DIR} CACHE STRING "" FORCE)
set(GTK_LIBRARIES
	${GTK2_ATK_LIBRARY}
	${GTK2_CAIRO_LIBRARY}
	${GTK2_GDK_LIBRARY}
	${GTK2_GDK_PIXBUF_LIBRARY}
	${GTK2_GLIB_LIBRARY}
	${GTK2_GIO_LIBRARY}
	${GTK2_GOBJECT_LIBRARY}
	${GTK2_GTK_LIBRARY}
	${GTK2_PANGO_LIBRARY} CACHE STRING "" FORCE)

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

# 
if (NOT EXCLUDE_PLPLOT)
  find_package(PlplotF95 QUIET)
  set(HAVE_LIBPLPLOTF95D ${PLPLOT_FOUND})
  if(PLPLOT_FOUND)
    set(LIBRARIES ${LIBRARIES} ${PLPLOT_LIBRARIES})
    include_directories(${PLPLOT_INCLUDE_DIR})
    include_directories(${PLPLOT_MODULE_DIR})
    set(CMAKE_REQUIRED_LIBRARIES "${PLPLOT_LIBRARIES}")
    set(CMAKE_REQUIRED_INCLUDES "${PLPLOT_INCLUDE_DIR}")
    check_c_source_compiles("
#include <plplot/plplot.h> 
int main(int argc, void *argv[])
{
  PLFLT w = 0.5;
  plinit();
  plwidth(w);
}" NEW_PLPLOT)
  else(PLPLOT_FOUND)
    message(STATUS "PLPLOT not found: PLPLOT integration and examples will not be built")
  endif(PLPLOT_FOUND)
else(NOT EXCLUDE_PLPLOT)
  message(STATUS "PLPLOT Excluded as command option")
endif(NOT EXCLUDE_PLPLOT)

add_subdirectory(src)
add_subdirectory(test)
add_subdirectory(examples)
if(PLPLOT_FOUND AND NOT EXCLUDE_PLPLOT)
  add_subdirectory(plplot)
endif(PLPLOT_FOUND AND NOT EXCLUDE_PLPLOT)
