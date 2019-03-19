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
# Last modifications: James Tappin 8/17/2012, vmagnin 07/18/2016, 
# Jens Hunger 01/07/2018, vmagnin 2019-03-14 (PLplot>=5.13)
# CMAKE build file for gtk-fortran

cmake_minimum_required(VERSION 2.8.5)

project(gtk-fortran Fortran)

# Major version of GTK for the current branch:
#set(GTKv 2)
set(GTKv 3)

# CMAKE versions up to and including 2.8.10 fail to add the GIO libs for GTK 2
if (CMAKE_VERSION VERSION_LESS 2.8.11)
    message(STATUS "CMAKE Version before   # use PkgConfig
  FIND_PACKAGE(PkgConfig REQUIRED)
  PKG_CHECK_MODULES(GTK REQUIRED gtk+-3.0)
  # Setup CMake to use GTK+, tell the compiler where to look for headers
  # and to the linker where to look for libraries
  INCLUDE_DIRECTORIES(${GTK_INCLUDE_DIRS})
  LINK_DIRECTORIES(${GTK_LIBRARY_DIRS})
  # Add other flags to the compiler
  ADD_DEFINITIONS(${GTK_CFLAGS_OTHER})2.8.11 -- GIO example will not be built")
endif()

# Note that we override the lib directory here because most
# 64-bit systems don't actually work if we use the default.
include(GNUInstallDirs)
set(CMAKE_INSTALL_LIBDIR lib)

#===============================================================================
# Uncomment this to debug or use "cmake -D CMAKE_BUILD_TYPE=debug .."
# set(CMAKE_BUILD_TYPE debug)
if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE release)
endif()


#===============================================================================
# Setting compilation flags for various compilers and build types:
#===============================================================================
if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
    # gfortran compiler:
    # Removed -ffpe-trap=invalid,zero,overflow as it gives a lot of crashes deep
    # in the call stack in GTK 3
    set(CMAKE_Fortran_FLAGS_DEBUG
        "-g -pthread -Wall -Wextra -pedantic -std=f2008 -Wtabs -fcheck=all -fbacktrace -Wno-unused-dummy-argument")
    set(CMAKE_Fortran_FLAGS_RELEASE
        "-pthread -O3 -mtune=native -march=native")
    if (UNIX)
      set(CMAKE_EXE_LINKER_FLAGS
          "-rdynamic")
    endif()
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    # ifort compiler:
    set(CMAKE_Fortran_FLAGS_DEBUG
        "-g -warn all")
    set(CMAKE_Fortran_FLAGS_RELEASE
        "-O3 -mtune=native -march=native")
# You can add here other elseif() for other specific compilers...
else()
    # Standard flags for all the other compilers:
    set(CMAKE_Fortran_FLAGS_DEBUG "-g")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

# Print compiler CMake ID, version and path:
message(STATUS "Compiler: ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION} ${CMAKE_Fortran_COMPILER}")
message(STATUS "Build type is: ${CMAKE_BUILD_TYPE}")
# Print compilation flags :
if (${CMAKE_BUILD_TYPE} STREQUAL "release")
    message(STATUS "Compilation flags: ${CMAKE_Fortran_FLAGS_RELEASE}")
elseif(${CMAKE_BUILD_TYPE} STREQUAL "debug")
    message(STATUS "Compilation flags: ${CMAKE_Fortran_FLAGS_DEBUG}")
endif()

#===============================================================================
# Package generation:
#===============================================================================
set(CPACK_PACKAGE_VERSION "0.1.0")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "gtk${GTKv}-fortran")
set(CPACK_GENERATOR "STGZ;TGZ")
set(CPACK_SOURCE_GENERATOR "STGZ;TGZ")
include(CPack)


#===============================================================================
set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")
include(CheckFortranSourceCompiles)

#===============================================================================
# Uninstall target:
#===============================================================================
configure_file(
    "${PROJECT_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
    IMMEDIATE @ONLY)
add_custom_target(uninstall COMMAND ${CMAKE_COMMAND} -P
${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)

#===============================================================================
# Examples can be tested via make test:
#===============================================================================
enable_testing()

#===============================================================================
# GTK libraries:
#===============================================================================
if (WIN32)
    # use PkgConfig:
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(GTK REQUIRED gtk+-3.0)
    # Setup CMake to use GTK, tell the compiler where to look for headers
    # and to the linker where to look for libraries:
    include_directories(${GTK_INCLUDE_DIRS})
    link_directories(${GTK_LIBRARY_DIRS})
    # Add other flags to the compiler:
    add_definitions(${GTK_CFLAGS_OTHER})
elseif (UNIX)
    find_package(GTK${GTKv} COMPONENTS gtk)
    set(GTK_INCLUDES
        ${GTK${GTKv}_ATK_INCLUDE_DIR}
        ${GTK${GTKv}_CAIRO_INCLUDE_DIR}
        ${GTK${GTKv}_GDK_INCLUDE_DIR}
        ${GTK${GTKv}_GDK_PIXBUF_INCLUDE_DIR}
        ${GTK${GTKv}_GLIB_INCLUDE_DIR}
        ${GTK${GTKv}_GOBJECT_INCLUDE_DIR}
        ${GTK${GTKv}_GTK_INCLUDE_DIR}
        ${GTK${GTKv}_PANGO_INCLUDE_DIR} CACHE STRING "" FORCE)
    set(GTK_LIBRARIES
        ${GTK${GTKv}_ATK_LIBRARY}
        ${GTK${GTKv}_CAIRO_LIBRARY}
        ${GTK${GTKv}_GDK_LIBRARY}
        ${GTK${GTKv}_GDK_PIXBUF_LIBRARY}
        ${GTK${GTKv}_GLIB_LIBRARY}
        ${GTK${GTKv}_GIO_LIBRARY}
        ${GTK${GTKv}_GOBJECT_LIBRARY}
        ${GTK${GTKv}_GTK_LIBRARY}
        ${GTK${GTKv}_PANGO_LIBRARY} CACHE STRING "" FORCE)
endif()


#===============================================================================
# Doxygen documentation generation (optionnal):
#===============================================================================
find_package(Doxygen)
set(HAVE_DOXYGEN ${DOXYGEN_FOUND})
if(DOXYGEN_FOUND)
    configure_file("${PROJECT_SOURCE_DIR}/Doxyfile"
        "${CMAKE_CURRENT_BINARY_DIR}/Doxyfile" IMMEDIATE)
    set_property(DIRECTORY APPEND PROPERTY ADDITIONAL_MAKE_CLEAN_FILES "latex" "html") 
    add_custom_target(doxygen
        COMMAND ${DOXYGEN_EXECUTABLE}
        "${CMAKE_CURRENT_BINARY_DIR}/Doxyfile" 
        COMMENT "Writing documentation..." WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    )
    add_custom_target(doc)
    add_dependencies(doc doxygen)
endif(DOXYGEN_FOUND)


#===============================================================================
# PLplot integration (>=5.13 needed)
# ----------------------------------
# A lot of changes have occured in PLplot 5.11, 5.12 and 5.13: the libraries 
# have been renamed (plplot and plplot-fortran) and the ISO_C_BINDING has been 
# adopted.
# By now (March 2019), the find_package(plplot) command gives a lot of errors
# in Debian/Ubuntu distributions because of some renaming in Debian packages.
# As a temporary (?) workaround, we are now using pkg-config.
# If a CMake error occurs, add -D EXCLUDE_PLPLOT=true to your CMake command.
#===============================================================================
if (NOT EXCLUDE_PLPLOT)
    find_package(PkgConfig REQUIRED)

    pkg_check_modules(PLPLOT-FORTRAN REQUIRED plplot-fortran)
    pkg_check_modules(PLPLOT REQUIRED plplot)

    # Setup CMake to use PLplot, tell the compiler where to look for headers
    # and to the linker where to look for libraries:
    include_directories(${PLPLOT-FORTRAN_INCLUDE_DIRS})
    link_directories(${PLPLOT-FORTRAN_LIBRARY_DIRS})
    # Add other flags to the compiler:
    add_definitions(${PLPLOT-FORTRAN_CFLAGS_OTHER})

    set(LIBRARIES ${LIBRARIES} ${PLPLOT_LIBRARIES})
    include_directories(${PLPLOT_INCLUDE_DIRS})
    set(CMAKE_REQUIRED_LIBRARIES "${PLPLOT_LIBRARIES}")
    set(CMAKE_REQUIRED_INCLUDES "${PLPLOT-FORTRAN_INCLUDE_DIRS}")
else(NOT EXCLUDE_PLPLOT)
    message(STATUS "PLPLOT Excluded as command option")
endif(NOT EXCLUDE_PLPLOT)


#===============================================================================
# Add subdirectories to build:
#===============================================================================
add_subdirectory(src)
add_subdirectory(test)
add_subdirectory(examples)

if(${GTKv} GREATER 2)
    add_subdirectory(sketcher)
endif()

if(PLPLOT_FOUND AND NOT EXCLUDE_PLPLOT)
    add_subdirectory(plplot)
endif(PLPLOT_FOUND AND NOT EXCLUDE_PLPLOT)
