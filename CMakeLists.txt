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
#===============================================================================
# Contributed by Kyle Horne: 05.11.2011
# Last modifications: James Tappin 8/17/2012, Jens Hunger 01/07/2018, 
# vmagnin 2020-02-07
#
# CMAKE build file for gtk-fortran
# Options:
# -D CMAKE_BUILD_TYPE=debug
# -D EXCLUDE_PLPLOT=true
# -D NO_BUILD_EXAMPLES=true
# -D NO_BUILD_HL=true
# -D INSTALL_EXAMPLES=true

cmake_minimum_required(VERSION 3.4)

project(gtk-fortran Fortran)

set(CMAKE_PROJECT_DESCRIPTION "A partial GTK / Fortran 2003 binding")
set(CMAKE_PROJECT_HOMEPAGE_URL "https://github.com/vmagnin/gtk-fortran/")

#===============================================================================
# Major version of GTK for the current branch:
#===============================================================================
set(GTKv 3)
set(GTKname "gtk+-3.0")
set(CMAKE_PROJECT_VERSION_MAJOR ${GTKv})
set(CMAKE_PROJECT_VERSION_MINOR 0)
set(CMAKE_PROJECT_VERSION_PATCH 0)

#===============================================================================
# We override the default lib directory because on 64-bit systems, lib can also
# be "lib64" for example.
#===============================================================================
include(GNUInstallDirs)
set(CMAKE_INSTALL_LIBDIR lib)

#===============================================================================
# Default build type is release
# Uncomment this to debug or use "cmake -D CMAKE_BUILD_TYPE=debug .."
#===============================================================================
# set(CMAKE_BUILD_TYPE debug)
if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE release)
endif()

#===============================================================================
# Setting compilation flags for various compilers and build types:
#===============================================================================
if (${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
    # gfortran compiler:
    set(CMAKE_Fortran_FLAGS_DEBUG
        "-g -pthread -Wall -Wextra -pedantic -std=f2008 -Wtabs -fcheck=all -fbacktrace -Wno-unused-dummy-argument")
    set(CMAKE_Fortran_FLAGS_RELEASE "-pthread -O3 -mtune=native -march=native")
    if (UNIX)
        set(CMAKE_EXE_LINKER_FLAGS "-rdynamic")
    endif()
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    # ifort compiler:
    set(CMAKE_Fortran_FLAGS_DEBUG "-g -warn all")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -mtune=native -march=native")
# >>> You can add here other elseif() for other specific compilers...
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
set(CPACK_PACKAGE_CHECKSUM SHA256)
set(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/LICENSE")
set(CPACK_RESOURCE_FILE_README "${PROJECT_SOURCE_DIR}/README.md")
set(CPACK_GENERATOR "TGZ")
set(CPACK_SOURCE_GENERATOR "TGZ")
include(CPack)

#===============================================================================
# "Path for CMake modules to be loaded by the include() or find_package() 
# commands before checking the default modules that come with CMake"
#===============================================================================
set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

#===============================================================================
# Uninstall target:
# Generic code adapted from
# https://gitlab.kitware.com/cmake/community/-/wikis/FAQ#can-i-do-make-uninstall-with-cmake
#===============================================================================
if(NOT TARGET uninstall)
    configure_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
        "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
        IMMEDIATE @ONLY)

    add_custom_target(uninstall
        COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)
endif()

#===============================================================================
# Examples can be tested via make test:
#===============================================================================
enable_testing()

#===============================================================================
# Find all GTK libraries:
#===============================================================================
# Use PkgConfig:
find_package(PkgConfig REQUIRED)
pkg_check_modules(GTK REQUIRED ${GTKname})
# Setup CMake to use GTK, tell the compiler where to look for headers
# and to the linker where to look for libraries:
include_directories(${GTK_INCLUDE_DIRS})
link_directories(${GTK_LIBRARY_DIRS})
# Add other flags to the compiler:
add_definitions(${GTK_CFLAGS_OTHER})

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
if (NOT EXCLUDE_PLPLOT AND NOT NO_BUILD_HL)
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
else(NOT EXCLUDE_PLPLOT AND NOT NO_BUILD_HL)
    message(STATUS ">>> PLPLOT excluded as command option")
endif(NOT EXCLUDE_PLPLOT AND NOT NO_BUILD_HL)

#===============================================================================
# Add subdirectories to build:
#===============================================================================
add_subdirectory(src)
add_subdirectory(examples)

if(NOT NO_BUILD_HL)
    add_subdirectory(sketcher)

    if(PLPLOT_FOUND AND NOT EXCLUDE_PLPLOT)
        add_subdirectory(plplot)
    endif(PLPLOT_FOUND AND NOT EXCLUDE_PLPLOT)
else(NOT NO_BUILD_HL)
    message(STATUS ">>> High Level API excluded as command option")
endif(NOT NO_BUILD_HL)
