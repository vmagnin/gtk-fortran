# Copyright (C) 2011
# Free Software Foundation, Inc.
#
# This file is part of the gtk-fortran GTK Fortran Interface library.
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
# Contributed by @awvwgk (2022)
# Last modifications: vmagnin 2022-06-08
#===============================================================================

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  # gfortran compiler:
  set(
    CMAKE_Fortran_FLAGS_RELEASE_INIT
    "-pthread"
    "-O3"
    "-mtune=native"
    "-march=native"
  )
  set(
    CMAKE_Fortran_FLAGS_DEBUG_INIT
    "-g"
    "-pthread"
    "-Wall"
    "-Wextra"
    "-pedantic"
    "-std=f2008"
    "-Wtabs"
    "-fcheck=all"
    "-fbacktrace"
    "-Wno-unused-dummy-argument"
  )
  if(UNIX)
    set(CMAKE_EXE_LINKER_FLAGS_INIT "-rdynamic")
  endif()
elseif((CMAKE_Fortran_COMPILER_ID STREQUAL "Intel") OR (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM"))
  # ifort and ifx compilers (OneAPI):
  set(
    CMAKE_Fortran_FLAGS_RELEASE_INIT
    "-O3"
  )
  set(
    CMAKE_Fortran_FLAGS_DEBUG_INIT
    "-g"
    "-warn all"
    "-warn nounused"
  )
else()
  # Standard flags for all the other compilers:
  set(
    CMAKE_Fortran_FLAGS_RELEASE_INIT
    "-O3"
  )
  set(
    CMAKE_Fortran_FLAGS_DEBUG_INIT
    "-g"
  )
endif()

string(REPLACE ";" " " CMAKE_Fortran_FLAGS_RELEASE_INIT "${CMAKE_Fortran_FLAGS_RELEASE_INIT}")
string(REPLACE ";" " " CMAKE_Fortran_FLAGS_DEBUG_INIT "${CMAKE_Fortran_FLAGS_DEBUG_INIT}")
string(REPLACE ";" " " CMAKE_EXE_LINKER_FLAGS_INIT "${CMAKE_EXE_LINKER_FLAGS_INIT}")
