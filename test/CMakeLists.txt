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
# Contributed by James Tappin: 8/7/2012
# Last modifications: Jens Hunger: 03/06/2013, ArmstrongJ: 07/03/2016

add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/run_all
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/run_all.pl ${CMAKE_CURRENT_BINARY_DIR}/run_all
)

add_custom_target(tester ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/run_all)
