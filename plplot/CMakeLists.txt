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
# Contributed by James Tappin, last updated: 1/21/2013

# set(sources
#   "plplot_extra.f90")

include_directories("/usr/include")
#include_directories(${PLPLOT_INCLUDES})
include_directories("${CMAKE_BINARY_DIR}/plplot")
include_directories("${CMAKE_BINARY_DIR}/src")
include_directories(${GTK_INCLUDES})
#include_directories(${PLPLOT_MODULE_DIR})

add_custom_command(
  OUTPUT plplot_extra.mod
  COMMAND ${CMAKE_Fortran_COMPILER} -c ${CMAKE_CURRENT_SOURCE_DIR}/plplot_extra.f90
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/plplot_extra.f90)
add_custom_target(plplot_extra_module ALL DEPENDS plplot_extra.mod)

install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/plplot_extra.mod"
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gtk-2-fortran)

if (NOT NO_BUILD_EXAMPLES) 
  add_executable(hl_plplot1e "hl_plplot1e.f90")
  add_dependencies(hl_plplot1e plplot_extra_module)
  target_link_libraries(hl_plplot1e gtk-fortran_static ${GTK_LIBRARIES} ${PLPLOT_LIBRARIES})
  add_test(hl_plplot1e ./hl_plplot1e)

  add_executable(hl_plplot4e "hl_plplot4e.f90")
  add_dependencies(hl_plplot4e plplot_extra_module)
  target_link_libraries(hl_plplot4e gtk-fortran_static ${GTK_LIBRARIES} ${PLPLOT_LIBRARIES})
  add_test(hl_plplot4e ./hl_plplot4e)

  add_executable(hl_plplot8e "hl_plplot8e.f90")
  add_dependencies(hl_plplot8e plplot_extra_module)
  target_link_libraries(hl_plplot8e gtk-fortran_static ${GTK_LIBRARIES} ${PLPLOT_LIBRARIES})
  add_test(hl_plplot8e ./hl_plplot8e)

  add_executable(hl_plplot17e "hl_plplot17e.f90")
  add_dependencies(hl_plplot17e plplot_extra_module)
  target_link_libraries(hl_plplot17e gtk-fortran_static ${GTK_LIBRARIES} ${PLPLOT_LIBRARIES})
  add_test(hl_plplot17e ./hl_plplot17e)

  add_executable(hl_plplot30e "hl_plplot30e.f90")
  add_dependencies(hl_plplot30e plplot_extra_module)
  target_link_libraries(hl_plplot30e gtk-fortran_static ${GTK_LIBRARIES} ${PLPLOT_LIBRARIES})
  add_test(hl_plplot30e ./hl_plplot30e)
endif (NOT NO_BUILD_EXAMPLES) 

# If the INSTALL_EXAMPLES variable is set, then copy the examples
# to ${CMAKE_INSTALL_DATAROOTDIR/gtk-fortran/examples<gtkversion>
# useful (e.g.) for building binary packages.
if (INSTALL_EXAMPLES)
   install(FILES
	"${CMAKE_CURRENT_SOURCE_DIR}/hl_plplot1e.f90"
   	"${CMAKE_CURRENT_SOURCE_DIR}/hl_plplot4e.f90"
   	"${CMAKE_CURRENT_SOURCE_DIR}/hl_plplot8e.f90"
   	"${CMAKE_CURRENT_SOURCE_DIR}/hl_plplot17e.f90"
   	"${CMAKE_CURRENT_SOURCE_DIR}/hl_plplot30e.f90"
	DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/gtk-fortran/examples2)
endif(INSTALL_EXAMPLES)
