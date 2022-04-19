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
# Contributed by James Tappin
# Last updated: vmagnin 2019-03-14 (PLplot>=5.13), 2022-04-15
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

include_directories("${CMAKE_BINARY_DIR}/plplot")
include_directories("${CMAKE_BINARY_DIR}/src/modules")

#===============================================================================
# PLplot extra for accessing the pl_cmd() routine:
#===============================================================================
set(extra_file "../src/plplot_extra.f90")

add_custom_command(
  OUTPUT plplot_extra.mod
  COMMAND ${CMAKE_Fortran_COMPILER} -c ${CMAKE_CURRENT_SOURCE_DIR}/${extra_file}
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${extra_file}
)

add_custom_target(plplot_extra_module ALL DEPENDS plplot_extra.mod)

install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/plplot_extra.mod"
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${gtk_V_fortran}
)

#===============================================================================
# Build PLplot examples:
#===============================================================================
if (NOT NO_BUILD_EXAMPLES)
  foreach(example hl_plplot8e hl_plplot17e hl_plplot17e_gto hl_plplot30e
          hl_plplot1e hl_plplot4e)
    add_executable(${example} "${example}.f90")
    add_dependencies(${example} plplot_extra_module)
    target_link_libraries(${example} gtk-fortran_static ${GTK_LIBRARIES}
                          ${PLPLOT-FORTRAN_LIBRARIES} ${PLPLOT_LIBRARIES})
    add_test(${example} ./${example})
  endforeach(example)
endif (NOT NO_BUILD_EXAMPLES)

#===============================================================================
# If the INSTALL_EXAMPLES variable is set, then copy the examples
# to ${CMAKE_INSTALL_DATAROOTDIR/gtk-fortran/examples<gtkversion>
# useful (e.g.) for building binary packages.
#===============================================================================
if (INSTALL_EXAMPLES)
  install(FILES
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_plplot1e.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_plplot4e.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_plplot8e.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_plplot17e.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_plplot17e_gto.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_plplot30e.f90"
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/gtk-fortran/examples${GTKv}
  )
endif(INSTALL_EXAMPLES)
