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
# Contributed by James Tappin
# Last updated: vmagnin 07/18/2016, vmagnin 2019-03-12 (PLplot>5.10)

#===============================================================================
# PLplot integration
# ------------------
# Since PLplot>=5.10, the PLplot libraries have been renamed (plplot and 
# plplot-fortran) and the ISO_C_BINDING has been adopted.
# By now (March 2019), the find_package(plplot) command gives a lot of errors
# in Debian/Ubuntu distributions because of some renaming in Debian packages.
# As a temporary workaround, we are using pkg-config.
# The code will be cleaned later.
#===============================================================================

include_directories("${CMAKE_BINARY_DIR}/plplot")
include_directories("${CMAKE_BINARY_DIR}/src/modules")
include_directories(${GTK_INCLUDES})

#===============================================================================
# TODO: clean that code which concerns old PLplot versions.
# NEW_PLPLOT_DEFS is for PLplot>=5.9.8
#===============================================================================
if(NEW_PLPLOT_DEFS)
  set(extra_file "plplot_extra_ndef.f90")
else()
  set(extra_file "plplot_extra.f90")
endif()

add_custom_command(
  OUTPUT plplot_extra.mod
  COMMAND ${CMAKE_Fortran_COMPILER} -c ${CMAKE_CURRENT_SOURCE_DIR}/${extra_file}
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${extra_file}
)
add_custom_target(plplot_extra_module ALL DEPENDS plplot_extra.mod)

install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/plplot_extra.mod"
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/gtk-${GTKv}-fortran
)

if (NOT NO_BUILD_EXAMPLES) 
#===============================================================================
  # TODO: clean that code which concerns old PLplot versions
  # NEW_PLPLOT is for PLplot>=5.9.10
#===============================================================================
#   if (NEW_PLPLOT)
#     message(STATUS "New PLPLOT")
     set (example_1e hl_plplot1e)
     set (example_4e hl_plplot4e)
#   else (NEW_PLPLOT)
#     message(STATUS "Old PLPLOT")
#     set (example_1e hl_plplot1e_old)
#     set (example_4e hl_plplot4e_old)
#   endif (NEW_PLPLOT)

  foreach(example hl_plplot8e hl_plplot17e hl_plplot17e_gto hl_plplot30e 
          ${example_1e} ${example_4e})
    add_executable(${example} "${example}.f90")
    add_dependencies(${example} plplot_extra_module)
    target_link_libraries(${example} gtk-fortran_static ${GTK_LIBRARIES} ${PLPLOT-FORTRAN_LIBRARIES} ${PLPLOT_LIBRARIES})
    add_test(${example} ./${example})
  endforeach(example)
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
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_plplot17e_gto.f90"
    "${CMAKE_CURRENT_SOURCE_DIR}/hl_plplot30e.f90"
    DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/gtk-fortran/examples${GTKv}
  )
endif(INSTALL_EXAMPLES)
