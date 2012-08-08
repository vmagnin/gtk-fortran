#
# copyright : (c) 2010 Maxime Lenoir, Alain Coulais,
#                      Sylwester Arabas and Orion Poplawski
#
#  Modified to find Fortran rather than C++ libraries:
#		July 2012: James Tappin
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#

find_package(PkgConfig)
pkg_check_modules(PLPLOTF95 QUIET plplotd-f95)

find_library(PLPLOT_LIBRARY NAMES plplotd)
find_library(PLPLOTF95_LIBRARY NAMES plplotf95d)
find_library(PLPLOTF95C_LIBRARY NAMES plplotf95cd)
set(PLPLOT_LIBRARIES ${PLPLOT_LIBRARY} ${PLPLOTF95_LIBRARY} ${PLPLOTF95C_LIBRARY})
find_path(PLPLOT_INCLUDE_DIR NAMES plplot/plplot.h)
find_path(PLPLOT_MODULE_DIR NAMES plplot.mod PATHS ${PLPLOTF95_INCLUDE_DIRS}) 
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(PLPLOT DEFAULT_MSG PLPLOT_LIBRARIES 
  PLPLOT_INCLUDE_DIR)

mark_as_advanced(
  PLPLOT_LIBRARY
  PLPLOTCF95_LIBRARY
  PLPLOTCF95C_LIBRARY
  PLPLOT_LIBRARIES
  PLPLOT_INCLUDE_DIR 
  PLPLOT_MODULE_DIR 
  )
