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
    # Only gtkf-sketcher.f90 needs Fortran 2008 for execute_command_line():
    "-std=f2008"
    "-Wtabs"
    "-fcheck=all"
    "-fbacktrace"
    "-Wno-unused-dummy-argument"
  )
  if(UNIX)
    set(CMAKE_EXE_LINKER_FLAGS_INIT "-rdynamic")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  # ifort compiler:
  set(
    CMAKE_Fortran_FLAGS_RELEASE_INIT
    "-g"
    "-warn all"
    "-warn nounused"
  )
  set(
    CMAKE_Fortran_FLAGS_DEBUG_INIT
    "-O3"
  )
else()
  # Standard flags for all the other compilers:
  set(
    CMAKE_Fortran_FLAGS_RELEASE_INIT
    "-g"
  )
  set(
    CMAKE_Fortran_FLAGS_DEBUG_INIT
    "-O3"
  )
endif()
string(REPLACE ";" " " CMAKE_Fortran_FLAGS_RELEASE_INIT "${CMAKE_Fortran_FLAGS_RELEASE_INIT}")
string(REPLACE ";" " " CMAKE_Fortran_FLAGS_DEBUG_INIT "${CMAKE_Fortran_FLAGS_DEBUG_INIT}")
string(REPLACE ";" " " CMAKE_EXE_LINKER_FLAGS_INIT "${CMAKE_EXE_LINKER_FLAGS_INIT}")
