# - FindGTK3.cmake
# This module can find the GTK3 widget libraries and several of its other
# optional components like gtkmm, glade, and glademm.
#
# NOTE: If you intend to use version checking, CMake 2.6.2 or later is
#       required.
#
# Specify one or more of the following components
# as you call this find module. See example below.
#
#   gtk
#   gtkmm
#   glade
#   glademm
#
# The following variables will be defined for your use
#
#   GTK3_FOUND - Were all of your specified components found?
#   GTK3_INCLUDE_DIRS - All include directories
#   GTK3_LIBRARIES - All libraries
#   GTK3_DEFINITIONS - Additional compiler flags
#
#   GTK3_VERSION - The version of GTK3 found (x.y.z)
#   GTK3_MAJOR_VERSION - The major version of GTK3
#   GTK3_MINOR_VERSION - The minor version of GTK3
#   GTK3_PATCH_VERSION - The patch version of GTK3
#
# Optional variables you can define prior to calling this module:
#
#   GTK3_DEBUG - Enables verbose debugging of the module
#   GTK3_ADDITIONAL_SUFFIXES - Allows defining additional directories to
#                              search for include files
#
#=================
# Example Usage:
#
#   Call find_package() once, here are some examples to pick from:
#
#   Require GTK 3.6 or later
#       find_package(GTK3 3.6 REQUIRED gtk)
#
#   Require GTK 3.10 or later and Glade
#       find_package(GTK3 3.10 REQUIRED gtk glade)
#
#   Search for GTK/GTKMM 3.8 or later
#       find_package(GTK3 3.8 COMPONENTS gtk gtkmm)
#
#   if(GTK3_FOUND)
#      include_directories(${GTK3_INCLUDE_DIRS})
#      add_executable(mygui mygui.cc)
#      target_link_libraries(mygui ${GTK3_LIBRARIES})
#   endif()
#

#=============================================================================
# Copyright 2009 Kitware, Inc.
# Copyright 2008-2012 Philip Lowman <philip@yhbt.com>
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)

# Convert to GTK3: 7/11/13.

# Version 1.5 (UNRELEASED) (CMake 2.8.12)
#   * 14236: Detect gthread library
#            Detect pangocairo on windows
#            Detect pangocairo with gtk module instead of with gtkmm
#   * 14259: Use vc100 libraries with MSVC11
#   * 14260: Export a GTK2_DEFINITIONS variable to set /vd2 when appropriate
#            (i.e. MSVC)
#   * Use the optimized/debug syntax for _LIBRARY and _LIBRARIES variables when
#     appropriate. A new set of _RELEASE variables was also added.
#   * Remove GTK2_SKIP_MARK_AS_ADVANCED option, as now the variables are
#     marked as advanced by SelectLibraryConfigurations
#   * Detect gmodule, pangoft2 and pangoxft libraries
# Version 1.4 (10/4/2012) (CMake 2.8.10)
#   * 12596: Missing paths for FindGTK2 on NetBSD
#   * 12049: Fixed detection of GTK include files in the lib folder on
#            multiarch systems.
# Version 1.3 (11/9/2010) (CMake 2.8.4)
#   * 11429: Add support for detecting GTK2 built with Visual Studio 10.
#            Thanks to Vincent Levesque for the patch.
# Version 1.2 (8/30/2010) (CMake 2.8.3)
#   * Merge patch for detecting gdk-pixbuf library (split off
#     from core GTK in 2.21).  Thanks to Vincent Untz for the patch
#     and Ricardo Cruz for the heads up.
# Version 1.1 (8/19/2010) (CMake 2.8.3)
#   * Add support for detecting GTK2 under macports (thanks to Gary Kramlich)
# Version 1.0 (8/12/2010) (CMake 2.8.3)
#   * Add support for detecting new pangommconfig.h header file
#     (Thanks to Sune Vuorela & the Debian Project for the patch)
#   * Add support for detecting fontconfig.h header
#   * Call find_package(Freetype) since it's required
#   * Add support for allowing users to add additional library directories
#     via the GTK2_ADDITIONAL_SUFFIXES variable (kind of a future-kludge in
#     case the GTK developers change versions on any of the directories in the
#     future).
# Version 0.8 (1/4/2010)
#   * Get module working under MacOSX fink by adding /sw/include, /sw/lib
#     to PATHS and the gobject library
# Version 0.7 (3/22/09)
#   * Checked into CMake CVS
#   * Added versioning support
#   * Module now defaults to searching for GTK if COMPONENTS not specified.
#   * Added HKCU prior to HKLM registry key and GTKMM specific environment
#      variable as per mailing list discussion.
#   * Added lib64 to include search path and a few other search paths where GTK
#      may be installed on Unix systems.
#   * Switched to lowercase CMake commands
#   * Prefaced internal variables with _GTK2 to prevent collision
#   * Changed internal macros to functions
#   * Enhanced documentation
# Version 0.6 (1/8/08)
#   Added GTK2_SKIP_MARK_AS_ADVANCED option
# Version 0.5 (12/19/08)
#   Second release to cmake mailing list

#=============================================================
# _GTK3_GET_VERSION
# Internal function to parse the version number in gtkversion.h
#   _OUT_major = Major version number
#   _OUT_minor = Minor version number
#   _OUT_micro = Micro version number
#   _gtkversion_hdr = Header file to parse
#=============================================================

#include(${CMAKE_CURRENT_LIST_DIR}/SelectLibraryConfigurations.cmake)
include(SelectLibraryConfigurations)

function(_GTK3_GET_VERSION _OUT_major _OUT_minor _OUT_micro _gtkversion_hdr)
    file(STRINGS ${_gtkversion_hdr} _contents REGEX "#define GTK_M[A-Z]+_VERSION[ \t]+")
    if(_contents)
        string(REGEX REPLACE ".*#define GTK_MAJOR_VERSION[ \t]+\\(([0-9]+)\\).*" "\\1" ${_OUT_major} "${_contents}")
        string(REGEX REPLACE ".*#define GTK_MINOR_VERSION[ \t]+\\(([0-9]+)\\).*" "\\1" ${_OUT_minor} "${_contents}")
        string(REGEX REPLACE ".*#define GTK_MICRO_VERSION[ \t]+\\(([0-9]+)\\).*" "\\1" ${_OUT_micro} "${_contents}")

        if(NOT ${_OUT_major} MATCHES "[0-9]+")
            message(FATAL_ERROR "Version parsing failed for GTK3_MAJOR_VERSION!")
        endif()
        if(NOT ${_OUT_minor} MATCHES "[0-9]+")
            message(FATAL_ERROR "Version parsing failed for GTK3_MINOR_VERSION!")
        endif()
        if(NOT ${_OUT_micro} MATCHES "[0-9]+")
            message(FATAL_ERROR "Version parsing failed for GTK3_MICRO_VERSION!")
        endif()

        set(${_OUT_major} ${${_OUT_major}} PARENT_SCOPE)
        set(${_OUT_minor} ${${_OUT_minor}} PARENT_SCOPE)
        set(${_OUT_micro} ${${_OUT_micro}} PARENT_SCOPE)
    else()
        message(FATAL_ERROR "Include file ${_gtkversion_hdr} does not exist")
    endif()
endfunction()

#=============================================================
# _GTK3_FIND_INCLUDE_DIR
# Internal function to find the GTK include directories
#   _var = variable to set (_INCLUDE_DIR is appended)
#   _hdr = header file to look for
#=============================================================
function(_GTK3_FIND_INCLUDE_DIR _var _hdr)

    if(GTK3_DEBUG)
        message(STATUS "[FindGTK3.cmake:${CMAKE_CURRENT_LIST_LINE}] "
                       "_GTK3_FIND_INCLUDE_DIR( ${_var} ${_hdr} )")
    endif()

    set(_gtk_packages
        # If these ever change, things will break.
        ${GTK3_ADDITIONAL_SUFFIXES}
        glibmm-2.4
        glib-2.0
        atk-1.0
        atkmm-1.6
        cairo
        cairomm-1.0
        gdk-pixbuf-2.0
        gdkmm-2.4
        giomm-2.4
        gtk-3.0
        gtkmm-2.4
        libglade-2.0
        libglademm-2.4
        pango-1.0
        pangomm-1.4
        sigc++-2.0
	gtk-unix-print-2.0
     )

    #
    # NOTE: The following suffixes cause searching for header files in both of
    # these directories:
    #         /usr/include/<pkg>
    #         /usr/lib/<pkg>/include
    #

    set(_suffixes)
    foreach(_d ${_gtk_packages})
        list(APPEND _suffixes ${_d})
        list(APPEND _suffixes ${_d}/include) # for /usr/lib/gtk-2.0/include
    endforeach()

    if(GTK3_DEBUG)
        message(STATUS "[FindGTK3.cmake:${CMAKE_CURRENT_LIST_LINE}]     "
                       "include suffixes = ${_suffixes}")
    endif()

    if(CMAKE_LIBRARY_ARCHITECTURE)
      set(_gtk3_arch_dir /usr/lib/${CMAKE_LIBRARY_ARCHITECTURE})
      if(GTK3_DEBUG)
        message(STATUS "Adding ${_gtk3_arch_dir} to search path for multiarch support")
      endif()
    endif()
    find_path(${_var}_INCLUDE_DIR ${_hdr}
        PATHS
            ${_gtk3_arch_dir}
            /usr/local/lib64
            /usr/local/lib
            /usr/lib64
            /usr/lib
            /opt/gnome/include
            /opt/gnome/lib
            /opt/openwin/include
            /usr/openwin/lib
            /sw/include
            /sw/lib
            /opt/local/include
            /opt/local/lib
            /usr/pkg/lib
            /usr/pkg/include/glib
            $ENV{GTKMM_BASEPATH}/include
            $ENV{GTKMM_BASEPATH}/lib
            [HKEY_CURRENT_USER\\SOFTWARE\\gtkmm\\2.4;Path]/include
            [HKEY_CURRENT_USER\\SOFTWARE\\gtkmm\\2.4;Path]/lib
            [HKEY_LOCAL_MACHINE\\SOFTWARE\\gtkmm\\2.4;Path]/include
            [HKEY_LOCAL_MACHINE\\SOFTWARE\\gtkmm\\2.4;Path]/lib
        PATH_SUFFIXES
            ${_suffixes}
    )

    if(${_var}_INCLUDE_DIR)
        set(GTK3_INCLUDE_DIRS ${GTK3_INCLUDE_DIRS} ${${_var}_INCLUDE_DIR} PARENT_SCOPE)
    endif()

endfunction()

#=============================================================
# _GTK3_FIND_LIBRARY
# Internal function to find libraries packaged with GTK3
#   _var = library variable to create (_LIBRARY is appended)
#=============================================================
function(_GTK3_FIND_LIBRARY _var _lib _expand_vc _append_version)

    if(GTK3_DEBUG)
        message(STATUS "[FindGTK3.cmake:${CMAKE_CURRENT_LIST_LINE}] "
                       "_GTK3_FIND_LIBRARY( ${_var} ${_lib} ${_expand_vc} ${_append_version} )")
    endif()

    # Not GTK versions per se but the versions encoded into Windows
    # import libraries (GtkMM 2.14.1 has a gtkmm-vc80-2_4.lib for example)
    # Also the MSVC libraries use _ for . (this is handled below)
    set(_versions 3.20 3.18 3.16 3.14 3.12
                  3.10  3.8  3.6  3.4  3.2 3.0 3
                  2.40 2.38 2.36 2.34 2.32
                  2.30 2.28 2.26 2.24 2.22
                  2.20 2.18 2.16 2.14 2.12
                  2.10  2.8  2.6  2.4  2.2 2.0
                  1.20 1.18 1.16 1.14 1.12
                  1.10  1.8  1.6  1.4  1.2 1.0)

    set(_library)
    set(_library_d)

    set(_library ${_lib})

    if(_expand_vc AND MSVC)
        # Add vc80/vc90/vc100 midfixes
        if(MSVC80)
            set(_library   ${_library}-vc80)
        elseif(MSVC90)
            set(_library   ${_library}-vc90)
        elseif(MSVC10)
            set(_library ${_library}-vc100)
        elseif(MSVC11)
            # Up to gtkmm-win 2.22.0-2 there are no vc110 libraries but vc100 can be used
            set(_library ${_library}-vc100)
        endif()
        set(_library_d ${_library}-d)
    endif()

    if(GTK3_DEBUG)
        message(STATUS "[FindGTK3.cmake:${CMAKE_CURRENT_LIST_LINE}]     "
                       "After midfix addition = ${_library} and ${_library_d}")
    endif()

    set(_lib_list)
    set(_libd_list)
    if(_append_version)
        foreach(_ver ${_versions})
            list(APPEND _lib_list  "${_library}-${_ver}")
            list(APPEND _libd_list "${_library_d}-${_ver}")
        endforeach()
    else()
        set(_lib_list ${_library})
        set(_libd_list ${_library_d})
    endif()

    if(GTK3_DEBUG)
        message(STATUS "[FindGTK3.cmake:${CMAKE_CURRENT_LIST_LINE}]     "
                       "library list = ${_lib_list} and library debug list = ${_libd_list}")
    endif()

    # For some silly reason the MSVC libraries use _ instead of .
    # in the version fields
    if(_expand_vc AND MSVC)
        set(_no_dots_lib_list)
        set(_no_dots_libd_list)
        foreach(_l ${_lib_list})
            string(REPLACE "." "_" _no_dots_library ${_l})
            list(APPEND _no_dots_lib_list ${_no_dots_library})
        endforeach()
        # And for debug
        set(_no_dots_libsd_list)
        foreach(_l ${_libd_list})
            string(REPLACE "." "_" _no_dots_libraryd ${_l})
            list(APPEND _no_dots_libd_list ${_no_dots_libraryd})
        endforeach()

        # Copy list back to original names
        set(_lib_list ${_no_dots_lib_list})
        set(_libd_list ${_no_dots_libd_list})
    endif()

    if(GTK3_DEBUG)
        message(STATUS "[FindGTK3.cmake:${CMAKE_CURRENT_LIST_LINE}]     "
                       "While searching for ${_var}_LIBRARY, our proposed library list is ${_lib_list}")
    endif()

    find_library(${_var}_LIBRARY_RELEASE
        NAMES ${_lib_list}
        PATHS
            /opt/gnome/lib
            /usr/openwin/lib
            /sw/lib
            $ENV{GTKMM_BASEPATH}/lib
            [HKEY_CURRENT_USER\\SOFTWARE\\gtkmm\\2.4;Path]/lib
            [HKEY_LOCAL_MACHINE\\SOFTWARE\\gtkmm\\2.4;Path]/lib
        )

    if(_expand_vc AND MSVC)
        if(GTK3_DEBUG)
            message(STATUS "[FindGTK3.cmake:${CMAKE_CURRENT_LIST_LINE}]     "
                           "While searching for ${_var}_LIBRARY_DEBUG our proposed library list is ${_libd_list}")
        endif()

        find_library(${_var}_LIBRARY_DEBUG
            NAMES ${_libd_list}
            PATHS
            $ENV{GTKMM_BASEPATH}/lib
            [HKEY_CURRENT_USER\\SOFTWARE\\gtkmm\\2.4;Path]/lib
            [HKEY_LOCAL_MACHINE\\SOFTWARE\\gtkmm\\2.4;Path]/lib
        )
    endif()

    select_library_configurations(${_var})

    set(${_var}_LIBRARY ${${_var}_LIBRARY} PARENT_SCOPE)

    set(GTK3_LIBRARIES ${GTK3_LIBRARIES} ${${_var}_LIBRARY})
    set(GTK3_LIBRARIES ${GTK3_LIBRARIES} PARENT_SCOPE)

    if(GTK3_DEBUG)
        message(STATUS "[FindGTK3.cmake:${CMAKE_CURRENT_LIST_LINE}]     "
                       "${_var}_LIBRARY_RELEASE = \"${${_var}_LIBRARY_RELEASE}\"")
        message(STATUS "[FindGTK3.cmake:${CMAKE_CURRENT_LIST_LINE}]     "
                       "${_var}_LIBRARY_DEBUG   = \"${${_var}_LIBRARY_DEBUG}\"")
        message(STATUS "[FindGTK3.cmake:${CMAKE_CURRENT_LIST_LINE}]     "
                       "${_var}_LIBRARY         = \"${${_var}_LIBRARY}\"")
    endif()

endfunction()

#=============================================================

#
# main()
#

set(GTK3_FOUND)
set(GTK3_INCLUDE_DIRS)
set(GTK3_LIBRARIES)
set(GTK3_DEFINITIONS)

if(NOT GTK3_FIND_COMPONENTS)
    # Assume they only want GTK
    set(GTK3_FIND_COMPONENTS gtk)
endif()

#
# If specified, enforce version number
#
if(GTK3_FIND_VERSION)
    #cmake_minimum_required(VERSION 2.6.2)
    set(GTK3_FAILED_VERSION_CHECK true)
    if(GTK3_DEBUG)
        message(STATUS "[FindGTK3.cmake:${CMAKE_CURRENT_LIST_LINE}] "
                       "Searching for version ${GTK3_FIND_VERSION}")
    endif()
    _GTK3_FIND_INCLUDE_DIR(GTK3_GTK gtk/gtk.h)
    if(GTK3_GTK_INCLUDE_DIR)
        _GTK3_GET_VERSION(GTK3_MAJOR_VERSION
                          GTK3_MINOR_VERSION
                          GTK3_PATCH_VERSION
                          ${GTK3_GTK_INCLUDE_DIR}/gtk/gtkversion.h)
        set(GTK3_VERSION
            ${GTK3_MAJOR_VERSION}.${GTK3_MINOR_VERSION}.${GTK3_PATCH_VERSION})
        if(GTK3_FIND_VERSION_EXACT)
            if(GTK3_VERSION VERSION_EQUAL GTK3_FIND_VERSION)
                set(GTK3_FAILED_VERSION_CHECK false)
            endif()
        else()
            if(GTK3_VERSION VERSION_EQUAL   GTK3_FIND_VERSION OR
               GTK3_VERSION VERSION_GREATER GTK3_FIND_VERSION)
                set(GTK3_FAILED_VERSION_CHECK false)
            endif()
        endif()
    else()
        # If we can't find the GTK include dir, we can't do version checking
        if(GTK3_FIND_REQUIRED AND NOT GTK3_FIND_QUIETLY)
            message(FATAL_ERROR "Could not find GTK3 include directory")
        endif()
        return()
    endif()

    if(GTK3_FAILED_VERSION_CHECK)
        if(GTK3_FIND_REQUIRED AND NOT GTK3_FIND_QUIETLY)
            if(GTK3_FIND_VERSION_EXACT)
                message(FATAL_ERROR "GTK3 version check failed.  Version ${GTK3_VERSION} was found, version ${GTK3_FIND_VERSION} is needed exactly.")
            else()
                message(FATAL_ERROR "GTK3 version check failed.  Version ${GTK3_VERSION} was found, at least version ${GTK3_FIND_VERSION} is required")
            endif()
        endif()

        # If the version check fails, exit out of the module here
        return()
    endif()
endif()

#
# Find all components
#

find_package(Freetype)
list(APPEND GTK3_INCLUDE_DIRS ${FREETYPE_INCLUDE_DIRS})
list(APPEND GTK3_LIBRARIES ${FREETYPE_LIBRARIES})

foreach(_GTK3_component ${GTK3_FIND_COMPONENTS})
    if(_GTK3_component STREQUAL "gtk")
        _GTK3_FIND_INCLUDE_DIR(GTK3_GTK gtk/gtk.h)

        if(UNIX)
            _GTK3_FIND_LIBRARY    (GTK3_GTK gtk false true)
            _GTK3_FIND_LIBRARY    (GTK3_GDK gdk false true)
        else()
            _GTK3_FIND_LIBRARY    (GTK3_GTK gtk-win32 false true)
            _GTK3_FIND_LIBRARY    (GTK3_GDK gdk-win32 false true)
        endif()

        _GTK3_FIND_INCLUDE_DIR(GTK3_GDK gdk/gdk.h)
        _GTK3_FIND_INCLUDE_DIR(GTK3_GDKCONFIG gdk/gdkconfig.h)

        _GTK3_FIND_INCLUDE_DIR(GTK3_CAIRO cairo.h)
        _GTK3_FIND_LIBRARY    (GTK3_CAIRO cairo false false)

        _GTK3_FIND_INCLUDE_DIR(GTK3_FONTCONFIG fontconfig/fontconfig.h)

        _GTK3_FIND_INCLUDE_DIR(GTK3_PANGO pango/pango.h)
        _GTK3_FIND_LIBRARY    (GTK3_PANGO pango false true)

        _GTK3_FIND_LIBRARY    (GTK3_PANGOCAIRO pangocairo false true)

        _GTK3_FIND_LIBRARY    (GTK3_PANGOFT2 pangoft2 false true)

        _GTK3_FIND_LIBRARY    (GTK3_PANGOXFT pangoxft false true)

        _GTK3_FIND_INCLUDE_DIR(GTK3_GDK_PIXBUF gdk-pixbuf/gdk-pixbuf.h)
        _GTK3_FIND_LIBRARY    (GTK3_GDK_PIXBUF gdk_pixbuf false true)

        _GTK3_FIND_LIBRARY    (GTK3_GTHREAD gthread false true)

        _GTK3_FIND_LIBRARY    (GTK3_GMODULE gmodule false true)

        _GTK3_FIND_LIBRARY    (GTK3_GIO gio false true)

        _GTK3_FIND_INCLUDE_DIR(GTK3_ATK atk/atk.h)
        _GTK3_FIND_LIBRARY    (GTK3_ATK atk false true)

        _GTK3_FIND_INCLUDE_DIR(GTK3_GOBJECT gobject/gobject.h)
        _GTK3_FIND_LIBRARY    (GTK3_GOBJECT gobject false true)

        _GTK3_FIND_INCLUDE_DIR(GTK3_GLIB glib.h)
        _GTK3_FIND_INCLUDE_DIR(GTK3_GLIBCONFIG glibconfig.h)
        _GTK3_FIND_LIBRARY    (GTK3_GLIB glib false true)

    elseif(_GTK3_component STREQUAL "gtkmm")

        _GTK3_FIND_INCLUDE_DIR(GTK3_GTKMM gtkmm.h)
        _GTK3_FIND_INCLUDE_DIR(GTK3_GTKMMCONFIG gtkmmconfig.h)
        _GTK3_FIND_LIBRARY    (GTK3_GTKMM gtkmm true true)

        _GTK3_FIND_INCLUDE_DIR(GTK3_GDKMM gdkmm.h)
        _GTK3_FIND_INCLUDE_DIR(GTK3_GDKMMCONFIG gdkmmconfig.h)
        _GTK3_FIND_LIBRARY    (GTK3_GDKMM gdkmm true true)

        _GTK3_FIND_INCLUDE_DIR(GTK3_PANGOMM pangomm.h)
        _GTK3_FIND_INCLUDE_DIR(GTK3_PANGOMMCONFIG pangommconfig.h)
        _GTK3_FIND_LIBRARY    (GTK3_PANGOMM pangomm true true)

        _GTK3_FIND_INCLUDE_DIR(GTK3_CAIROMM cairomm/cairomm.h)
        _GTK3_FIND_INCLUDE_DIR(GTK3_CAIROMMCONFIG cairommconfig.h)
        _GTK3_FIND_LIBRARY    (GTK3_CAIROMM cairomm true true)

        _GTK3_FIND_INCLUDE_DIR(GTK3_GIOMM giomm.h)
        _GTK3_FIND_INCLUDE_DIR(GTK3_GIOMMCONFIG giommconfig.h)
        _GTK3_FIND_LIBRARY    (GTK3_GIOMM giomm true true)

        _GTK3_FIND_INCLUDE_DIR(GTK3_ATKMM atkmm.h)
        _GTK3_FIND_LIBRARY    (GTK3_ATKMM atkmm true true)

        _GTK3_FIND_INCLUDE_DIR(GTK3_GLIBMM glibmm.h)
        _GTK3_FIND_INCLUDE_DIR(GTK3_GLIBMMCONFIG glibmmconfig.h)
        _GTK3_FIND_LIBRARY    (GTK3_GLIBMM glibmm true true)

        _GTK3_FIND_INCLUDE_DIR(GTK3_SIGC++ sigc++/sigc++.h)
        _GTK3_FIND_INCLUDE_DIR(GTK3_SIGC++CONFIG sigc++config.h)
        _GTK3_FIND_LIBRARY    (GTK3_SIGC++ sigc true true)

    elseif(_GTK3_component STREQUAL "glade")

        _GTK3_FIND_INCLUDE_DIR(GTK3_GLADE glade/glade.h)
        _GTK3_FIND_LIBRARY    (GTK3_GLADE glade false true)

    elseif(_GTK3_component STREQUAL "glademm")

        _GTK3_FIND_INCLUDE_DIR(GTK3_GLADEMM libglademm.h)
        _GTK3_FIND_INCLUDE_DIR(GTK3_GLADEMMCONFIG libglademmconfig.h)
        _GTK3_FIND_LIBRARY    (GTK3_GLADEMM glademm true true)

    else()
        message(FATAL_ERROR "Unknown GTK3 component ${_component}")
    endif()
endforeach()

#
# Solve for the GTK3 version if we haven't already
#
if(NOT GTK3_FIND_VERSION AND GTK3_GTK_INCLUDE_DIR)
    _GTK3_GET_VERSION(GTK3_MAJOR_VERSION
                      GTK3_MINOR_VERSION
                      GTK3_PATCH_VERSION
                      ${GTK3_GTK_INCLUDE_DIR}/gtk/gtkversion.h)
    set(GTK3_VERSION ${GTK3_MAJOR_VERSION}.${GTK3_MINOR_VERSION}.${GTK3_PATCH_VERSION})
endif()

#
# On MSVC, according to https://wiki.gnome.org/gtkmm/MSWindows, the /vd2 flag needs to be
# passed to the compiler in order to use gtkmm
#
if(MSVC)
    foreach(_GTK3_component ${GTK3_FIND_COMPONENTS})
        if(_GTK3_component STREQUAL "gtkmm")
            set(GTK3_DEFINITIONS "/vd2")
        elseif(_GTK3_component STREQUAL "glademm")
            set(GTK3_DEFINITIONS "/vd2")
        endif()
    endforeach()
endif()

#
# Try to enforce components
#

set(_GTK3_did_we_find_everything true)  # This gets set to GTK3_FOUND

foreach(_GTK3_component ${GTK3_FIND_COMPONENTS})
    string(TOUPPER ${_GTK3_component} _COMPONENT_UPPER)

    if(_GTK3_component STREQUAL "gtk")
        FIND_PACKAGE_HANDLE_STANDARD_ARGS(GTK3_${_COMPONENT_UPPER} "Some or all of the gtk libraries were not found."
            GTK3_GTK_LIBRARY
            GTK3_GTK_INCLUDE_DIR

            GTK3_GDK_INCLUDE_DIR
            GTK3_GDKCONFIG_INCLUDE_DIR
            GTK3_GDK_LIBRARY

            GTK3_GLIB_INCLUDE_DIR
            GTK3_GLIBCONFIG_INCLUDE_DIR
            GTK3_GLIB_LIBRARY
        )
    elseif(_GTK3_component STREQUAL "gtkmm")
        FIND_PACKAGE_HANDLE_STANDARD_ARGS(GTK3_${_COMPONENT_UPPER} "Some or all of the gtkmm libraries were not found."
            GTK3_GTKMM_LIBRARY
            GTK3_GTKMM_INCLUDE_DIR
            GTK3_GTKMMCONFIG_INCLUDE_DIR

            GTK3_GDKMM_INCLUDE_DIR
            GTK3_GDKMMCONFIG_INCLUDE_DIR
            GTK3_GDKMM_LIBRARY

            GTK3_GLIBMM_INCLUDE_DIR
            GTK3_GLIBMMCONFIG_INCLUDE_DIR
            GTK3_GLIBMM_LIBRARY

        )
    elseif(_GTK3_component STREQUAL "glade")
        FIND_PACKAGE_HANDLE_STANDARD_ARGS(GTK3_${_COMPONENT_UPPER} "The glade library was not found."
            GTK3_GLADE_LIBRARY
            GTK3_GLADE_INCLUDE_DIR
        )
    elseif(_GTK3_component STREQUAL "glademm")
        FIND_PACKAGE_HANDLE_STANDARD_ARGS(GTK3_${_COMPONENT_UPPER} "The glademm library was not found."
            GTK3_GLADEMM_LIBRARY
            GTK3_GLADEMM_INCLUDE_DIR
            GTK3_GLADEMMCONFIG_INCLUDE_DIR
        )
    endif()

    if(NOT GTK3_${_COMPONENT_UPPER}_FOUND)
        set(_GTK3_did_we_find_everything false)
    endif()
endforeach()

if(_GTK3_did_we_find_everything AND NOT GTK3_VERSION_CHECK_FAILED)
    set(GTK3_FOUND true)
else()
    # Unset our variables.
    set(GTK3_FOUND false)
    set(GTK3_VERSION)
    set(GTK3_VERSION_MAJOR)
    set(GTK3_VERSION_MINOR)
    set(GTK3_VERSION_PATCH)
    set(GTK3_INCLUDE_DIRS)
    set(GTK3_LIBRARIES)
    set(GTK3_DEFINITIONS)
endif()

if(GTK3_INCLUDE_DIRS)
   list(REMOVE_DUPLICATES GTK3_INCLUDE_DIRS)
endif()

