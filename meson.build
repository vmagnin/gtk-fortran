# Copyright (C) 2019
# Free Software Foundation, Inc.
#
# This file is part of the gtk-fortran GTK / Fortran Interface library.
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
# Contributed by vmagnin (first commit: 2019-05-30)
# Meson >= 0.51 is needed (see https://github.com/mesonbuild/meson/pull/4979)
# By default, Meson installs projects to the prefix /usr/local/

project('gtk-fortran', ['fortran', 'c'], license: 'GPL3', meson_version: '>=0.51', version: 'gtk-3-fortran')

# Set here the GTK version to use:
gtk_dep = dependency('gtk+-3.0')
#gtk_dep = dependency('gtk4')

# Deriving the library name :
message('GTK '+gtk_dep.version())
version_split = gtk_dep.version().split('.')
MAJOR_VERSION = version_split[0]
gtk_V_fortran = 'gtk-' + MAJOR_VERSION + '-fortran'
message(gtk_V_fortran)

# Print some informations:
system = host_machine.system()
message('Host machine system = '+system)
build_type = get_option('buildtype')
message('Build type: '+build_type)

fortran = meson.get_compiler('fortran')

# Define arguments for different build types (debug is default):
if build_type == 'debug'
  supported_arguments = fortran.get_supported_arguments([
  '-pthread',
  '-Wextra',
  '-pedantic',
  '-std=f2008',
  '-Wtabs',
  '-fcheck=all',
  '-fbacktrace',
  '-Wno-unused-dummy-argument'])
elif build_type == 'release'
  # -w inhibit all warning messages
  supported_arguments = fortran.get_supported_arguments([
  '-pthread',
  '-w'])
endif

add_project_arguments(supported_arguments, language: 'fortran')

# Needed for programs using GTKBuilder (gtkf-sketcher and gtkbuilder2):
supported_rdynamic = fortran.get_supported_arguments('-rdynamic')
add_project_link_arguments(supported_rdynamic, language: 'fortran')

# Each of these subdirectories contain a meson.build file:
subdir('src')
subdir('examples')
subdir('sketcher') 
subdir('plplot')

# WARNING:
warning('*** The gtk-fortran meson build system is EXPERIMENTAL ! ***')
warning('*** Use CMake for production ! ***')
