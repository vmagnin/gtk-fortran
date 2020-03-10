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

project('gtk-fortran', ['fortran', 'c'], license: 'GPL3', meson_version: '>=0.51', default_options: ['fortran_std=f2008'])

# Major version of GTK for the current branch (from VERSIONS file):
MAJOR_VERSION = run_command('sed', '-n', '-E', 's/gtk-fortran;(.*)/\\1/p', 
                                 files('VERSIONS'), check:true).stdout().strip()
# Deriving the library name :
gtk_V_fortran = 'gtk-' + MAJOR_VERSION + '-fortran'
message('Building '+gtk_V_fortran)

# Set here the GTK version to use:
if MAJOR_VERSION.to_int() <= 3
    gtk_dep = dependency('gtk+-'+MAJOR_VERSION+'.0')
else
    gtk_dep = dependency('gtk'+MAJOR_VERSION)
endif

# Print some informations:
system = host_machine.system()
message('Host machine system = '+system)
build_type = get_option('buildtype')
message('Build type: '+build_type)

fortran = meson.get_compiler('fortran')

# Define arguments for different build types (debug is default):
if build_type == 'debug'
  warning_level = 3
  supported_arguments = fortran.get_supported_arguments([
  '-pthread',
  '-pedantic',
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
