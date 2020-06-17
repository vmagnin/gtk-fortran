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
# Last modifications: vmagnin 2020-06-17

# Build the gtk-fortran libraries (static and shared):
src = [
  'atk-auto.f90', 
  'cairo-auto.f90', 
  'gdk-auto.f90',
  'gdk-pixbuf-auto.f90',
  'gdk-pixbuf-hl.f90',
  'gdkevents-auto.f90',
  'glib-auto.f90', 
  'gtk.f90', 
  'gtk-draw-hl.f90',
  'gtk-hl.f90', 
  'gtk-hl-assistant.f90', 
  'gtk-hl-button.f90', 
  'gtk-hl-chooser.f90',
  'gtk-hl-combobox.f90',
  'gtk-hl-container.f90', 
  'gtk-hl-dialog.f90',
  'gtk-hl-entry.f90', 
  'gtk-hl-infobar.f90',
  'gtk-hl-menu.f90',
  'gtk-hl-misc.f90', 
  'gtk-hl-progress.f90',  
  'gtk-hl-spin-slider.f90', 
  'gtk-hl-tree.f90', 
  'gtk-sup.f90',
  'pango-auto.f90', 
  'unixonly-auto.f90']
# TODO: under MSYS2/WINDOWS, do we need 'mswindowsonly-auto.f90' or 'unixonly-auto.f90' ?

static_lib = static_library(gtk_V_fortran, sources: src, dependencies: gtk_dep, install: true)
# See: http://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html
shared_lib = shared_library(gtk_V_fortran, sources: src, dependencies: gtk_dep, install: true, soversion: '3')

# FIXME (it does not work): "Source files will be compiled only once and object files will be reused to build both shared and static libraries":
#lib = both_libraries('gtk-3-fortran', sources : src, dependencies: gtk_dep)
#static_lib = lib.get_static_lib()
#shared_lib = lib.get_shared_lib()


# FIXME : install .mod files
# Normally, we should write something like:
#mod_files = [
#    'gtk.mod',
#    'g.mod'
#    ]
#install_headers(mod_files, subdir:  gtk_V_fortran)
# but Meson does not yet support installing .mod files, like .h files.
# See the .mod installation issues:
# https://github.com/mesonbuild/meson/issues/4708
# https://github.com/mesonbuild/meson/issues/5374
# THIS IS A WORKAROUND THAT WILL BE LATER REMOVED:
# MAYBE YOU WILL HAVE TO MODIFY THE 25a6634 id ON YOUR MACHINE:
install_subdir(meson.build_root()/'src'/'25a6634@'/gtk_V_fortran+'@sha',
                install_dir: 'include'/gtk_V_fortran,
                strip_directory: true,
                exclude_files: ['src_atk-auto.f90.o', 'src_gdk-pixbuf-auto.f90.o', 'src_gtk.f90.o', 'src_gtk-hl-chooser.f90.o', 'src_gtk-hl-entry.f90.o', 'src_gtk-hl-misc.f90.o', 'src_gtk-sup.f90.o', 'src_cairo-auto.f90.o', 'src_gdk-pixbuf-hl.f90.o', 'src_gtk-hl-combobox.f90.o', 'src_gtk-hl.f90.o', 'src_gtk-hl-progress.f90.o', 'src_pango-auto.f90.o', 'src_gdk-auto.f90.o', 'src_glib-auto.f90.o', 'src_gtk-hl-assistant.f90.o', 'src_gtk-hl-container.f90.o', 'src_gtk-hl-infobar.f90.o', 'src_gtk-hl-spin-slider.f90.o', 'src_unixonly-auto.f90.o', 'src_gdkevents-auto.f90.o', 'src_gtk-draw-hl.f90.o', 'src_gtk-hl-button.f90.o', 'src_gtk-hl-dialog.f90.o', 'src_gtk-hl-menu.f90.o', 'src_gtk-hl-tree.f90.o'])
# DOES NOT WORK:
#install_subdir(shared_lib.private_dir_include(),
# src/meson.build:78:0: ERROR: Arguments must be strings.

# Generate the pkgconfig file. The -Wl,-R option is necessary to dynamically load the shared library when launching an executable.
pkg = import('pkgconfig')
pkg.generate(shared_lib, subdirs: gtk_V_fortran, description: 'Fortran 2003 interface for GTK', name: gtk_V_fortran, version: gtk_dep.version(), url: 'https://github.com/vmagnin/gtk-fortran', libraries: gtk_dep, extra_cflags: '-Wl,-R'+get_option('prefix')/get_option('libdir'))


# Install src/usemodules.pl as command gtk_V_fortran+'-modscan':
usemodules_file_data = configuration_data({
    'PREFIX' : get_option('prefix'),
    'GTK' : MAJOR_VERSION,
    'SHARE' : 'share'
    })
configure_file(input: 'usemodules.pl', output: gtk_V_fortran+'-modscan', configuration: usemodules_file_data, install: true, install_dir: 'bin', install_mode: 'rwxr-xr-x') 
# Install its man page:
modscan_manpage_data = configuration_data({'GTK' : MAJOR_VERSION})
configure_file(input: 'gtk-fortran-modscan.man', output: gtk_V_fortran+'-modscan.1', configuration: modscan_manpage_data, install: true, install_dir: get_option('mandir')/'man1') 
# Install lists of supported GTK functions and enums (used by usemodules.pl):
install_data('gtk-fortran-index.csv', rename: gtk_V_fortran+'-index.csv', install_dir: get_option('datadir')/'gtk-fortran')
install_data('gtk-enumerators.lis', rename: 'gtk-'+MAJOR_VERSION+'-enumerators.lis', install_dir: get_option('datadir')/'gtk-fortran')


# TODO: should we also install the sketcher tool ?
