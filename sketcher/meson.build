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

exe = executable('gtkf-sketcher', 'gtkf-sketcher.f90', link_with : static_lib)
test('gtkf-sketcher', exe)
  
sketcher_files = {
  'gtkf-sketcher.glade': ['gtkf-sketcher.glade'],
  'example.glade': ['example.glade'],
  'default.options': ['default.options']
  }
  
foreach out_file, in_file : sketcher_files
  configure_file(copy: true, input:in_file, output:out_file) 
endforeach

subdir('data')
subdir('example')
