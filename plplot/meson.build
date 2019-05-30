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

dep1 = dependency('plplot', version: '>=5.13.0', required: false)
dep2 = dependency('plplot-fortran', version: '>=5.13.0', required: false)

if dep2.found()
    # Compiles but does not link (-c option):
    #plplot_extra = executable('plplot_extra', 'plplot_extra_ndef.f90', dependencies: [dep1, dep2], link_args: '-c')
    
    plplotexamples = {
    'hl_plplot1e': ['hl_plplot1e.f90', 'plplot_extra_ndef.f90'],
    'hl_plplot4e': ['hl_plplot4e.f90', 'plplot_extra_ndef.f90'],
    'hl_plplot8e': ['hl_plplot8e.f90', 'plplot_extra_ndef.f90'],
    'hl_plplot17e': ['hl_plplot17e.f90', 'plplot_extra_ndef.f90'],  
    'hl_plplot17e_gto': ['hl_plplot17e_gto.f90', 'plplot_extra_ndef.f90'],
    'hl_plplot30e': ['hl_plplot30e.f90', 'plplot_extra_ndef.f90']
    }
  
    foreach name, src : plplotexamples
        exe = executable(name, src, dependencies: [dep1, dep2], link_with : static_lib)
        test(name, exe)
    endforeach
    
    # TODO: install the plplot_extra.mod file in /usr/local/include/gtk-3-fortran/
    # But it is built in a directory named like d37b71e@@plplot_extra@exe
    # How can we install it cleanly ?
    # See the .mod installation issues:
    # https://github.com/mesonbuild/meson/issues/4708
    # https://github.com/mesonbuild/meson/issues/5374
else
    warning('PLplot>=5.13.0 NOT FOUND !')
endif
