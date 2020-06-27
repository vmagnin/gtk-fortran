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

examples = {
  'bazaar': ['bazaar.f90'],
  'cairo-tests': ['cairo-tests.f90'],
  'cairo-basics': ['cairo-basics.f90'],
  'cairo-basics-click': ['cairo-basics-click.f90'],    
  'gtkzero_gapp': ['gtkzero_gapp.f90'],
  'gtkbuilder': ['gtkbuilder.f90'],
  'gtkbuilder2': ['gtkbuilder2.f90'],
  'gtkhello': ['gtkhello.f90'],    
  'hl_assistant': ['hl_assistant.f90'],    
  'hl_choosers': ['hl_choosers.f90'],    
  'hl_combo': ['hl_combo.f90'],    
  'hl_containers': ['hl_containers.f90'],    
  'hl_dialog': ['hl_dialog.f90'],    
  'hl_list1': ['hl_list1.f90'],    
  'hl_list_n': ['hl_list_n.f90'],    
  'hl_list_renderers': ['hl_list_renderers.f90'],    
  'hl_pbar': ['hl_pbar.f90'],    
  'hl_radio': ['hl_radio.f90'],    
  'hl_sliders': ['hl_sliders.f90'],    
  'hl_sliders2': ['hl_sliders2.f90'],    
  'hl_textview': ['hl_textview.f90'],    
  'hl_tree': ['hl_tree.f90'],    
  'hl_cairo1': ['hl_cairo1.f90'],    
  'hl_cairo_clock': ['hl_cairo_clock.f90'],    
  'hl_cairo_viewer': ['hl_cairo_viewer.f90'],    
  'hl_infobar': ['hl_infobar.f90'],    
  'julia_pixbuf': ['julia_pixbuf.f90'],    
  'list_demo': ['list_demo.f90'],    
  'mandelbrot_pixbuf': ['mandelbrot_pixbuf.f90'],    
  'mandelbrot_pixbuf_zoom': ['mandelbrot_pixbuf_zoom.f90'],    
  'notebooks': ['notebooks.f90'],
  'pixbuf_without_gui': ['pixbuf_without_gui.f90'],  
  'gio_demo': ['gio_demo.f90'],    
  'tests': ['tests.f90']   
  }

foreach name, file : examples
  exe = executable(name, file, link_with : static_lib)
  test(name, exe)
endforeach
  
generated_file = configure_file(copy: true, input:'gtkbuilder.glade', output:'gtkbuilder.glade') 
