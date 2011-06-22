! Copyright (C) 2011
! Free Software Foundation, Inc.

! This file is part of the gtk-fortran gtk+ Fortran Interface library.

! This is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3, or (at your option)
! any later version.

! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! Under Section 7 of GPL version 3, you are granted additional
! permissions described in the GCC Runtime Library Exception, version
! 3.1, as published by the Free Software Foundation.

! You should have received a copy of the GNU General Public License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! gfortran -Wl,--rpath,../../src -I../../src gtkextra-auto.f90 gtkextra_plot.f90 -o gtkextra_plot -L../../src -lgtk-fortran `pkg-config --cflags --libs gtkextra-3.0`
! Contributed by Jens Hunger

module handlers
  use iso_c_binding, only: c_int, c_ptr, c_funloc, c_loc, c_null_ptr, c_double
  
  use gtk, only: gtk_container_add, gtk_main, gtk_widget_get_window, gtk_widget_show_all,&
  gtk_window_new, gtk_window_set_title, gtk_box_pack_start, gtk_table_new, gtk_vbox_new,&
  gtk_table_attach, gtk_label_new, gtk_button_new_with_label, gtk_main_quit,&
  TRUE, FALSE, CNULL, NULL, GTK_WINDOW_TOPLEVEL, gtk_init, g_signal_connect,&
  gtk_justify_center, gtk_widget_show, gtk_widget_queue_draw

  use g, only: g_timeout_add

  use gtkextra
  
  implicit none

  include "gtkextraenums-auto.f90"

  type, bind(c) :: gdkcolor
    integer(c_int):: pixel
    integer(c_int):: red
    integer(c_int):: green
    integer(c_int):: blue
  end type gdkcolor
  
  type(c_ptr)                             :: my_window, w_vbox, w_button
  type(c_ptr)                             :: w_plot_canvas, w_plot
  type(c_ptr)                             :: colorptr, dataset
  type(c_ptr)                             :: child !  GtkPlotCanvasChild *child;
  type(c_ptr)                             :: x_axis, y_axis !GtkPlotAxis *x_axis, *y_axis;
  type(gdkcolor),target                   :: color
  real(c_double), dimension(1:6), target  :: x, y, dx, dy
  integer(c_int)                          :: points = 6

  integer(c_int) :: run_status = TRUE
  integer(c_int) :: boolresult
!  logical :: boolevent
!  integer :: width, height
  
  
contains
  ! User defined event handlers go here
  function delete_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata
    run_status = FALSE
    ret = FALSE
    call gtk_main_quit()
  end function delete_event

  function cb_change_dataset(widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata

!    GtkWidget *canvas = GTK_WIDGET(user_data);
  x = [0., 0.2, 0.4, 0.6, 0.8, 1.0]
  call random_number(y)
  call gtk_plot_data_set_points(dataset, x, y, dx, dy, points)
  call gtk_plot_canvas_paint(w_plot_canvas)
  call gtk_widget_queue_draw(w_plot_canvas)

  ret = TRUE
  end function cb_change_dataset
  
end module handlers


program gtkextra_plot_demo
  use handlers
  implicit none
  
  call gtk_init ()
  
  ! Properties of the main window :
  my_window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  call gtk_window_set_title(my_window, "GtkExtra Plot Demo"//CNULL)
  call g_signal_connect (my_window, "delete-event"//CNULL, c_funloc(delete_event))

  ! VBox container
  w_vbox = gtk_vbox_new(FALSE, 0)
  call gtk_container_add(my_window, w_vbox)

  ! Create GtkExtra plot canvas
  w_plot_canvas = gtk_plot_canvas_new(500,300,1.0_c_double)
! // Do this in order to have auto resize of plot with window.
!GTK_PLOT_CANVAS_UNSET_FLAGS (GTK_PLOT_CANVAS (w_plot_canvas),GTK_PLOT_CANVAS_DND_FLAGS);
!GTK_PLOT_CANVAS_SET_FLAGS (GTK_PLOT_CANVAS (w_plot_canvas),GTK_PLOT_CANVAS_CAN_RESIZE);

  ! Create new GtkExtra plot and add it to plot canvas
  w_plot = gtk_plot_new(NULL)
  call gtk_plot_set_range(w_plot, 0.0_c_double, 1._c_double, -0.5_c_double, 1.4_c_double)
  call gtk_plot_axis_set_ticks(gtk_plot_get_axis(w_plot,GTK_PLOT_AXIS_LEFT),0.5_c_double,5)
  child = gtk_plot_canvas_plot_new(w_plot)
  call gtk_plot_canvas_put_child(w_plot_canvas, child,.10_c_double, .1_c_double, .9_c_double, .85_c_double)

  ! Setup the axis
  call gtk_plot_set_legends_border(w_plot, 0, 0)
  call gtk_plot_axis_set_visible(gtk_plot_get_axis(w_plot,GTK_PLOT_AXIS_TOP), FALSE)
  call gtk_plot_axis_set_visible(gtk_plot_get_axis(w_plot,GTK_PLOT_AXIS_RIGHT), FALSE)
  x_axis = gtk_plot_get_axis(w_plot, GTK_PLOT_AXIS_BOTTOM)
  y_axis = gtk_plot_get_axis(w_plot, GTK_PLOT_AXIS_LEFT)
  call gtk_plot_axis_set_title(x_axis, "Time [s]"//CNULL)
  call gtk_plot_axis_hide_title(y_axis)
  call gtk_plot_x0_set_visible(w_plot, TRUE)
  call gtk_plot_y0_set_visible(w_plot, TRUE)
  call gtk_plot_canvas_put_child(w_plot_canvas,&
  gtk_plot_canvas_text_new("Helvetica"//CNULL,12, 0,NULL, NULL,FALSE,GTK_JUSTIFY_CENTER,"Intensity [a.u.]"//CNULL),&
  .10_c_double, .1_c_double, .20_c_double, .20_c_double)

  ! Build data and put it in plot
  x = [0., 0.2, 0.4, 0.6, 0.8, 1.0]
  y = [1.2, .4, .5, .35, .10, .40]
  dx = 0.0
  dy = 0.0
  points = 6
  dataset = gtk_plot_data_new()
  call gtk_plot_data_set_points(dataset, x, y, dx, dy, points)
  !error = gdk_colormap_alloc_color() !not in gdk-auto.f90
  !gbool = gdk_color_parse("red"//CNULL, colorptr) !colorptr have to be allocated before
  color%red = 65535
  color%green = 0
  color%blue = 0
  colorptr = c_loc(color)
  call gtk_plot_data_set_symbol(dataset,GTK_PLOT_SYMBOL_DIAMOND,GTK_PLOT_SYMBOL_EMPTY,10, 2.0, colorptr, colorptr)
  call gtk_plot_data_set_line_attributes(dataset,GTK_PLOT_LINE_SOLID,0,0,2.0, colorptr)
  call gtk_plot_data_set_connector(dataset, GTK_PLOT_CONNECT_STRAIGHT)
  call gtk_plot_data_hide_legend(dataset)
  call gtk_plot_add_data(w_plot, dataset)

  ! Show widgets and put the into vbox container
  call gtk_widget_show(dataset)
  call gtk_widget_show(w_plot)
!  call g_signal_connect(w_plot_canvas, "expose-event",G_CALLBACK(&plot_expose_event), NULL);
  call gtk_box_pack_start(w_vbox, w_plot_canvas, TRUE, TRUE, 0)
  
  ! Add OK button
  w_button = gtk_button_new_with_label("OK"//CNULL)
  call gtk_box_pack_start(w_vbox, w_button, FALSE, FALSE, 0)
  call g_signal_connect(w_button,"clicked"//CNULL,c_funloc(delete_event))

  call gtk_widget_show_all (my_window)
        
  boolresult = g_timeout_add(500,c_funloc(cb_change_dataset),NULL)

  call gtk_main()

  print *, "All done"

end program gtkextra_plot_demo
