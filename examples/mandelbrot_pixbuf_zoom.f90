! Copyright (C) 2011
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran gtk+ Fortran Interface library.
!
! This is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3, or (at your option)
! any later version.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! Under Section 7 of GPL version 3, you are granted additional
! permissions described in the GCC Runtime Library Exception, version
! 3.1, as published by the Free Software Foundation.
!
! You should have received a copy of the GNU General Public License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!------------------------------------------------------------------------------
! Contributed by Jerry DeLisle and Vincent Magnin
! Event handling & Zoom : James Tappin
! Last modifications: vmagnin 2020-12-18, 2021-01-22
!------------------------------------------------------------------------------

module handlers
  use cairo, only: cairo_paint
  use gdk, only: gdk_cairo_set_source_pixbuf
  use gdk_pixbuf, only: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, &
       & gdk_pixbuf_get_rowstride, gdk_pixbuf_new
  use gtk, only: gtk_application_window_new, gtk_application_new, &
       & gtk_window_set_default_size, gtk_box_new, &
       & gtk_window_set_child, gtk_box_append, gtk_drawing_area_new,  &
       & gtk_label_new, gtk_label_set_text, &
       & gtk_statusbar_new, gtk_statusbar_push, &
       & gtk_widget_queue_draw, &
       & gtk_drawing_area_set_draw_func, &
       & gtk_widget_set_size_request, gtk_widget_show, gtk_window_new, &
       & gtk_window_set_title, gtk_init, g_signal_connect, TRUE, FALSE, &
       & GTK_ORIENTATION_VERTICAL, GDK_COLORSPACE_RGB, &
       & gtk_gesture_click_new, gtk_widget_add_controller, &
       & gtk_event_controller_get_widget, gtk_event_controller_motion_new, &
       & gtk_gesture_single_get_current_button, &
       & gtk_gesture_single_set_button, gtk_event_controller_scroll_new, &
       & GTK_EVENT_CONTROLLER_SCROLL_VERTICAL
  use g, only: g_main_context_iteration, &
             & g_main_context_pending, &
             & g_application_run, g_object_unref
  use, intrinsic :: iso_c_binding

  implicit none
  integer(c_int) :: run_status = TRUE
  integer(c_int) :: boolresult
  type(c_ptr) :: my_pixbuf, status_bar, rangeid
  character(kind=c_char), dimension(:,:,:), pointer :: pixel
  integer(kind=c_int) :: nch, rowstride, width, height
  logical :: need_point     ! even/odd point flag
  logical :: computing_flag
  character(len=120) :: rangestr
  real(kind=c_double) :: mxmin, mxmax, mymin, mymax
  integer(kind=c_int) :: mouse_x, mouse_y

contains
  ! User defined event handlers go here

  ! This function is needed to update the GUI during long computations.
  ! https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html
  subroutine pending_events ()
    do while(IAND(g_main_context_pending(c_null_ptr), run_status) /= FALSE)
      ! FALSE for non-blocking:
      boolresult = g_main_context_iteration(c_null_ptr, FALSE)
    end do
  end subroutine pending_events

  ! "It is called whenever GTK needs to draw the contents of the drawing area
  ! to the screen."
  ! https://developer.gnome.org/gtk4/stable/GtkDrawingArea.html#gtk-drawing-area-set-draw-func
  subroutine draw(widget, my_cairo_context, width, height, gdata) bind(c)
    type(c_ptr), value, intent(in)    :: widget, my_cairo_context, gdata
    integer(c_int), value, intent(in) :: width, height

    ! We redraw the pixbuf:
    call gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 0d0, 0d0)
    call cairo_paint(my_cairo_context)
  end subroutine draw

  ! Click callback function ("pressed" signal):
  subroutine click_cb(gesture, n_press, x, y, gdata) bind(c)
    type(c_ptr), value, intent(in)    :: gesture, gdata
    integer(c_int), value, intent(in) :: n_press
    real(c_double), value, intent(in) :: x, y
    real(kind=c_double), save :: x0, y0
    real(kind=c_double) :: x1, y1
    type(c_ptr) :: drawing_area
    integer(kind=c_int) :: id

    print *, "Button ", gtk_gesture_single_get_current_button(gesture)
    print *, n_press, " click(s) at ", int(x), int(y)

    if (n_press > 1) then
      print *, "Double clicks have no effect!"
      return
    end if

    if (computing_flag) then
      print *, "Be patient..."
      return
    end if

    drawing_area = gtk_event_controller_get_widget(gesture)

    if (gtk_gesture_single_get_current_button(gesture) == 3) then
       ! Right button => reset to full set
       need_point=.false.
       call set_limits
       call mandelbrot_set(drawing_area, 1000_c_int)
       id = gtk_statusbar_push(status_bar, 0_c_int, &
            & "Left|Centre mark: region corner, Right: Reset, "//&
            & "Wheel: Zoom in/out"//c_null_char)
    else if (.not. need_point) then
       need_point=.true.
       print *, "First point"
       call mand_xy(int(x, c_int), int(y, c_int), x0, y0)
       id = gtk_statusbar_push(status_bar, 0_c_int, &
            & "Click the opposite corner of the region"//c_null_char)
    else if (need_point) then ! Already have one point
       need_point=.false.
       print *, "Second point"

       call mand_xy(int(x, c_int), int(y, c_int), x1, y1)
       ! If it is not a double-click, we zoom:
       if ((min(x0,x1)/=max(x0,x1)).and.(min(y0,y1)/=max(y0,y1))) then
         mxmin=min(x0,x1)
         mxmax=max(x0,x1)
         mymin=min(y0,y1)
         mymax=max(y0,y1)
       end if

       write(rangestr, &
            & "('Xmin: ',g11.4,' Xmax: ',g11.4,' Range: ',g11.4,' Ymin: '"//&
            & ",g11.4,' Ymax: ', g11.4,' Range: ',g11.4)") &
            & mxmin, mxmax, mxmax-mxmin, mymin, mymax, mymax-mymin
       call gtk_label_set_text(rangeid, trim(rangestr)//c_null_char)

       call mandelbrot_set(drawing_area, 1000_c_int)
       id = gtk_statusbar_push(status_bar, 0_c_int, &
            & "Left|Centre: mark region corner, "//&
            & "Right: Reset, Wheel: Zoom in/out"//c_null_char)
    end if
  end subroutine click_cb

  ! Scroll callback function ("scroll" signal):
  recursive subroutine scroll_cb(controller, x, y, gdata) bind(c)
    type(c_ptr), value, intent(in)    :: controller, gdata
    real(c_double), value, intent(in) :: x, y
    type(c_ptr) :: drawing_area
    ! Mathematical coordinates:
    real(kind=c_double) :: xr, yr, xx, yy
    integer(kind=c_int) :: id

    print *, "Scroll x,y= ", x, y
    ! We need to redraw the area:
    drawing_area = gtk_event_controller_get_widget(controller)

    if (computing_flag) return             ! One wheel step at a time !

    if (y > 0) then ! Zoom out
       call mand_xy(mouse_x, mouse_y, xx, yy)
       xr=min(mxmax-mxmin, 1.5_c_double)
       yr=min(mymax-mymin, 1.5_c_double)
    else ! Zoom in
       call mand_xy(mouse_x, mouse_y, xx, yy)
       xr = (mxmax-mxmin)/4._c_double
       yr = (mymax-mymin)/4._c_double
    end if

    mxmax=xx+xr
    mxmin=xx-xr
    if (mxmax > 1._c_double) then
       mxmax = 1._c_double
       mxmin = mxmax - 2._c_double * xr
    else if (mxmin < -2._c_double) then
       mxmin = -2._c_double
       mxmax = mxmin + 2._c_double * xr
    end if

    mymin = yy-yr
    mymax = yy+yr
    if (mymax > 1.5_c_double) then
       mymax = 1.5_c_double
       mymin = mymax - 2._c_double * yr
    else if (mymin < -1.5_c_double) then
       mymin = -1.5_c_double
       mymax = mymin + 2._c_double * yr
    end if

    write(rangestr, &
         & "('Xmin: ',g11.4,' Xmax: ',g11.4,' Range: ',g11.4,' Ymin: ',"//&
         & "g11.4,' Ymax: ', g11.4,' Range: ',g11.4)") &
         & mxmin, mxmax, mxmax-mxmin, mymin, mymax, mymax-mymin
    call gtk_label_set_text(rangeid, trim(rangestr)//c_null_char)
    print *, "Window:", mxmin,mxmax,mymin, mymax

    call mandelbrot_set(drawing_area, 1000_c_int)

    id = gtk_statusbar_push(status_bar, 0_c_int, &
         & "Left|Centre: mark region corner, "//&
         & "Right: Reset, Wheel: Zoom in/out"//c_null_char)
  end subroutine scroll_cb

  ! Motion callback function ("motion" signal):
  subroutine motion_cb(controller, x, y, gdata) bind(c)
    type(c_ptr), value, intent(in)    :: controller, gdata
    real(c_double), value, intent(in) :: x, y

    print *, "Motion x,y= ", x, y
    mouse_x = nint(x)
    mouse_y = nint(y)
  end subroutine motion_cb

  subroutine set_limits()
    mxmin = -2.0_c_double
    mxmax = 1.0_c_double
    mymin = -1.5_c_double
    mymax = 1.5_c_double
  end subroutine set_limits

  subroutine mand_xy(ix, iy, x, y)
    integer(kind=c_int), intent(in) :: ix, iy
    real(kind=c_double), intent(out) :: x, y
    real(kind=c_double) :: scx, scy

    scx = (mxmax-mxmin)/real(width, c_double)   ! x scale
    scy = (mymax-mymin)/real(height, c_double)  ! y scale

    x = mxmin + scx * real(ix, c_double)
    y = mymin + scy * real(iy, c_double)
  end subroutine mand_xy

  !*********************************************
  ! A tribute to Benoit MANDELBROT (1924-2010)
  ! http://en.wikipedia.org/wiki/Mandelbrot_set
  !*********************************************
  subroutine mandelbrot_set(my_drawing_area, itermax)
    ! Whole set: xmin=-2d0, xmax=+1d0, ymin=-1.5d0, ymax=+1.5d0, itermax=1000
    ! Seahorse valley:  around x=-0.743643887037151, y=+0.13182590420533, itermax=5000
    type(c_ptr), intent(in) :: my_drawing_area
    integer(kind=c_int), intent(in) :: itermax
    integer(kind=c_int)    :: i, j, k
    real(kind=c_double)    :: x, y ! coordinates in the complex plane
    complex(kind=c_double) :: c, z
    integer(kind=c_int8_t) :: red, green, blue     ! rgb color
    real(kind=c_double)    :: t0, t1
    integer :: it

    computing_flag = .true.

    call system_clock(it)
    t0=real(it, c_double)/1000._c_double

    do i=0, width-1
       ! **************************************************************************
       ! Needed if you want to display progressively the result during computation.
       ! We provoke a draw event only once in a while to avoid degrading
       ! the performances:
       ! **************************************************************************
       if (mod(i, 10_c_int) == 0) then
         call gtk_widget_queue_draw(my_drawing_area)
       end if

       do j=0, height-1
          call mand_xy(i, j, x, y)
          c = x + y*(0.0_c_double,1.0_c_double)   ! Starting point
          z = (0.0_c_double, 0.0_c_double)        ! z0
          k = 1
          do while ((k <= itermax) .and. ((real(z)**2+aimag(z)**2)<4.0_c_double))
             z = z*z+c
             k = k+1
          end do

          if (k>itermax) then
             ! Black pixel:
             red   = 0
             green = 0
             blue  = 0
          else
             red   = int(min(255, k*2),  KIND=c_int8_t)
             green = int(min(255, k*5),  KIND=c_int8_t)
             blue  = int(min(255, k*10), KIND=c_int8_t)
          end if

          ! We write in the pixbuffer:
          pixel(1,i+1,j+1)=char(red)
          pixel(2,i+1,j+1)=char(green)
          pixel(3,i+1,j+1)=char(blue)
          pixel(4,i+1,j+1)=char(255)  ! Opacity (alpha channel)

       end do
       ! **************************************************************************
       ! You need to manage the GTK events during computation:
       ! **************************************************************************
       call pending_events()
       if (run_status == FALSE) return ! Exit if we had a delete event.
    end do

    call gtk_widget_queue_draw(my_drawing_area)

    call system_clock(it)
    t1=real(it, c_double)/1000._c_double
    print *, "System time = ", t1-t0

    computing_flag = .false.
    need_point     = .false.
  end subroutine mandelbrot_set

  ! The GUI is defined here:
  subroutine activate(app, gdata) bind(c)
    implicit none
    type(c_ptr), value, intent(in)  :: app, gdata
    type(c_ptr) :: my_window, jb
    type(c_ptr) :: my_drawing_area, controller, controller2, controller3
    integer :: i, j
    integer(kind=c_int) :: id

    my_window = gtk_application_window_new(app)

    ! Properties of the main window :
    width = 700
    height = 700
    call gtk_window_set_default_size(my_window, width, height)
    call gtk_window_set_title(my_window, &
         & "A tribute to Benoit MANDELBROT (1924-2010)"//c_null_char)

    jb = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0_c_int)
    call gtk_window_set_child(my_window, jb)

    my_drawing_area = gtk_drawing_area_new()
    call gtk_widget_set_size_request(my_drawing_area, &
         & width, height)

    call gtk_drawing_area_set_draw_func(my_drawing_area, &
                     & c_funloc(draw), c_null_ptr, c_null_funptr)

    ! We need a gesture controller to detect mouse clicks:
    ! https://developer.gnome.org/gtk4/stable/GtkGestureClick.html
    ! https://developer.gnome.org/gtk4/stable/GtkWidget.html#gtk-widget-add-controller
    controller = gtk_gesture_click_new()
    ! 0 to listen to all buttons (button 1 by default):
    call gtk_gesture_single_set_button (controller, 0_c_int)
    call g_signal_connect(controller, "pressed"//c_null_char, &
                                        & c_funloc(click_cb))
    call gtk_widget_add_controller(my_drawing_area, controller)

    ! And a controller for scrolling:
    ! https://developer.gnome.org/gtk4/stable/GtkEventControllerScroll.html
    controller2 = gtk_event_controller_scroll_new (GTK_EVENT_CONTROLLER_SCROLL_VERTICAL)
    call g_signal_connect(controller2, "scroll"//c_null_char, &
                                        & c_funloc(scroll_cb))
    call gtk_widget_add_controller(my_drawing_area, controller2)
    ! And a controller to detect motion and know where is the mouse:
    ! https://developer.gnome.org/gtk4/stable/GtkEventControllerMotion.html
    controller3 = gtk_event_controller_motion_new ()
    call g_signal_connect(controller3, "motion"//c_null_char, &
                                        & c_funloc(motion_cb))
    call gtk_widget_add_controller(my_drawing_area, controller3)
    mouse_x = 0
    mouse_y = 0

    call gtk_box_append(jb, my_drawing_area)

    ! Set the initial view
    call set_limits()
    write(rangestr, &
         & "('Xmin: ',g11.4,' Xmax: ',g11.4,' Range: ',g11.4,' Ymin: ',g11.4,' Ymax: ', g11.4,' Range: ',g11.4)") &
         & mxmin,  mxmax, mxmax-mxmin, mymin, mymax, mymax-mymin
    rangeid = gtk_label_new(trim(rangestr)//c_null_char)
    call gtk_box_append(jb, rangeid)

    status_bar = gtk_statusbar_new()
    call gtk_box_append(jb, status_bar)
    id = gtk_statusbar_push(status_bar, 0_c_int, &
         & "Left|Centre: mark region corner, "//&
         & "Right: Reset, Wheel: Zoom in/out"//c_null_char)

    call gtk_widget_show(my_window)

    ! We create a pixbuffer to store the pixels of the image:
    my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8_c_int, width, height)
    nch = gdk_pixbuf_get_n_channels(my_pixbuf)
    call c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, &
         &int((/nch, width, height/)))
    rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)

    ! We use char() because we need unsigned integers.
    ! Our pixbuffer has an Alpha channel but is possible to create a pixbuffer
    ! with only Red, Green, Blue.
    do i=1, height
       do j=1, width
          pixel(1,j,i)=char(0)     ! Red
          pixel(2,j,i)=char(0)     ! Green
          pixel(3,j,i)=char(0)     ! Blue
          pixel(4,j,i)=char(255)   ! Opacity (Alpha channel)
       end do
    end do

    call Mandelbrot_set(my_drawing_area, 1000_c_int)

  end subroutine activate
end module handlers

! We create a GtkApplication:
program mandelbrot_pixbuf_zoom
  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_funloc, c_null_char, c_null_ptr
  use gtk, only: gtk_application_new, g_signal_connect, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  use handlers, only: activate

  implicit none
  integer(c_int) :: exit_status
  type(c_ptr)    :: app

  app = gtk_application_new("gtk-fortran.examples.mandelbrot-pixbuf-zoom"//c_null_char, &
                            & G_APPLICATION_FLAGS_NONE)
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), &
                      & c_null_ptr)
  exit_status = g_application_run(app, 0_c_int, [c_null_ptr])
  call g_object_unref(app)
end program mandelbrot_pixbuf_zoom

