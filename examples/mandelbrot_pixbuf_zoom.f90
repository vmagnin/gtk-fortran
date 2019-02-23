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
! Contributed by Jerry DeLisle and Vincent Magnin
! Event handling & Zoom : James Tappin
! Last modification: vmagnin 02-23-2019
! gfortran -I../src ../src/gtk.f90 ../src/gdkevents-auto3.f90 mandelbrot_pixbuf_zoom.f90 `pkg-config --cflags --libs gtk+-3.0` -Wall -Wextra -pedantic -std=f2003 -g

module handlers
  use gdk_events, only: gdkeventbutton, gdkeventscroll

  use cairo, only: cairo_destroy, cairo_paint

  use gdk, only: gdk_cairo_create, gdk_cairo_set_source_pixbuf

  use gdk_pixbuf, only: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, &
       & gdk_pixbuf_get_rowstride, gdk_pixbuf_new

  use gtk, only: gtk_bin_get_child, gtk_box_new, gtk_box_pack_start, &
       & gtk_container_add, gtk_drawing_area_new, gtk_event_box_new, &
       & gtk_events_pending, gtk_label_new, gtk_label_set_text, &
       & gtk_main_iteration_do, gtk_statusbar_new, gtk_statusbar_push, &
       & gtk_widget_add_events, gtk_widget_get_window, gtk_widget_queue_draw, &
       & gtk_widget_set_size_request, gtk_widget_show_all, gtk_window_new, &
       & gtk_window_set_title, gtk_init, g_signal_connect, TRUE, FALSE, &
       & GDK_SCROLL_UP, GDK_SCROLL_DOWN, GDK_SHIFT_MASK, GDK_CONTROL_MASK, &
       & GDK_BUTTON_PRESS_MASK, GDK_SCROLL_MASK, GTK_ORIENTATION_VERTICAL, &
       & GTK_WINDOW_TOPLEVEL, GDK_COLORSPACE_RGB, gtk_main, gtk_main_quit

  use iso_c_binding

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

contains
  ! User defined event handlers go here
  function delete_event (widget, event, gdata) result(ret)  bind(c)
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata

    run_status = FALSE
    ret = FALSE
    call gtk_main_quit()
  end function delete_event


  subroutine pending_events ()
    do while(IAND(gtk_events_pending(), run_status) /= FALSE)
       boolresult = gtk_main_iteration_do(FALSE) ! False for non-blocking
    end do
  end subroutine pending_events


  subroutine expose_event (widget, event, gdata)  bind(c)
    type(c_ptr), value, intent(in) :: widget, event, gdata

    call paint_set(gtk_widget_get_window(widget))
  end subroutine expose_event


  subroutine paint_set(window)
    type(c_ptr), intent(in) :: window
    type(c_ptr) :: my_cairo_context

    my_cairo_context = gdk_cairo_create (window)
    call gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 0d0, 0d0)
    call cairo_paint(my_cairo_context)
    call cairo_destroy(my_cairo_context)
  end subroutine paint_set


  recursive subroutine mark_point(widget, event, gdata)  bind(c)
    type(c_ptr), value, intent(in) :: widget, event, gdata
    type(gdkeventbutton), pointer :: fevent
    real(kind=c_double), save :: x0, y0
    real(kind=c_double) :: x1, y1, xr, yr, dr
    type(c_ptr) :: drawing_area
    integer(kind=c_int) :: id

    if (.not. c_associated(event)) return  ! shouldn't happen
    if (computing_flag) return

    call c_f_pointer(event, fevent)
    drawing_area = gtk_bin_get_child(widget)

    if (fevent%button == 3) then ! Right button reset to full set
       need_point=.false.
       call set_limits
       call mandelbrot_set(drawing_area, 1000_c_int)
       call paint_set(gtk_widget_get_window(drawing_area))
       id = gtk_statusbar_push(status_bar, 0_c_int, &
            & "Left|Centre mark: region corner, Right: Reset, "//&
            & "Wheel: Zoom in/out"//c_null_char)
    else if (.not. need_point) then
       need_point=.true.
       print *, "First point"
       call mand_xy(int(fevent%x,c_int), int(fevent%y, c_int), x0, y0)
       id = gtk_statusbar_push(status_bar, 0_c_int, &
            & "Click the opposite corner of the region"//c_null_char)
    else if (need_point) then ! Already have one point
       need_point=.false.
       print *, "Second point"
       call mand_xy(int(fevent%x,c_int), int(fevent%y, c_int), x1, y1)
       mxmin=min(x0,x1)
       mxmax=max(x0,x1)
       mymin=min(y0,y1)
       mymax=max(y0,y1)

       if ((mxmin==mxmax).or.(mymin==mymax)) print *, "double-click"

       select case(fevent%state)
       case(GDK_SHIFT_MASK) ! Second point was shifted: square (bigger)
          xr=mxmax-mxmin
          yr=mymax-mymin
          if (xr > yr) then
             dr = xr-yr
             mymin=mymin-dr/2._c_double
             mymax=mymax+dr/2._c_double
          else if (yr > xr) then
             dr = yr-xr
             mxmin=mxmin-dr/2._c_double
             mxmax=mxmax+dr/2._c_double
          end if
       case(GDK_CONTROL_MASK) ! Second point was control: square (smaller)
          xr=mxmax-mxmin
          yr=mymax-mymin
          if (xr > yr) then
             dr = xr-yr
             mxmin=mxmin+dr/2._c_double
             mxmax=mxmax-dr/2._c_double
          else if (yr > xr) then
             dr = yr-xr
             mymin=mymin+dr/2._c_double
             mymax=mymax-dr/2._c_double
          end if
       case  default
       end select

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
  end subroutine mark_point

  ! Need to be recursive in case the wheel is turned many steps at a time
  recursive subroutine zoom_view(widget, event, gdata)  bind(c)
    type(c_ptr), value, intent(in) :: widget, event, gdata
    type(gdkeventscroll), pointer :: fevent
    type(c_ptr) :: drawing_area
    real(kind=c_double) :: xr, yr, x, y
    integer(kind=c_int) :: id

    if (.not. c_associated(event)) return  ! shouldn't happen
    if (computing_flag) return             ! One wheel step at a time !

    call c_f_pointer(event, fevent)

    select case (fevent%direction)
    case(GDK_SCROLL_UP) ! Zoom out
       call mand_xy(int(fevent%x,c_int), int(fevent%y, c_int), x, y)
       xr=min(mxmax-mxmin, 1.5_c_double)
       yr=min(mymax-mymin, 1.5_c_double)
    case (GDK_SCROLL_DOWN) ! Zoom in
       call mand_xy(int(fevent%x,c_int), int(fevent%y, c_int), x, y)
       xr = (mxmax-mxmin)/4._c_double
       yr = (mymax-mymin)/4._c_double
    case default ! Transverse scroll -- ignore
       return
    end select

    mxmax=x+xr
    mxmin=x-xr
    if (mxmax > 1._c_double) then
       mxmax = 1._c_double
       mxmin = mxmax - 2._c_double * xr
    else if (mxmin < -2._c_double) then
       mxmin = -2._c_double
       mxmax = mxmin + 2._c_double * xr
    end if
    mymin = y-yr
    mymax = y+yr
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

    drawing_area = gtk_bin_get_child(widget)

    call mandelbrot_set(drawing_area, 1000_c_int)

    id = gtk_statusbar_push(status_bar, 0_c_int, &
         & "Left|Centre: mark region corner, "//&
         & "Right: Reset, Wheel: Zoom in/out"//c_null_char)

  end subroutine zoom_view


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
       ! We provoke an expose_event once in a while to improve performances:
       if (mod(i,10_c_int)==0) then
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

          ! This subrountine processes gtk events as needed during the computation.
          call pending_events()
          if (run_status == FALSE) return ! Exit if we had a delete event.
       end do
    end do

    call gtk_widget_queue_draw(my_drawing_area)

    call system_clock(it)
    t1=real(it, c_double)/1000._c_double
    print *, "System time = ", t1-t0

    computing_flag = .false.
    need_point     = .false.
  end subroutine mandelbrot_set

end module handlers


program mandelbrot_zoom
  use iso_c_binding, only: c_ptr, c_funloc, c_f_pointer
  use handlers
  implicit none
  type(c_ptr) :: my_window, jb
  type(c_ptr) :: my_event_box, my_drawing_area
  integer :: i, j
  integer(kind=c_int) :: id
  integer(kind=c_int), parameter :: ev_mask = ior(GDK_BUTTON_PRESS_MASK, &
                                                & GDK_SCROLL_MASK)

  call gtk_init ()

  ! Properties of the main window :
  width = 700
  height = 700

  ! Set the initial view
  call set_limits()

  my_window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  call gtk_window_set_title(my_window, &
       & "A tribute to Benoit MANDELBROT (1924-2010)"//c_null_char)
  call g_signal_connect (my_window, "delete-event"//c_null_char, &
       & c_funloc(delete_event))

  jb = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0_c_int)
  call gtk_container_add(my_window, jb)

  my_event_box = gtk_event_box_new()
  call gtk_widget_add_events(my_event_box, ev_mask)
  my_drawing_area = gtk_drawing_area_new()
  call gtk_widget_set_size_request(my_drawing_area, &
       & width, height)
  call gtk_container_add(my_event_box, my_drawing_area)
  call g_signal_connect (my_drawing_area, "draw"//c_null_char, &
       & c_funloc(expose_event))
  call g_signal_connect(my_event_box, "button-press-event"//c_null_char, &
       & c_funloc(mark_point))
  call g_signal_connect(my_event_box, "scroll-event"//c_null_char, &
       & c_funloc(zoom_view))
  call gtk_box_pack_start(jb, my_event_box, FALSE, FALSE, 0_c_int)

  write(rangestr, &
       & "('Xmin: ',g11.4,' Xmax: ',g11.4,' Range: ',g11.4,' Ymin: ',g11.4,' Ymax: ', g11.4,' Range: ',g11.4)") &
       & mxmin,  mxmax, mxmax-mxmin, mymin, mymax, mymax-mymin
  rangeid = gtk_label_new(trim(rangestr)//c_null_char)
  call gtk_box_pack_start(jb, rangeid, FALSE, FALSE, 0_c_int)

  status_bar = gtk_statusbar_new()
  call gtk_box_pack_start(jb, status_bar, FALSE, FALSE, 0_c_int)
  id = gtk_statusbar_push(status_bar, 0_c_int, &
       & "Left|Centre: mark region corner, "//&
       & "Right: Reset, Wheel: Zoom in/out"//c_null_char)

  call gtk_widget_show_all (my_window)

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

  ! The window stays opened after the computation
  ! Main loop:
  call gtk_main()
  print *, "All done"

end program mandelbrot_zoom
