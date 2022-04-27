! Copyright (C) 2011
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran GTK+ Fortran Interface library.
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
! -----------------------------------------------------------------------------
! Contributed by James Tappin
! Some code derived from a demo program by "tadeboro" posted on the gtk forums.
! Last modifications: 2013-01-31, vmagnin 2020-06-17 (GTK 4), 2022-03-29
! -----------------------------------------------------------------------------

!*
! gtk_draw_hl
module gtk_draw_hl
  ! This module provides a high-level drawing interface which automatically
  ! handles redrawing on exposure, and bundles the most likely events to
  ! be needed.
  !
  ! Note:
  !
  ! This module has undergone a major rewrite which has considerably
  ! streamlined the code. To the ordinary user, the most noticable difference
  ! is that the backing image is now a cairo image surface rather than a
  ! GDK pixbuf. When using PLplot, the "memcairo" device is not readily usable
  ! any more (extcairo is now the only supported driver), however
  ! cumulative plotting (e.g. PLplot's strip charts) now
  ! works correctly.
  !
  ! ### Routine List
  !
  ! * hl_gtk_drawing_area_new; Create the drawing area.
  ! * hl_gtk_drawing_area_get_surface; Get the backing cairo surface
  ! * hl_gtk_drawing_area_get_gdk_pixbuf; Get the contents to a GdkPixbuf
  ! * hl_gtk_drawing_area_expose_cb; Default callback for expose events.
  ! * hl_gtk_drawing_area_destroy_cb; Default callback for destroy signal
  ! * hl_gtk_drawing_area_resize_cb; Default callback for resize signal
  ! * hl_gtk_drawing_area_cairo_new; Create a cairo context attached to the
  ! backing surface.
  ! * hl_gtk_drawing_area_resize: Resize the drawing area and the backing
  ! surface
  ! * hl_gtk_drawing_area_cairo_destroy; Destroy the context.
  !/

  !********************************
  ! Gtk modules for gtk-draw-hl.f90
  !********************************
  use cairo, only: cairo_create, cairo_destroy, cairo_get_target, &
       & cairo_image_surface_create, cairo_image_surface_get_data, &
       & cairo_image_surface_get_format, cairo_image_surface_get_height, &
       & cairo_image_surface_get_stride, cairo_image_surface_get_width, &
       & cairo_paint, cairo_set_source_surface, cairo_surface_destroy, &
       & cairo_surface_get_reference_count, cairo_surface_get_type, &
       & cairo_surface_reference, cairo_surface_status, cairo_status_to_string

  use g, only: g_object_get_data, g_object_set_data

  use gdk, only: gdk_pixbuf_get_from_surface, gdk_cairo_set_source_pixbuf

  use gtk, only: gtk_drawing_area_new, gtk_scrolled_window_set_child, &
       & gtk_scrolled_window_new, gtk_scrolled_window_set_policy, &
       & gtk_widget_get_allocation, gtk_widget_set_can_focus, &
       & gtk_widget_set_size_request, gtk_widget_set_tooltip_text, &
       & g_signal_connect, gtk_widget_queue_draw, &
       & gtk_widget_get_realized, gtk_drawing_area_set_draw_func, &
       & TRUE, FALSE, &
       & CAIRO_FORMAT_ARGB32, &
       & CAIRO_FORMAT_RGB24, CAIRO_STATUS_SUCCESS, &
       & GTK_POLICY_AUTOMATIC, &
       & gtk_gesture_click_new, gtk_widget_add_controller, &
       & gtk_event_controller_motion_new, &
       & gtk_gesture_single_set_button, gtk_event_controller_scroll_new, &
       & gtk_event_controller_key_new, GTK_EVENT_CONTROLLER_SCROLL_VERTICAL, &
       & gtk_widget_set_focusable

  use gtk_sup

  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: error_unit

  implicit none
  type, bind(c) :: gtkallocation
     integer(c_int) :: x,y,width,height
  end type gtkallocation

contains

  !+
  function hl_gtk_drawing_area_new(scroll, size, ssize, expose_event, &
       & data_expose, button_press_event, data_button_press, &
       & button_release_event, data_button_release, scroll_event, &
       & data_scroll, motion_event, data_motion, realize, data_realize, &
       & configure_event, data_configure, key_press_event, data_key_press, &
       & key_release_event, data_key_release, enter_event, data_enter, &
       & leave_event, data_leave, destroy, data_destroy, &
       & tooltip, has_alpha, size_allocate, data_size_allocate, &
       & cairo_status, hscroll_policy, vscroll_policy) result(plota)

    type(c_ptr) :: plota
    type(c_ptr), intent(out), optional :: scroll
    integer(c_int), intent(in), optional, dimension(2) :: size, ssize
    type(c_funptr), optional :: expose_event, button_press_event, &
         & button_release_event, scroll_event, key_press_event, &
         & key_release_event, motion_event, realize, configure_event,&
         & enter_event, leave_event, destroy, size_allocate
    type(c_ptr), intent(in), optional :: data_expose, data_button_press, &
         & data_button_release, data_scroll, data_motion, data_realize, &
         & data_configure, data_key_press, data_key_release, data_enter, &
         & data_leave, data_destroy, data_size_allocate
    character(kind=c_char), dimension(*), optional, intent(in) :: tooltip
    integer(c_int), intent(in), optional :: has_alpha
    integer(c_int), intent(out), optional :: cairo_status
    integer(c_int), intent(in), optional :: hscroll_policy, vscroll_policy

    ! A high-level drawing area
    !
    ! SCROLL |  c_ptr |  optional |  If present, then the drawing area will be
    ! 		placed in a scrollable window, whose pointer will be returned
    ! 		here. If it is present, then it rather than the drawable should
    ! 		be used for packing.
    ! SIZE |  c_int() |  optional |  The requested size for the area. If no size is
    ! 		given then a default size of 256x256 is used.
    ! SSIZE |  c_int() : |  optional |  The requested size for a scrolling window
    ! EXPOSE_EVENT |  c_funptr |  optional |  Callback for expose-event signal
    ! 		N.B. In GTK3 the signal is called "draw". If this is not given
    ! 		then a default handler is provided which copies the image
    ! 		surface to the drawing area.
    ! DATA_EXPOSE |  c_ptr |  optional |  Data for expose_event callback
    ! BUTTON_PRESS_EVENT |  c_funptr |  optional |  Callback for button-press-event
    ! 		signal
    ! DATA_BUTTON_PRESS |  c_ptr |  optional |  Data for button_press_event callback
    ! BUTTON_RELEASE_EVENT |  c_funptr |  optional |  Callback for
    ! 		button-release-event signal
    ! DATA_BUTTON_RELEASE |  c_ptr |  optional |  Data for button_release_event
    ! 		callback
    ! SCROLL_EVENT |  c_funptr |  optional |  Callback for scroll-event signal
    ! DATA_SCROLL |  c_ptr |  optional |  Data for scroll_event callback
    ! REALIZE |  c_funptr |  optional |  Callback for realize signal
    ! DATA_REALIZE |  c_ptr |  optional |  Data for realize callback
    ! CONFIGURE_EVENT | c_funptr |  optional |  Callback for configure-event signal
    ! DATA_CONFIGURE |  c_ptr |  optional |  Data for configure_event callback
    ! KEY_PRESS_EVENT |  c_funptr |  optional |  Callback for key-press-event
    ! 		signal
    ! DATA_KEY_PRESS |  c_ptr |  optional |  Data for key_press_event callback
    ! KEY_RELEASE_EVENT |  c_funptr |  optional |  Callback for
    ! 		key-release-event signal
    ! DATA_KEY_RELEASE |  c_ptr |  optional |  Data for key_release_event
    ! 		callback
    ! MOTION_EVENT |  c_funptr |  optional |  Callback for the motion-notify-event
    ! 		signal
    ! DATA_MOTION |  c_ptr |  optional |  Data for motion_event
    ! ENTER_EVENT |  c_funptr |  optional |  Callback for the enter-notify-event
    ! 		signal
    ! DATA_ENTER |  c_ptr |  optional |  Data for enter_event
    ! LEAVE_EVENT |  c_funptr |  optional |  Callback for the leave-notify-event
    ! 		signal
    ! DATA_LEAVE |  c_ptr |  optional |  Data for leave_event
    ! DESTROY |  c_funptr |  optional |  Callback when the widget is destroyed.
    ! DATA_DESTROY |  c_ptr |  optional |  Data to pass to the destroy callback.
    ! TOOLTIP |  string |  optional |  Tooltip for the drawing area.
    ! HAS_ALPHA |  boolean |  optional |  If a pixbuf is used, should it have
    ! 		an alpha (transparency) channel (default=FALSE)
    ! SIZE_ALLOCATE |  c_funptr |  optional |  Callback for the
    ! 		'size-allocate' ('resize' in GTK 4) signal
    ! DATA_SIZE_ALLOCATE |  c_ptr |  optional |  Data for size_allocate.
    ! CAIRO_STATUS |  c_int |  optional |  Status code from the cairo surface.
    ! HSCROLL_POLICY |  int |  optional |  Horizontal scrolling policy for the
    ! 		containing scroll window (default AUTOMATIC).
    ! VSCROLL_POLICY |  int |  optional |  Vertical scrolling policy for the
    ! 		containing scroll window (default AUTOMATIC).
    !
    ! * If an explicit size is given then the drawing area cannot be made
    ! smaller than that by resizing the containing window
    !-

    type(c_ptr) :: isurface, cstat_cstr
    integer(c_int) :: s_type
    integer(c_int) :: szx, szy
    logical :: rgba
    integer(c_int) :: cstat, hpolicy, vpolicy
    character(len=120) :: cstat_fstr
    ! GtkEventControllers:
    type(c_ptr) :: controller_m, controller_s, controller_c, controller_k

    plota = gtk_drawing_area_new()
    if (present(size)) then
       call gtk_widget_set_size_request(plota, size(1), size(2))
       szx = size(1)
       szy = size(2)
    else
       call gtk_widget_set_size_request(plota, -1_c_int, -1_c_int)
       szx = 1   ! Negative values give a cairo error
       szy = 1
    end if

    ! Add it to a scrolling window if one is requested
    if (present(scroll)) then
       scroll = gtk_scrolled_window_new()
       if (present(hscroll_policy)) then
          hpolicy = hscroll_policy
       else
          hpolicy = GTK_POLICY_AUTOMATIC
       end if
       if (present(vscroll_policy)) then
          vpolicy = vscroll_policy
       else
          vpolicy = GTK_POLICY_AUTOMATIC
       end if
       call gtk_scrolled_window_set_policy(scroll, hpolicy, vpolicy)
       if (present(ssize)) &
            & call gtk_widget_set_size_request(scroll, ssize(1), ssize(2))
       call gtk_scrolled_window_set_child(scroll, plota)
    end if

    ! Create the backing surface
    if (present(has_alpha)) then
       rgba = c_f_logical(has_alpha)
    else
       rgba = .false.
    end if

    if (rgba) then
       s_type = CAIRO_FORMAT_ARGB32
    else
       s_type = CAIRO_FORMAT_RGB24
    end if
    isurface = cairo_image_surface_create(s_type, szx, szy)
    isurface = cairo_surface_reference(isurface)   ! Prevent accidental deletion
    call g_object_set_data(plota, "backing-surface"//c_null_char, isurface)

    ! Realize signal
    if (present(realize)) then
       if (present(data_realize)) then
          call g_signal_connect(plota, "realize"//c_null_char, realize, &
               & data_realize)
       else
          call g_signal_connect(plota, "realize"//c_null_char, realize)
       endif
    end if

    ! Destroy signal
    if (present(destroy)) then
       if (present(data_destroy)) then
          call g_signal_connect(plota, "destroy"//c_null_char, destroy, &
               & data_destroy)
       else
          call g_signal_connect(plota, "destroy"//c_null_char, destroy)
       end if
    else
       call g_signal_connect(plota, "destroy"//c_null_char, &
            & c_funloc(hl_gtk_drawing_area_destroy_cb))
    end if

    ! Size-allocate (resize) signal
    if (present(size_allocate)) then
       if (present(data_size_allocate)) then
          call g_signal_connect(plota, "resize"//c_null_char, &
               & size_allocate, data_size_allocate)
       else
          call g_signal_connect(plota, "resize"//c_null_char, &
               & size_allocate)
       end if
    else
       call g_signal_connect(plota, "resize"//c_null_char, &
            & c_funloc(hl_gtk_drawing_area_resize_cb))
    end if

    ! Expose event (GTK 4):
    if (present(expose_event)) then
       if (present(data_expose)) then
         call gtk_drawing_area_set_draw_func(plota, &
                           & expose_event, data_expose, c_null_funptr)
       else
         call gtk_drawing_area_set_draw_func(plota, &
                           & expose_event, c_null_ptr, c_null_funptr)
       endif
    else
      call gtk_drawing_area_set_draw_func(plota, &
         & c_funloc(hl_gtk_drawing_area_expose_cb), c_null_ptr, c_null_funptr)
    end if

    ! We need a gesture controller to detect mouse clicks:
    ! https://developer.gnome.org/gtk4/stable/GtkGestureClick.html
    ! https://developer.gnome.org/gtk4/stable/GtkWidget.html#gtk-widget-add-controller
    if (present(button_press_event).OR.present(button_release_event)) then
       controller_c = gtk_gesture_click_new()
       ! 0 to listen to all buttons (button 1 by default):
       call gtk_gesture_single_set_button (controller_c, 0_c_int)

       if (present(button_press_event)) then
         if (present(data_button_press)) then
           call g_signal_connect(controller_c, "pressed"//c_null_char, &
                               & button_press_event, data_button_press)
         else
           call g_signal_connect(controller_c, "pressed"//c_null_char, &
                               & button_press_event)
         endif
       end if

       if (present(button_release_event)) then
         if (present(data_button_release)) then
           call g_signal_connect(controller_c, "released"//c_null_char, &
                               & button_release_event, data_button_release)
         else
           call g_signal_connect(controller_c, "released"//c_null_char, &
                               & button_release_event)
         endif
       end if

       call gtk_widget_add_controller(plota, controller_c)
    end if

    ! And a controller for scrolling:
    ! https://developer.gnome.org/gtk4/stable/GtkEventControllerScroll.html
    if (present(scroll_event)) then
       controller_s = gtk_event_controller_scroll_new (GTK_EVENT_CONTROLLER_SCROLL_VERTICAL)
       if (present(data_scroll)) then
         call g_signal_connect(controller_s, "scroll"//c_null_char, &
                             & scroll_event, data_scroll)
       else
         call g_signal_connect(controller_s, "scroll"//c_null_char, &
                             & scroll_event)
       endif
       call gtk_widget_add_controller(plota, controller_s)
    end if

    ! And a controller for key events:
    ! https://developer.gnome.org/gtk4/stable/GtkEventControllerKey.html
    if (present(key_press_event).or.present(key_release_event)) then
      controller_k = gtk_event_controller_key_new()
      call g_signal_connect(controller_k, "im-update"//c_null_char, &
               & c_funloc(im_update_event_h))

      if (present(key_press_event)) then
        if (present(data_key_press)) then
          call g_signal_connect(controller_k, "key-pressed"//c_null_char, &
               & key_press_event, data_key_press)
        else
          call g_signal_connect(controller_k, "key-pressed"//c_null_char, &
               & key_press_event)
        endif
      end if

      if (present(key_release_event)) then
        if (present(data_key_release)) then
          call g_signal_connect(controller_k, "key-released"//c_null_char, &
               & key_press_event, data_key_release)
        else
          call g_signal_connect(controller_k, "key-released"//c_null_char, &
               & key_press_event)
        endif
      endif

      call gtk_widget_add_controller(plota, controller_k)
      ! Note: For keyboard events, the drawing area must be able to
      ! accept input focus:
      call gtk_widget_set_focusable(plota, TRUE)
    end if

    ! And a controller to detect motion and know where is the mouse:
    ! https://developer.gnome.org/gtk4/stable/GtkEventControllerMotion.html
    if (present(motion_event).or.present(enter_event).or.present(leave_event)) then
      controller_m = gtk_event_controller_motion_new ()

      if (present(data_motion)) then
        call g_signal_connect(controller_m, "motion"//c_null_char, &
                            & motion_event, data_motion)
      else
        call g_signal_connect(controller_m, "motion"//c_null_char, &
                            & motion_event)
      endif

      ! Enter event
      if (present(data_enter)) then
        call g_signal_connect(controller_m, "enter"//c_null_char, &
             & enter_event, data_enter)
      else
        call g_signal_connect(controller_m, "enter"//c_null_char, &
             & enter_event)
      endif

      ! Leave event
      if (present(data_leave)) then
        call g_signal_connect(controller_m, "leave"//c_null_char, &
             & leave_event, data_leave)
      else
        call g_signal_connect(controller_m, "leave"//c_null_char, &
             & leave_event)
      endif

      call gtk_widget_add_controller(plota, controller_m)
    end if

    ! Configure event
    if (present(configure_event)) then
       if (present(data_configure)) then
          call g_signal_connect(plota, "configure-event"//c_null_char, &
               & configure_event,  data_configure)
       else
          call g_signal_connect(plota, "configure-event"//c_null_char, &
               & configure_event)
       endif
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(plota, tooltip)

    ! If we fail to create the cairo surface we need to warn the user.
    ! Cairo does not generate error messages, just a propagated error state.

    if (present(cairo_status)) then
       cairo_status = cairo_surface_status(isurface)
    else
       cstat = cairo_surface_status(isurface)
       if (cstat /= CAIRO_STATUS_SUCCESS) then
          cstat_cstr = cairo_status_to_string(cstat)
          call c_f_string(cstat_cstr, cstat_fstr)
          write(error_unit, "(A)") &
               & "HL_GTK_DRAWING_AREA_NEW: Error creating backing store", &
               & trim(cstat_fstr)
       end if
    end if
  end function hl_gtk_drawing_area_new

  subroutine im_update_event_h(controller, gdata) bind(c)
    type(c_ptr), value, intent(in)    :: controller, gdata
    print *, "im_update event detected"
  end subroutine im_update_event_h

  !+
  function hl_gtk_drawing_area_get_surface(area) result(isurface)
    type(c_ptr) :: isurface
    type(c_ptr), intent(in) :: area

    ! Convenience routine to get the backing surface of a drawing area.
    !
    ! AREA |  c_ptr |  required |  The drawing area whose surface is required.
    !-

    isurface = g_object_get_data(area, "backing-surface"//c_null_char)

  end function hl_gtk_drawing_area_get_surface

  !+
  function hl_gtk_drawing_area_get_gdk_pixbuf(area, x0, y0, xsize, ysize) &
       & result(pixb)
    type(c_ptr) :: pixb
    type(c_ptr), intent(in) :: area
    integer(c_int), intent(in), optional :: x0, y0, xsize, ysize

    ! Read a drawing area (strictly the cairo surface
    ! backing store) to a GDK pixbuf.
    !
    ! AREA |  c_ptr |  required |  The drawing area
    ! X0, Y0 |  c_int |  optional |  The origin of the area to return (defaults 0)
    ! XSIZE, YSIZE |  c_int |  optional |  The size of the region to return
    ! 		(defaults, from the origin to the top right of the surface).
    !-

    type(c_ptr) :: surface
    integer(c_int) :: nx, ny, rs, fmt, lpix
    integer(c_int) :: xorig, yorig, xrange, yrange

    if (present(x0)) then
       xorig = max(x0, 0)
    else
       xorig = 0
    end if
    if (present(y0)) then
       yorig = max(y0, 0)
    else
       yorig = 0
    end if
    surface = hl_gtk_drawing_area_get_surface(area)
    nx = cairo_image_surface_get_width(surface)
    ny = cairo_image_surface_get_height(surface)
    rs = cairo_image_surface_get_stride(surface)
    fmt = cairo_image_surface_get_format(surface)
    lpix = rs*(ny-1)+nx*4

    if (present(xsize)) then
       xrange = min(xsize, nx - xorig)
    else
       xrange = nx - xorig
    end if
    if (present(ysize)) then
       yrange = min(ysize, ny - yorig)
    else
       yrange = ny - yorig
    end if

    pixb = gdk_pixbuf_get_from_surface(surface, xorig, yorig, &
         & xrange, yrange)

  end function hl_gtk_drawing_area_get_gdk_pixbuf

  !+
  subroutine hl_gtk_drawing_area_draw_pixbuf(area, pixbuf, x, y)
    type(c_ptr), intent(in) :: area, pixbuf
    integer(c_int), intent(in), optional :: x, y

    ! Render a GdkPixbuf on a drawing area
    !
    ! AREA |  c_ptr |  required |  The drawing area.
    ! PIXBUF |  c_ptr |  required |  The pixbuf to draw.
    ! X, Y |  c_int |  optional |  The coordinate of the upper left corner of the
    ! 		pixbuf on the drawing area (defaults 0).
    !
    ! If you are rendering a pixbuf among other operations then just use
    ! gdk_cairo_set_source_pixbuf directly on the context with which you
    ! are working.
    !-

    type(c_ptr) :: cc
    real(c_double) :: xx, yy

    if (present(x)) then
       xx = real(x,c_double)
    else
       xx = 0._c_double
    end if
    if (present(y)) then
       yy = real(y, c_double)
    else
      yy = 0._c_double
    end if

    cc = hl_gtk_drawing_area_cairo_new(area)

    call gdk_cairo_set_source_pixbuf(cc, pixbuf, xx, yy)
    call cairo_paint(cc)

    call gtk_widget_queue_draw(area)
    call hl_gtk_drawing_area_cairo_destroy(cc)

  end subroutine hl_gtk_drawing_area_draw_pixbuf

  !+
  function hl_gtk_drawing_area_expose_cb(area, event, data) bind(c) &
       & result(rv)
    integer(c_int) :: rv
    type(c_ptr), value :: area, event, data

    ! Default callback for exposing a drawing area. For this to be connected
    ! no explicit expose callback should be specified.
    !
    ! AREA |  c_ptr |  required |  The drawing area
    ! EVENT |  c_ptr |  required |  GTK2 = event structure, GTK3 = a cairo context
    ! DATA |  c_ptr |  required |  A pointer to user data (not used).
    !-

    type(c_ptr) :: cr, isurface

    rv = FALSE

    isurface = g_object_get_data(area, "backing-surface"//c_null_char)
    if (.not. c_associated(isurface)) then
       write(error_unit,*) &
            & 'hl_gtk_drawing_area_expose_cb: Backing surface is NULL'
       return
    end if

    ! Note for plplot users, this cairo context is a different one from
    ! the context used by plplot for the actual drawing.

    cr = event
    call cairo_set_source_surface(cr, isurface, 0._c_double, 0._c_double)
    call cairo_paint(cr)

  end function hl_gtk_drawing_area_expose_cb

  !+
  subroutine hl_gtk_drawing_area_destroy_cb(area, data) bind(c)
    type(c_ptr), value :: area, data

    ! Default callback for the destroy signal on the drawing area.
    ! Just destroys the backing surface.
    !
    ! AREA |  c_ptr |  required |  The drawing area being destroyed.
    ! DATA |  c_ptr |  required |  User data for the callback (not used)
    !-

    type(c_ptr) :: isurface

    isurface = g_object_get_data(area, "backing-surface"//c_null_char)
    if (c_associated(isurface)) call cairo_surface_destroy(isurface)

  end subroutine hl_gtk_drawing_area_destroy_cb

  !+
  subroutine hl_gtk_drawing_area_resize_cb(area, data) bind(c)
    type(c_ptr), value :: area, data

    ! Default call back for resizing the drawing area, just
    ! copies the old backing store to the new one
    !
    ! AREA |  c_ptr |  required |  The drawing area being destroyed.
    ! DATA |  c_ptr |  required |  User data for the callback (not used)
    !-

    call hl_gtk_drawing_area_resize(area, copy=.true.)
  end subroutine hl_gtk_drawing_area_resize_cb

  !+
  function hl_gtk_drawing_area_cairo_new(area) result(cr)
    type(c_ptr) :: cr
    type(c_ptr), intent(in) :: area

    ! Create a cairo context which will draw into the backing surface
    !
    ! AREA |  c_ptr |  required |  The drawing area to which we will draw.
    !
    ! After the drawing operations, you should call `gtk_widget_queue_draw`
    ! to update the plot on the screen and `hl_gtk_pixbuf_cairo_destroy`
    ! to destroy the cairo context.
    !-

    type(c_ptr) :: isurface

    isurface = g_object_get_data(area, "backing-surface"//c_null_char)
    if (.not. c_associated(isurface)) then
       cr = C_NULL_PTR
       write(error_unit,*) "hl_gtk_pixbuf_cairo_new:: Backing surface is NULL"
       return
    end if

    cr = cairo_create(isurface)

  end function hl_gtk_drawing_area_cairo_new

  !+
  subroutine hl_gtk_drawing_area_cairo_destroy(cr, destroy_surface)

    type(c_ptr), intent(inout) :: cr
    integer(c_int), intent(in), optional :: destroy_surface

    ! Update the backing surface and destroy the cairo context
    !
    ! CR |  c_ptr |  required |  The cairo context to put in the pixbuf
    ! DESTROY_SURFACE |  boolean  |  optional |  Set to TRUE to destroy the
    ! 		cairo_surface as well as the context. Normally the cairo
    ! 		surface is destroyed by the destroy callback of the drawing
    ! 		area, so does not need to be explicitly destroyed.
    !
    ! This is called following drawing operations to the context created by
    ! `hl_gtk_drawing_area_cairo_new`. N.B. This does not update the window,
    ! use gtk_widget_queue_draw to do that.
    !-

    type(c_ptr) :: isurface

    if (present(destroy_surface)) then
       if (destroy_surface == TRUE) then
          ! Get the cairo surface and destroy it
          isurface = cairo_get_target(cr)
          call cairo_surface_destroy(isurface)
       end if
    end if

    call cairo_destroy(cr)

  end subroutine hl_gtk_drawing_area_cairo_destroy

  !+
  subroutine hl_gtk_drawing_area_resize(area, size, copy)
    type(c_ptr), intent(in) :: area
    integer(c_int), intent(in), optional, dimension(2) :: size
    logical, optional, intent(in) :: copy

    ! Resize a drawing area and its backing store.
    !
    ! AREA |  c_ptr |  required |  The area to resize.
    ! SIZE |  int(2)  |  optional |  The new size, if omitted, then the
    ! 		backing store is resized to match the drawing area (e.g.
    ! 		after resizing the containing window).
    ! COPY |  logical |  optional |  Set to .true. to copy the surface
    ! 		contents to the new backing store.
    !-

    type(c_ptr) :: cback, cback_old, cr
    integer(c_int) :: szx, szy, s_type
    type(gtkallocation), target:: alloc
    logical :: copy_surface

    if (present(copy)) then
       copy_surface = copy
    else
       copy_surface = .false.
    end if

    ! If the SIZE keyword is present then resize the window
    if (present(size)) then
       call gtk_widget_set_size_request(area, size(1), size(2))
       szx = size(1)
       szy = size(2)
    endif

    ! Ensure that the allocation is up-to-date
    call gtk_widget_get_allocation(area,c_loc(alloc))
    szx = alloc%width
    szy = alloc%height

    ! Get the backing store and make a new one with the right type. Then
    ! make that into the backing store.
    cback_old = g_object_get_data(area, "backing-surface"//c_null_char)
    s_type = cairo_image_surface_get_format(cback_old)
    cback = cairo_image_surface_create(s_type, szx, szy)
    cback = cairo_surface_reference(cback)   ! Prevent accidental deletion
    call g_object_set_data(area, "backing-surface"//c_null_char, cback)

    ! If the copy keyword is set then make a copy from the old
    ! backing store to the new if the gdk surface is really there.
    if (copy_surface) then
       if (c_f_logical(gtk_widget_get_realized(area))) then
          cr = cairo_create(cback)
          call cairo_set_source_surface(cr, cback_old, &
               & 0._c_double, 0._c_double)
          call cairo_paint(cr)
          call cairo_destroy(cr)
       end if
    end if

    ! Ensure that the old backing store is fully dereferenced.
    do
       if (cairo_surface_get_reference_count(cback_old) <= 0) exit
       call cairo_surface_destroy(cback_old)
    end do

  end subroutine hl_gtk_drawing_area_resize

  !+
  subroutine hl_gtk_drawing_area_get_size(area, width, height)
    type(c_ptr), intent(in) :: area
    integer(c_int), intent(out), optional :: width, height

    ! Convenience routine to get the current size of a drawing area
    !
    ! AREA |  c_ptr |  required |  The drawing area whose size is needed.
    ! WIDTH |  c_int |  optional |  The width of the area.
    ! HEIGHT |  c_int |  optional |  The height of the area
    !-

    type(gtkallocation), target :: alloc

    call gtk_widget_get_allocation(area,c_loc(alloc))
    if (present(width)) width = alloc%width
    if (present(height)) height = alloc%height

  end subroutine hl_gtk_drawing_area_get_size

end module gtk_draw_hl
