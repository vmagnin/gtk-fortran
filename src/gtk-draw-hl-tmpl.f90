! Copyright (C) 2011
! Free Software Foundation, Inc.

! This file is part of the gtk-fortran GTK+ Fortran Interface library.

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
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! Contributed by James Tappin
! Some code derived from a demo program by "tadeboro" posted on the gtk forums.
! Last modification: 01-31-2013

!!$T Template file for gtk-draw-hl.f90.
!!$T  Make edits to this file, and keep them identical between the
!!$T  GTK2 & GTK3 branches.

!!$T Lines to appear only in specific versions should be prefixed by
!!$T !!$<lib><op><ver>!
!!$T Where <lib> is GTK or GLIB, <op> is one of < > <= >=
!!$T and <ver> is the version boundary, e.g. !!$GTK<=2.24! to include
!!$T the line in GTK+ version 2.24 and higher. 
!!$T The mk_gtk_hl.pl script should be used to generate the source file.

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
  ! * hl_gtk_drawing_area_resize_cb; Default callback for size-allocate signal
  ! * hl_gtk_drawing_area_cairo_new; Create a cairo context attached to the
  ! backing surface.
  ! * hl_gtk_drawing_area_resize: Resize the drawing area and the backing
  ! surface
  ! * hl_gtk_drawing_area_cairo_destroy; Destroy the context.
  !/



  !********************************
  ! Gtk modules for gtk-draw-hl-tmpl.f90
  use cairo, only: cairo_create, cairo_destroy, cairo_get_target, &
       & cairo_image_surface_create, cairo_image_surface_get_data, &
       & cairo_image_surface_get_format, cairo_image_surface_get_height, &
       & cairo_image_surface_get_stride, cairo_image_surface_get_width, &
       & cairo_paint, cairo_set_source_surface, cairo_surface_destroy, &
       & cairo_surface_get_reference_count, cairo_surface_get_type, &
       & cairo_surface_reference, cairo_surface_status, cairo_status_to_string

  use g, only: g_object_get_data, g_object_set_data

!!$GTK< 3.0!  use gdk, only: gdk_cairo_create, gdk_cairo_set_source_pixbuf
!!$GTK>=3.0!  use gdk, only: gdk_cairo_create, gdk_pixbuf_get_from_surface, &
!!$GTK>=3.0!     & gdk_cairo_set_source_pixbuf

  use gdk_pixbuf, only: gdk_pixbuf_get_pixels, gdk_pixbuf_get_rowstride, &
       & gdk_pixbuf_new

  use gtk, only: gtk_drawing_area_new, &
       & gtk_scrolled_window_add_with_viewport, &
       & gtk_scrolled_window_new, gtk_scrolled_window_set_policy, &
       & gtk_widget_add_events, gtk_widget_get_allocation, &
       & gtk_widget_get_window, gtk_widget_set_can_focus, &
       & gtk_widget_set_size_request, gtk_widget_set_tooltip_text, &
       & g_signal_connect, gtk_widget_queue_draw, &
       & gtk_events_pending, gtk_main_iteration_do, &
       & TRUE, FALSE, &
       & CAIRO_FORMAT_ARGB32, &
       & CAIRO_FORMAT_RGB24, CAIRO_STATUS_SUCCESS, GDK_EXPOSURE_MASK, &
       & GDK_POINTER_MOTION_MASK, &
       & GDK_BUTTON_PRESS_MASK, GDK_BUTTON_RELEASE_MASK, GDK_KEY_PRESS_MASK, &
       & GDK_KEY_RELEASE_MASK, GDK_ENTER_NOTIFY_MASK, GDK_LEAVE_NOTIFY_MASK, &
       & GDK_STRUCTURE_MASK, GDK_SCROLL_MASK, GDK_ALL_EVENTS_MASK, &
       & GTK_POLICY_AUTOMATIC, GDK_COLORSPACE_RGB


  use gtk_sup

  use iso_c_binding
  use iso_fortran_env, only: error_unit

  implicit none

  ! Type definition for a cairo_user_data_key
!!$  type, bind(c) :: cairo_user_data_key_t
!!$     integer(kind=c_int) :: dummy
!!$  end type cairo_user_data_key_t

  type, bind(c) :: gtkallocation
     integer(kind=c_int) :: x,y,width,height
  end type gtkallocation

contains

  !+
  function hl_gtk_drawing_area_new(scroll, size, ssize, expose_event, &
       & data_expose, button_press_event, data_button_press, &
       & button_release_event, data_button_release, scroll_event, &
       & data_scroll, motion_event, data_motion, realize, data_realize, &
       & configure_event, data_configure, key_press_event, data_key_press, &
       & key_release_event, data_key_release, enter_event, data_enter, &
       & leave_event, data_leave, destroy, data_destroy, event_mask, &
       & event_exclude, auto_add_mask, &
       & tooltip, has_alpha, size_allocate, data_size_allocate, &
       & cairo_status, hscroll_policy, vscroll_policy) result(plota)

    type(c_ptr) :: plota
    type(c_ptr), intent(out), optional :: scroll
    integer(kind=c_int), intent(in), optional, dimension(2) :: size, ssize
    type(c_funptr), optional :: expose_event, button_press_event, &
         & button_release_event, scroll_event, key_press_event, &
         & key_release_event, motion_event, realize, configure_event,&
         & enter_event, leave_event, destroy, size_allocate
    type(c_ptr), intent(in), optional :: data_expose, data_button_press, &
         & data_button_release, data_scroll, data_motion, data_realize, &
         & data_configure, data_key_press, data_key_release, data_enter, &
         & data_leave, data_destroy, data_size_allocate
    integer(kind=c_int), intent(in), optional :: event_mask, event_exclude
    integer(kind=c_int), intent(in), optional :: auto_add_mask
    character(kind=c_char), dimension(*), optional, intent(in) :: tooltip
    integer(kind=c_int), intent(in), optional :: has_alpha
    integer(kind=c_int), intent(out), optional :: cairo_status
    integer(kind=c_int), intent(in), optional :: hscroll_policy, vscroll_policy

    ! A high-level drawing area
    !
    ! SCROLL: c_ptr: optional: If present, then the drawing area will be
    ! 		placed in a scrollable window, whose pointer will be returned
    ! 		here. If it is present, then it rather than the drawable should
    ! 		be used for packing.
    ! SIZE: c_int(): optional: The requested size for the area. If no size is
    ! 		given then a default size of 256x256 is used.
    ! SSIZE: c_int() :: optional: The requested size for a scrolling window
    ! EXPOSE_EVENT: c_funptr: optional: Callback for expose-event signal
    ! 		N.B. In GTK3 the signal is called "draw". If this is not given
    ! 		then a default handler is provided which copies the image
    ! 		surface to the drawing area.
    ! DATA_EXPOSE: c_ptr: optional: Data for expose_event callback
    ! BUTTON_PRESS_EVENT: c_funptr: optional: Callback for button-press-event
    ! 		signal
    ! DATA_BUTTON_PRESS: c_ptr: optional: Data for button_press_event callback
    ! BUTTON_RELEASE_EVENT: c_funptr: optional: Callback for
    ! 		button-release-event signal
    ! DATA_BUTTON_RELEASE: c_ptr: optional: Data for button_release_event
    ! 		callback
    ! SCROLL_EVENT: c_funptr: optional: Callback for scroll-event signal
    ! DATA_SCROLL: c_ptr: optional: Data for scroll_event callback
    ! REALIZE: c_funptr: optional: Callback for realize signal
    ! DATA_REALIZE: c_ptr: optional: Data for realize callback
    ! CONFIGURE_EVENT:c_funptr: optional: Callback for configure-event signal
    ! DATA_CONFIGURE: c_ptr: optional: Data for configure_event callback
    ! KEY_PRESS_EVENT: c_funptr: optional: Callback for key-press-event
    ! 		signal
    ! DATA_KEY_PRESS: c_ptr: optional: Data for key_press_event callback
    ! KEY_RELEASE_EVENT: c_funptr: optional: Callback for
    ! 		key-release-event signal
    ! DATA_KEY_RELEASE: c_ptr: optional: Data for key_release_event
    ! 		callback
    ! MOTION_EVENT: c_funptr: optional: Callback for the motion-notify-event
    ! 		signal
    ! DATA_MOTION: c_ptr: optional: Data for motion_event
    ! ENTER_EVENT: c_funptr: optional: Callback for the enter-notify-event
    ! 		signal
    ! DATA_ENTER: c_ptr: optional: Data for enter_event
    ! LEAVE_EVENT: c_funptr: optional: Callback for the leave-notify-event
    ! 		signal
    ! DATA_LEAVE: c_ptr: optional: Data for leave_event
    ! DESTROY: c_funptr: optional: Callback when the widget is destroyed.
    ! DATA_DESTROY: c_ptr: optional: Data to pass to the destroy callback.
    ! EVENT_MASK: c_int: optional: Mask for which events to pass.
    ! EVENT_EXCLUDE: c_int: optional: Mask for events not to pass (this might
    ! 		be used to prevent auto-enabling an event that should only
    ! 		be enabled by user actions)
    ! AUTO_ADD_MASK: boolean: optional: Set to FALSE to disable automatically
    ! 		adding events to the event mask if a handler is provided.
    ! TOOLTIP: string: optional: Tooltip for the drawing area.
    ! HAS_ALPHA: boolean: optional: If a pixbuf is used, should it have
    ! 		an alpha (transparency) channel (default=FALSE)
    ! SIZE_ALLOCATE: c_funptr: optional: Callback for the
    ! 		'size-allocate' signal
    ! DATA_SIZE_ALLOCATE: c_ptr: optional: Data for size_allocate.
    ! CAIRO_STATUS: c_int: optional: Status code from the cairo surface.
    ! HSCROLL_POLICY: int: optional: Horizontal scrolling policy for the
    ! 		containing scroll window (default AUTOMATIC). 
    ! VSCROLL_POLICY: int: optional: Vertical scrolling policy for the
    ! 		containing scroll window (default AUTOMATIC). 
    !
    ! Odd notes on mask interactions and other things.
    !
    ! * Scroll (wheel) events, are enabled by GDK_SCROLL_MASK or
    ! GDK_BUTTON_PRESS_MASK, thus (as far as I can see) there is no way
    ! to mask wheel events while allowing button presses to be processed.
    ! * It does not appear to be possible to remove events by unsetting bits
    ! in the event mask.
    ! * Adding a tooltip looks to implicitly enable some events.
    ! * An example where an explict EVENT_MASK and EVENT_EXCLUDE might be
    ! useful would be to enable motion events only if a button is down.
    ! * If an explicit size is given then the drawing area cannot be made
    ! smaller than that by resizing the containing window
    !-

    type(c_ptr) :: isurface, cstat_cstr
    integer(kind=c_int) :: mask, insert_mask
    integer(kind=c_int) :: auto_add, s_type
    integer(kind=c_int) :: szx, szy
    logical :: rgba
    integer(kind=c_int) :: cstat, hpolicy, vpolicy
    character(len=120) :: cstat_fstr

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
       scroll = gtk_scrolled_window_new(C_NULL_PTR, C_NULL_PTR)
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
       call gtk_scrolled_window_add_with_viewport (scroll, plota)
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
    call g_object_set_data(plota, "backing-surface", isurface)

    ! Set the event mask, if event callbacks are provided, then
    ! the corresponding mask will be ORed in unless disabled by setting
    ! AUTO_ADD_MASK to FALSE, or for individual events by including the
    ! relevant mask in EVENT_EXCLUDE. Note that expose events are always
    ! enabled.

    if (present(event_mask)) then
       mask = ior(event_mask, GDK_EXPOSURE_MASK)
    else
       mask = GDK_EXPOSURE_MASK
    end if

    if (present(event_exclude)) then
       insert_mask = iand(not(event_exclude), GDK_ALL_EVENTS_MASK)
    else
       insert_mask = GDK_ALL_EVENTS_MASK
    end if

    if (present(auto_add_mask)) then
       auto_add = auto_add_mask
    else
       auto_add = TRUE
    end if

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

    ! Size-allocate signal
    if (present(size_allocate)) then
       if (present(data_size_allocate)) then
          call g_signal_connect(plota, "size-allocate"//c_null_char, &
               & size_allocate, data_size_allocate)
       else
          call g_signal_connect(plota, "size-allocate"//c_null_char, &
               & size_allocate)
       end if
    else
       call g_signal_connect(plota, "size-allocate"//c_null_char, &
            & c_funloc(hl_gtk_drawing_area_resize_cb))
    end if

    ! Expose event
    if (present(expose_event)) then
       if (present(data_expose)) then
!!$GTK< 3.0!         call g_signal_connect(plota, "expose-event"//c_null_char, expose_event, &
!!$GTK< 3.0!              & data_expose)
!!$GTK>=3.0!         call g_signal_connect(plota, "draw"//c_null_char, expose_event, &
!!$GTK>=3.0!              & data_expose)
       else
!!$GTK< 3.0!         call g_signal_connect(plota, "expose-event"//c_null_char, expose_event)
!!$GTK>=3.0!         call g_signal_connect(plota, "draw"//c_null_char, expose_event) 
       endif
    else
!!$GTK< 3.0!       call g_signal_connect(plota, "expose-event"//c_null_char, &
!!$GTK< 3.0!            & c_funloc(hl_gtk_drawing_area_expose_cb))
!!$GTK>=3.0!       call g_signal_connect(plota, "draw"//c_null_char, &
!!$GTK>=3.0!            & c_funloc(hl_gtk_drawing_area_expose_cb))
    end if


    ! Button_press event
    if (present(button_press_event)) then
       if (present(data_button_press)) then
          call g_signal_connect(plota, "button-press-event"//c_null_char, &
               & button_press_event, data_button_press)
       else
          call g_signal_connect(plota, "button-press-event"//c_null_char, &
               & button_press_event)
       endif
       if (auto_add == TRUE) mask = ior(mask, &
            & iand(GDK_BUTTON_PRESS_MASK, insert_mask))
    end if

    ! Button_release event
    if (present(button_release_event)) then
       if (present(data_button_release)) then
          call g_signal_connect(plota, "button-release-event"//c_null_char, &
               & button_release_event, data_button_release)
       else
          call g_signal_connect(plota, "button-release-event"//c_null_char, &
               & button_release_event)
       endif
       if (auto_add == TRUE) mask = ior(mask, &
            & iand(GDK_BUTTON_RELEASE_MASK, insert_mask))
    end if

    ! Scroll event
    if (present(scroll_event)) then
       if (present(data_scroll)) then
          call g_signal_connect(plota, "scroll-event"//c_null_char, &
               &scroll_event, data_scroll)
       else
          call g_signal_connect(plota, "scroll-event"//c_null_char, &
               & scroll_event)
       endif
       if (auto_add == TRUE) mask = ior(mask, &
            & iand(GDK_SCROLL_MASK, insert_mask))
    end if

    ! Key_press event
    if (present(key_press_event)) then
       if (present(data_key_press)) then
          call g_signal_connect(plota, "key-press-event"//c_null_char, &
               & key_press_event, data_key_press)
       else
          call g_signal_connect(plota, "key-press-event"//c_null_char, &
               & key_press_event)
       endif
       ! Note: For keyboard events, the drawing area must be able to
       ! accept input focus as well as the KEY events.
       if (auto_add == TRUE) then
          mask = ior(mask, iand(GDK_KEY_PRESS_MASK, insert_mask))
          call gtk_widget_set_can_focus(plota, TRUE)
       end if

    end if

    ! Key_release event
    if (present(key_release_event)) then
       if (auto_add == TRUE) &
            & call gtk_widget_add_events(plota, GDK_KEY_RELEASE_MASK)
       if (present(data_key_release)) then
          call g_signal_connect(plota, "key-release-event"//c_null_char, &
               & key_release_event, data_key_release)
       else
          call g_signal_connect(plota, "key-release-event"//c_null_char, &
               & key_release_event)
       endif
       ! Note: For keyboard events, the drawing area must be able to
       ! accept input focus as well as the KEY events.
       if (auto_add == TRUE) then
          mask = ior(mask, iand(GDK_KEY_RELEASE_MASK, insert_mask))
          call gtk_widget_set_can_focus(plota, TRUE)
       end if
    end if

    ! Motion event
    if (present(motion_event)) then
       if (present(data_motion)) then
          call g_signal_connect(plota, "motion-notify-event"//c_null_char, &
               & motion_event, data_motion)
       else
          call g_signal_connect(plota, "motion-notify-event"//c_null_char, &
               & motion_event)
       endif
       if (auto_add == TRUE) mask = ior(mask, &
            & iand(GDK_POINTER_MOTION_MASK, insert_mask))
    end if

    ! Enter event
    if (present(enter_event)) then
       if (present(data_enter)) then
          call g_signal_connect(plota, "enter-notify-event"//c_null_char, &
               & enter_event, data_enter)
       else
          call g_signal_connect(plota, "enter-notify-event"//c_null_char, &
               & enter_event)
       endif
       if (auto_add == TRUE) mask = ior(mask, &
            & iand(GDK_ENTER_NOTIFY_MASK, insert_mask))
    end if

    ! Leave event
    if (present(leave_event)) then
       if (present(data_leave)) then
          call g_signal_connect(plota, "leave-notify-event"//c_null_char, &
               & leave_event, data_leave)
       else
          call g_signal_connect(plota, "leave-notify-event"//c_null_char, &
               & leave_event)
       endif
       if (auto_add == TRUE) mask = ior(mask, &
            & iand(GDK_LEAVE_NOTIFY_MASK, insert_mask))
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
       if (auto_add == TRUE) mask = ior(mask, &
            & iand(GDK_STRUCTURE_MASK, insert_mask))
    end if

    ! Apply the event mask

    if (mask /= 0) call gtk_widget_add_events(plota, mask)

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

  !+
  function hl_gtk_drawing_area_get_surface(area) result(isurface)
    type(c_ptr) :: isurface
    type(c_ptr), intent(in) :: area

    ! Convenience routine to get the backing surface of a drawing area.
    !
    ! AREA: c_ptr: required: The drawing area whose surface is required.
    !-

    isurface = g_object_get_data(area, "backing-surface")

  end function hl_gtk_drawing_area_get_surface

  !+
  function hl_gtk_drawing_area_get_gdk_pixbuf(area, x0, y0, xsize, ysize) &
       & result(pixb)
    type(c_ptr) :: pixb
    type(c_ptr), intent(in) :: area
    integer(kind=c_int), intent(in), optional :: x0, y0, xsize, ysize

    ! Read a drawing area (strictly the cairo surface
    ! backing store) to a GDK pixbuf.
    !
    ! AREA: c_ptr: required: The drawing area
    ! X0, Y0: c_int: optional: The origin of the area to return (defaults 0)
    ! XSIZE, YSIZE: c_int: optional: The size of the region to return
    ! 		(defaults, from the origin to the top right of the surface).
    !-

    type(c_ptr) :: surface
    integer(kind=c_int) :: nx, ny, rs, fmt, lpix
    integer(kind=c_int) :: xorig, yorig, xrange, yrange

!!$GTK< 3.0!    type(c_ptr) :: data, pdata
!!$GTK< 3.0!    integer(kind=c_int) :: has_alpha, rsp
!!$GTK< 3.0!    integer :: lppix, nbp
!!$GTK< 3.0!    character(kind=c_char, len=1), dimension(:), pointer :: chdata, swdata
!!$GTK< 3.0!    integer :: i,j,n,m

    ! Define endianness
!!$GTK< 3.0!    integer(kind=c_int8_t), dimension(2), parameter :: &
!!$GTK< 3.0!         & z1 = [0_c_int8_t, 1_c_int8_t]
!!$GTK< 3.0!    integer(kind=c_int16_t), parameter :: z2 = transfer(z1, 0_c_int16_t)
!!$GTK< 3.0!    logical, parameter :: is_big_endian = z2 == 1_c_int16_t

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

!!$GTK>=3.0!    pixb = gdk_pixbuf_get_from_surface(surface, xorig, yorig, &
!!$GTK>=3.0!         & xrange, yrange)

!!$GTK< 3.0!    select case(fmt)
!!$GTK< 3.0!    case (CAIRO_FORMAT_ARGB32)
!!$GTK< 3.0!       has_alpha = TRUE
!!$GTK< 3.0!       nbp = 4
!!$GTK< 3.0!    case(CAIRO_FORMAT_RGB24)
!!$GTK< 3.0!       has_alpha = FALSE
!!$GTK< 3.0!       nbp = 3
!!$GTK< 3.0!    case default
!!$GTK< 3.0!       pixb = c_null_ptr
!!$GTK< 3.0!       return
!!$GTK< 3.0!    end select
!!$GTK< 3.0!
!!$GTK< 3.0!    pixb = gdk_pixbuf_new(GDK_COLORSPACE_RGB, has_alpha, &
!!$GTK< 3.0!         & 8_c_int, xrange, yrange)
!!$GTK< 3.0!    rsp = gdk_pixbuf_get_rowstride(pixb)
!!$GTK< 3.0!    lppix = rsp*(ny-1) + nbp*nx
!!$GTK< 3.0!
!!$GTK< 3.0!    data = cairo_image_surface_get_data(surface)
!!$GTK< 3.0!    call c_f_pointer(data, chdata, [lpix])
!!$GTK< 3.0!    pdata = gdk_pixbuf_get_pixels(pixb)
!!$GTK< 3.0!    call c_f_pointer(pdata, swdata, [lppix])

    ! The pixel order in the cairo surface and the pixbuf differ, and
    ! for cairo depends on the endianness.
    ! pixbuf: RGB or RGBA
    ! cairo (little endian): BGR- or BGRA
    ! cairo (big endian): -RGB or ARGB

!!$GTK< 3.0!    if (c_f_logical(has_alpha)) then
!!$GTK< 3.0!       if (is_big_endian) then
!!$GTK< 3.0!          do i = 1, yrange
!!$GTK< 3.0!             do j = 1, xrange
!!$GTK< 3.0!                n = (yorig+i-1)*rs + 4*(xorig+j-1)+1
!!$GTK< 3.0!                m = (i-1)*rsp + 4*(j-1)+1
!!$GTK< 3.0!                swdata(m:m+3) = chdata(n+[1,2,3,0])
!!$GTK< 3.0!             end do
!!$GTK< 3.0!          end do
!!$GTK< 3.0!       else
!!$GTK< 3.0!          do i = 1, yrange
!!$GTK< 3.0!             do j = 1, xrange
!!$GTK< 3.0!                n = (yorig+i-1)*rs + 4*(xorig+j-1)+1
!!$GTK< 3.0!                m = (i-1)*rsp + 4*(j-1)+1
!!$GTK< 3.0!                swdata(m:m+3) = chdata(n+[2,1,0,3])
!!$GTK< 3.0!             end do
!!$GTK< 3.0!          end do
!!$GTK< 3.0!       end if
!!$GTK< 3.0!    else
!!$GTK< 3.0!       if (is_big_endian) then
!!$GTK< 3.0!          do i = 1, yrange
!!$GTK< 3.0!             do j = 1, xrange
!!$GTK< 3.0!                n = (yorig+i-1)*rs + 4*(xorig+j-1)+1
!!$GTK< 3.0!                m = (i-1)*rsp + 3*(j-1)+1
!!$GTK< 3.0!                swdata(m:m+2) = chdata(n+[1,2,3])
!!$GTK< 3.0!             end do
!!$GTK< 3.0!          end do
!!$GTK< 3.0!       else
!!$GTK< 3.0!          do i = 1, yrange
!!$GTK< 3.0!             do j = 1, xrange
!!$GTK< 3.0!                n = (yorig+i-1)*rs + 4*(xorig+j-1)+1
!!$GTK< 3.0!                m = (i-1)*rsp + 3*(j-1)+1
!!$GTK< 3.0!                swdata(m:m+2) = chdata(n+[2,1,0])
!!$GTK< 3.0!             end do
!!$GTK< 3.0!          end do
!!$GTK< 3.0!       end if
!!$GTK< 3.0!    end if

  end function hl_gtk_drawing_area_get_gdk_pixbuf

  !+
  subroutine hl_gtk_drawing_area_draw_pixbuf(area, pixbuf, x, y)
    type(c_ptr), intent(in) :: area, pixbuf
    integer(kind=c_int), intent(in), optional :: x, y

    ! Render a GdkPixbuf on a drawing area
    !
    ! AREA: c_ptr: required: The drawing area.
    ! PIXBUF: c_ptr: required: The pixbuf to draw.
    ! X, Y: c_int: optional: The coordinate of the upper left corner of the
    ! 		pixbuf on the drawing area (defaults 0).
    !
    ! If you are rendering a pixbuf among other operations then just use
    ! gdk_cairo_set_source_pixbuf directly on the context with which you
    ! are working.
    !-

    type(c_ptr) :: cc
    real(kind=c_double) :: xx, yy

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
    integer(kind=c_int) :: rv
    type(c_ptr), value :: area, event, data

    ! Default callback for exposing a drawing area. For this to be connected
    ! no explicit expose callback should be specified.
    !
    ! AREA: c_ptr: required: The drawing area
    ! EVENT: c_ptr: required: GTK2 = event structure, GTK3 = a cairo context
    ! 		Since this differs between versions, we won't use it.
    ! DATA: c_ptr: required: A pointer to user data (not used).
    !-

    type(c_ptr) :: cr, isurface

    rv = FALSE

    isurface = g_object_get_data(area, "backing-surface")
    if (.not. c_associated(isurface)) then
       write(error_unit,*) &
            & 'hl_gtk_drawing_area_expose_cb: Backing surface is NULL'
       return
    end if

    ! Note for plplot users, this cairo context is a different one from
    ! the context used by plplot for the actual drawing.

    cr = gdk_cairo_create(gtk_widget_get_window(area))
    call cairo_set_source_surface(cr, isurface, 0._c_double, 0._c_double) 
    call cairo_paint(cr)
    call cairo_destroy(cr)
  end function hl_gtk_drawing_area_expose_cb

  !+
  subroutine hl_gtk_drawing_area_destroy_cb(area, data) bind(c)
    type(c_ptr), value :: area, data

    ! Default callback for the destroy signal on the drawing area.
    ! Just destroys the backing surface.
    !
    ! AREA: c_ptr: required: The drawing area being destroyed.
    ! DATA: c_ptr: required: User data for the callback (not used)
    !-

    type(c_ptr) :: isurface

    isurface = g_object_get_data(area, "backing-surface")
    if (c_associated(isurface)) call cairo_surface_destroy(isurface)

  end subroutine hl_gtk_drawing_area_destroy_cb

  !+
  subroutine hl_gtk_drawing_area_resize_cb(area, data) bind(c)
    type(c_ptr), value :: area, data

    ! Default call back for resizing the drawing area, just
    ! copies the old backing store to the new one
    !
    ! AREA: c_ptr: required: The drawing area being destroyed.
    ! DATA: c_ptr: required: User data for the callback (not used)
    !-

    call hl_gtk_drawing_area_resize(area, copy=.true.)
  end subroutine hl_gtk_drawing_area_resize_cb

  !+
  function hl_gtk_drawing_area_cairo_new(area) result(cr)
    type(c_ptr) :: cr
    type(c_ptr), intent(in) :: area

    ! Create a cairo context which will draw into the backing surface
    !
    ! AREA: c_ptr: required: The drawing area to which we will draw.
    ! 
    ! After the drawing operations, you should call `gtk_widget_queue_draw`
    ! to update the plot on the screen and `hl_gtk_pixbuf_cairo_destroy`
    ! to destroy the cairo context.
    !-

    type(c_ptr) :: isurface

    isurface = g_object_get_data(area, "backing-surface")
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
    integer(kind=c_int), intent(in), optional :: destroy_surface

    ! Update the backing surface and destroy the cairo context
    !
    ! CR: c_ptr: required: The cairo context to put in the pixbuf
    ! DESTROY_SURFACE: boolean : optional: Set to TRUE to destroy the
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
    integer(kind=c_int), intent(in), optional, dimension(2) :: size
    logical, optional, intent(in) :: copy

    ! Resize a drawing area and its backing store.
    !
    ! AREA: c_ptr: required: The area to resize.
    ! SIZE: int(2) : optional: The new size, if omitted, then the
    ! 		backing store is resized to match the drawing area (e.g.
    ! 		after resizing the containing window).
    ! COPY: logical: optional: Set to .true. to copy the surface
    ! 		contents to the new backing store.
    !-

    type(c_ptr) :: cback, cback_old, cr, gdk_w
    integer(kind=c_int) :: szx, szy, s_type
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
    cback_old = g_object_get_data(area, "backing-surface")
    s_type = cairo_image_surface_get_format(cback_old)
    cback = cairo_image_surface_create(s_type, szx, szy)
    cback = cairo_surface_reference(cback)   ! Prevent accidental deletion
    call g_object_set_data(area, "backing-surface", cback)

    ! If the copy keyword is set then make a copy from the old
    ! backing store to the new if the gdk window is really there.
    if (copy_surface) then
       gdk_w = gtk_widget_get_window(area)
       if (c_associated(gdk_w)) then
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
    integer(kind=c_int), intent(out), optional :: width, height

    ! Convenience routine to get the current size of a drawing area
    !
    ! AREA: c_ptr: required: The drawing area whose size is needed.
    ! WIDTH: c_int: optional: The width of the area.
    ! HEIGHT: c_int: optional: The height of the area
    !-
 
    type(gtkallocation), target :: alloc

    call gtk_widget_get_allocation(area,c_loc(alloc))
    if (present(width)) width = alloc%width
    if (present(height)) height = alloc%height

  end subroutine hl_gtk_drawing_area_get_size

  ! *********************************************************************
  ! These routines are obsolete, but are retained for the time being to
  ! let older codes work

!!$  function hl_gtk_pixbuf_cairo_new(area, key) result(cr)
!!$    type(c_ptr) :: cr
!!$    type(c_ptr), intent(in) :: area
!!$    type(cairo_user_data_key_t), intent(in), target :: key
!!$    write(error_unit,*) "hl_gtk_pixbuf_cairo_new(area, key) is deprecated,"
!!$    write(error_unit,*) "use hl_gtk_drawing_area_cairo_new(area) instead"
!!$    cr = hl_gtk_drawing_area_cairo_new(area)
!!$  end function hl_gtk_pixbuf_cairo_new
!!$
!!$  subroutine hl_gtk_pixbuf_cairo_destroy(cr, key)
!!$    type(c_ptr), intent(inout) :: cr
!!$    type(cairo_user_data_key_t), intent(in), target :: key
!!$    write(error_unit,*) "hl_gtk_pixbuf_cairo_destroy(area, key) is deprecated,"
!!$    write(error_unit,*) "use hl_gtk_drawing_area_cairo_destroy(area) instead"
!!$    call hl_gtk_drawing_area_cairo_destroy(cr)
!!$  end subroutine hl_gtk_pixbuf_cairo_destroy
!!$
!!$  function hl_gtk_drawing_area_get_pixbuf(area) result(isurface)
!!$    type(c_ptr) :: isurface
!!$    type(c_ptr), intent(in) :: area
!!$    write(error_unit,*) "hl_gtk_drawing_area_get_pixbuf(area) is deprecated,"
!!$    write(error_unit,*) "use hl_gtk_drawing_area_get_surface(area) instead"
!!$    isurface = hl_gtk_drawing_area_get_surface(area)
!!$  end function hl_gtk_drawing_area_get_pixbuf

end module gtk_draw_hl
