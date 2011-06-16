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
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! Contributed by James Tappin
! Some code derived from a demo program by "tadeboro" posted on the gtk forums.
! Last modification: 06-15-2011

module gtk_draw_hl
  !*
  ! gtk_draw_hl
  ! This module provides a high-level drawing interface which automatically
  ! handles redrawing on exposure, and bundles the most likely events to
  ! be needed.
  !
  ! ### Routine List
  !
  ! * hl_gtk_drawing_area_new; Create the drawing area.
  ! * hl_gtk_drawing_area_expose_cb; Default callback for expose events.
  ! * hl_gtk_pixbuf_cairo_new; Create a cairo context attached to the pixbuf.
  ! * hl_gtk_pixbuf_cairo_destroy; Update the pixbuf and destroy the context.
  ! * is_big_endian; Utility function to determine endianness.
  !/

  use gtk, only: gtk_drawing_area_new, gtk_widget_add_events, &
       & gtk_widget_set_size_request, g_signal_connect, &
       & gtk_widget_set_tooltip_text, gtk_widget_get_events, &
       & gtk_widget_set_events, gtk_widget_set_can_focus, &
       & gtk_widget_get_window, gtk_scrolled_window_set_policy,&
       & gtk_scrolled_window_new, gtk_scrolled_window_add_with_viewport, &
       & GDK_EXPOSURE_MASK, GDK_BUTTON_PRESS_MASK, GDK_BUTTON_RELEASE_MASK, &
       & GDK_SCROLL_MASK, GDK_ENTER_NOTIFY_MASK, GDK_KEY_PRESS_MASK, &
       & GDK_KEY_RELEASE_MASK, GDK_LEAVE_NOTIFY_MASK, GDK_POINTER_MOTION_MASK,&
       & GDK_STRUCTURE_MASK, GDK_ALL_EVENTS_MASK, CAIRO_FORMAT_ARGB32, &
       & GDK_COLORSPACE_RGB, GTK_POLICY_AUTOMATIC, &
       & CNULL, NULL, FNULL, TRUE, FALSE

  use cairo, only: cairo_image_surface_create, cairo_set_user_data, &
       & cairo_get_user_data, cairo_get_target, cairo_image_surface_get_data, &
       & cairo_create, cairo_destroy, cairo_surface_destroy, cairo_paint

  use g, only: g_object_unref, g_object_ref, g_object_set_data, &
       & g_object_get_data

  use gdk_pixbuf, only: gdk_pixbuf_new, gdk_pixbuf_copy, &
       & gdk_pixbuf_get_height, gdk_pixbuf_get_width, &
       & gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels

  use gdk, only: gdk_cairo_create, gdk_cairo_set_source_pixbuf

  use iso_c_binding

  implicit none

  ! Type definition for a cairo_user_data_key
  type, bind(c) :: cairo_user_data_key_t
     integer(kind=c_int) :: dummy
  end type cairo_user_data_key_t

contains

  !+
  function hl_gtk_drawing_area_new(scroll, size, ssize, expose_event, &
       & data_expose, button_press_event, data_button_press, &
       & button_release_event, data_button_release, scroll_event, &
       & data_scroll, motion_event, data_motion, realize, data_realize, &
       & configure_event, data_configure, key_press_event, data_key_press, &
       & key_release_event, data_key_release, enter_event, data_enter, &
       & leave_event, data_leave, event_mask, event_exclude, auto_add_mask, &
       & tooltip, has_alpha, use_pixbuf) result(plota)

    type(c_ptr) :: plota
    type(c_ptr), intent(out), optional :: scroll
    integer(kind=c_int), intent(in), optional, dimension(2) :: size, ssize
    type(c_funptr), optional :: expose_event, button_press_event, &
         & button_release_event, scroll_event, key_press_event, &
         & key_release_event, motion_event, realize, configure_event,&
         & enter_event, leave_event
    type(c_ptr), intent(in), optional :: data_expose, data_button_press, &
         & data_button_release, data_scroll, data_motion, data_realize, &
         & data_configure, data_key_press, data_key_release, data_enter, &
         & data_leave
    integer(kind=c_int), intent(in), optional :: event_mask, event_exclude
    integer(kind=c_int), intent(in), optional :: auto_add_mask
    character(kind=c_char), dimension(*), optional, intent(in) :: tooltip
    integer(kind=c_int), intent(in), optional :: has_alpha, use_pixbuf

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
    ! 		and a pixbuf backing is requested, then a default handler is
    ! 		provided which copies the pixbuf to the drawing area.
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
    ! EVENT_MASK: c_int: optional: Mask for which events to pass.
    ! EVENT_EXCLUDE: c_int: optional: Mask for events not to pass (this might
    ! 		used to prevent auto-enabling an event that should only
    ! 		be enabled by user actions)
    ! AUTO_ADD_MASK: boolean: optional: Set to FALSE to disable automatically
    ! 		adding events to the event mask if a handler is provided.
    ! TOOLTIP: string: optional: Tooltip for the drawing area.
    ! HAS_ALPHA: boolean: optional: If a pixbuf is used, should it have
    ! 		an alpha (transparency) channel (default=FALSE)
    ! USE_PIXBUF: boolean: optional: Set to FALSE to disable the backing
    ! 		store pixbuf
    !
    ! Odd notes on mask interactions and other things.
    !
    ! * Scroll (wheel) events, are enabled by GDK_SCROLL_MASK or
    ! GDK_BUTTON_PRESS_MASK, thus (as far as I can see) there is no way
    ! to mask wheel events while allowing button presses to be processed.
    ! * It does not appear to be possible to remove events by unsetting bits
    ! in the event mask.
    ! * Adding a tooltip looks to implicitly enables some events.
    ! * If you want to get at the backing pixbuf explicitly, then use
    ! pixbuf = g_object_get_data(area, "backing-pixbuf").
    ! * An example where an explict EVENT_MASK and EVENT_EXCLUDE might be
    ! useful would be to enable motion events only if a button is down.
    !-

    type(c_ptr) :: pixbuf
    integer(kind=c_int) :: mask, insert_mask
    integer(kind=c_int) :: auto_add, alpha
    integer(kind=c_int) :: szx, szy
    logical :: backing_pixbuf

    plota = gtk_drawing_area_new()
    if (present(size)) then
       call gtk_widget_set_size_request(plota, size(1), size(2))
       szx = size(1)
       szy = size(2)
    else
       call gtk_widget_set_size_request(plota, 256, 256)
       szx = 256
       szy = 256
    end if

    if (present(use_pixbuf)) then
       backing_pixbuf = (use_pixbuf == TRUE)
    else
       backing_pixbuf = .TRUE.
    end if

    ! Add it to a scrolling window if one is requested
    if (present(scroll)) then
       scroll = gtk_scrolled_window_new(NULL, NULL)
       call gtk_scrolled_window_set_policy(scroll, GTK_POLICY_AUTOMATIC, &
            & GTK_POLICY_AUTOMATIC)
       if (present(ssize)) &
            & call gtk_widget_set_size_request(scroll, ssize(1), ssize(2))
       call gtk_scrolled_window_add_with_viewport (scroll, plota)
    end if

    ! If a backing pixbuf is requested, then create it.
    if (backing_pixbuf) then
       if (present(has_alpha)) then
          alpha = has_alpha
       else
          alpha = FALSE
       end if
       pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, &
            & alpha, 8_c_int, szx, szy)
       pixbuf = g_object_ref(pixbuf)
       call g_object_set_data(plota, "backing-pixbuf", pixbuf)
    end if

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
          call g_signal_connect(plota, "realize"//cnull, realize, &
               & data_realize)
       else
          call g_signal_connect(plota, "realize"//cnull, realize)
       endif
    end if

    ! Expose event
    if (present(expose_event)) then
       if (present(data_expose)) then
!!$2         call g_signal_connect(plota, "expose-event"//cnull, expose_event, &
!!$2              & data_expose)
         call g_signal_connect(plota, "draw"//cnull, expose_event, &
              & data_expose)
       endif
    else if (backing_pixbuf) then
!!$2       call g_signal_connect(plota, "expose-event"//cnull, &
!!$2            & c_funloc(hl_gtk_drawing_area_expose_cb))
       call g_signal_connect(plota, "draw"//cnull, &
            & c_funloc(hl_gtk_drawing_area_expose_cb))
    end if


    ! Button_press event
    if (present(button_press_event)) then
       if (present(data_button_press)) then
          call g_signal_connect(plota, "button-press-event"//cnull, &
               & button_press_event, data_button_press)
       else
          call g_signal_connect(plota, "button-press-event"//cnull, &
               & button_press_event)
       endif
       if (auto_add == TRUE) mask = ior(mask, &
            & iand(GDK_BUTTON_PRESS_MASK, insert_mask))
    end if

    ! Button_release event
    if (present(button_release_event)) then
       if (present(data_button_release)) then
          call g_signal_connect(plota, "button-release-event"//cnull, &
               & button_release_event, data_button_release)
       else
          call g_signal_connect(plota, "button-release-event"//cnull, &
               & button_release_event)
       endif
       if (auto_add == TRUE) mask = ior(mask, &
            & iand(GDK_BUTTON_RELEASE_MASK, insert_mask))
    end if

    ! Scroll event
    if (present(scroll_event)) then
       if (present(data_scroll)) then
          call g_signal_connect(plota, "scroll-event"//cnull, scroll_event, &
               & data_scroll)
       else
          call g_signal_connect(plota, "scroll-event"//cnull, scroll_event)
       endif
       if (auto_add == TRUE) mask = ior(mask, &
            & iand(GDK_SCROLL_MASK, insert_mask))
    end if

    ! Key_press event
    if (present(key_press_event)) then
       if (present(data_key_press)) then
          call g_signal_connect(plota, "key-press-event"//cnull, &
               & key_press_event, data_key_press)
       else
          call g_signal_connect(plota, "key-press-event"//cnull, &
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
          call g_signal_connect(plota, "key-release-event"//cnull, &
               & key_release_event, data_key_release)
       else
          call g_signal_connect(plota, "key-release-event"//cnull, &
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
          call g_signal_connect(plota, "motion-notify-event"//cnull, &
               & motion_event, data_motion)
       else
          call g_signal_connect(plota, "motion-notify-event"//cnull, &
               & motion_event)
       endif
       if (auto_add == TRUE) mask = ior(mask, &
            & iand(GDK_POINTER_MOTION_MASK, insert_mask))
    end if

    ! Enter event
    if (present(enter_event)) then
       if (present(data_enter)) then
          call g_signal_connect(plota, "enter-notify-event"//cnull, &
               & enter_event, data_enter)
       else
          call g_signal_connect(plota, "enter-notify-event"//cnull, &
               & enter_event)
       endif
       if (auto_add == TRUE) mask = ior(mask, &
            & iand(GDK_ENTER_NOTIFY_MASK, insert_mask))
    end if

    ! Leave event
    if (present(leave_event)) then
       if (present(data_leave)) then
          call g_signal_connect(plota, "leave-notify-event"//cnull, &
               & leave_event, data_leave)
       else
          call g_signal_connect(plota, "leave-notify-event"//cnull, &
               & leave_event)
       endif
       if (auto_add == TRUE) mask = ior(mask, &
            & iand(GDK_LEAVE_NOTIFY_MASK, insert_mask))
    end if

    ! Configure event
    if (present(configure_event)) then
       if (present(data_configure)) then
          call g_signal_connect(plota, "configure-event"//cnull, &
               & configure_event,  data_configure)
       else
          call g_signal_connect(plota, "configure-event"//cnull, &
               & configure_event)
       endif
       if (auto_add == TRUE) mask = ior(mask, &
            & iand(GDK_STRUCTURE_MASK, insert_mask))
    end if

    ! Apply the event mask

    if (mask /= 0) call gtk_widget_add_events(plota, mask)

    if (present(tooltip)) call gtk_widget_set_tooltip_text(plota, tooltip)

  end function hl_gtk_drawing_area_new

  !+
  function hl_gtk_drawing_area_expose_cb(area, event, data) bind(c) &
       & result(rv)
    integer(kind=c_int) :: rv
    type(c_ptr), value :: area, event, data

    ! Default callback for exposing a drawing area. For this to be connected
    ! a pixbuf must be requested, and no explicit expose callback should
    ! be specified.
    !
    ! AREA: c_ptr: required: The drawing area
    ! EVENT: c_ptr: required: GTK2 = event structure, GTK3 = a cairo context
    ! 		Since this differs between versions, we won't use it.
    ! DATA: c_ptr: required: A pointer to user data (not used).
    !-

    type(c_ptr) :: cr, pixbuf

    pixbuf = g_object_get_data(area, "backing-pixbuf")
    cr = gdk_cairo_create (gtk_widget_get_window(area))
    call gdk_cairo_set_source_pixbuf(cr, pixbuf, 0._c_double, 0._c_double) 
    call cairo_paint(cr)
    call cairo_destroy(cr)
    rv = FALSE
  end function hl_gtk_drawing_area_expose_cb

!+
  function hl_gtk_pixbuf_cairo_new(area, key) result(cr)
    type(c_ptr) :: cr
    type(c_ptr), intent(in) :: area
    type(cairo_user_data_key_t), intent(in), target :: key

    ! Create a cairo context which will draw into the pixbuf
    ! Based on C code posted to GtkForums by "tadeboro".
    !
    ! PIXBUF: c_ptr: required: The pixbuf to which we will draw.
    ! KEY: cairo_user_data_key_t: required: A key to identify the user
    ! 		data between this and hl_gtk_pixbuf_cairo_destroy.
    ! 
    ! After the drawing operations, you must call `hl_gtk_pixbuf_cairo_destroy`
    ! to update the pixbuf and destroy the cairo context.
    !-

    type(c_ptr) :: pixbuf
    integer(kind=c_int) :: width, height, n_channels
    character(kind=c_char), dimension(:,:,:), pointer :: p_pixels, s_pixels
    type(c_ptr) :: pp_pixels, ss_pixels
    type(c_ptr) :: surface
    integer, dimension(4) :: idx
    integer :: ih, iw
    real(kind=c_double) :: alpha_factor
    integer(kind=c_int) :: ok

    pixbuf = g_object_get_data(area, "backing-pixbuf")
    if (.not. c_associated(pixbuf)) then
       cr = NULL
       write(0,*) "hl_gtk_pixbuf_cairo_new:: pixbuf is NULL"
       return
    end if

    ! Extract the pixbuf dimensions
    width = gdk_pixbuf_get_width(pixbuf)
    height = gdk_pixbuf_get_height(pixbuf)
    n_channels = gdk_pixbuf_get_n_channels(pixbuf)

    ! And its pixels
    pp_pixels = gdk_pixbuf_get_pixels(pixbuf)
    call c_f_pointer(pp_pixels, p_pixels, (/n_channels, width, height/))

    ! Now make a matching cairo surface.

    surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, &
         & width, height)
    ss_pixels = cairo_image_surface_get_data(surface)
    call c_f_pointer(ss_pixels, s_pixels, (/4, width, height/))

    ! The mapping of channels to indices in the cairo surface depends on
    ! the endianness of the platform
    if (is_big_endian()) then
       idx = [4,3,2,1]
    else
       idx = [1,2,3,4]
    end if

    if (n_channels == 3) then
       do ih = 1, height
          do iw = 1, width
             s_pixels(idx(1), iw, ih) = p_pixels(3, iw, ih)
             s_pixels(idx(2), iw, ih) = p_pixels(2, iw, ih)
             s_pixels(idx(3), iw, ih) = p_pixels(1, iw, ih)
             s_pixels(idx(4), iw, ih) = char(255)
          end do
       end do
    else
       do ih = 1, height
          do iw = 1, width
             alpha_factor = real(ichar(p_pixels(4, iw, ih)), c_double) / &
                  & 255._c_double
             s_pixels(idx(1), iw, ih) = &
                  & char(nint(ichar(p_pixels(3, iw, ih))*alpha_factor))
             s_pixels(idx(2), iw, ih) = &
                  & char(nint(ichar(p_pixels(2, iw, ih))*alpha_factor))
             s_pixels(idx(3), iw, ih) = &
                  & char(nint(ichar(p_pixels(1, iw, ih))*alpha_factor))
             s_pixels(idx(4), iw, ih) = p_pixels(4, iw, ih)
          end do
       end do
    end if

    ! Create the cairo context from the surface
    cr = cairo_create(surface)
    call cairo_surface_destroy(surface)
    ok = cairo_set_user_data(cr, c_loc(key), pixbuf, c_funloc(g_object_unref))
  end function hl_gtk_pixbuf_cairo_new

  !+
  function hl_gtk_pixbuf_cairo_destroy(cr, key) &
       & result(pixbuf)

    type(c_ptr) :: pixbuf
    type(c_ptr), intent(inout) :: cr
    type(cairo_user_data_key_t), intent(in), target :: key

    ! Update the pixbuf and destroy the cairo context
    ! Based on C code posted to GtkForums by "tadeboro".
    !
    ! CR: c_ptr: required: The cairo context to put in the pixbuf
    ! KEY: cairo_user_data_key_t: required: The key to find the pixbuf (just
    ! 		has to be the same variable as KEY in hl_gtk_pixbuf_cairo_new
    !
    ! This is called following drawing operations to the context created by
    ! `hl_gtk_pixbuf_cairo_new`
    !-

    integer(kind=c_int) :: width, height, n_channels !, p_stride, s_stride
    character(kind=c_char), dimension(:,:,:), pointer :: p_pixels, s_pixels
    type(c_ptr) :: pp_pixels, ss_pixels
    type(c_ptr) :: surface
    integer, dimension(4) :: idx
    integer :: ih, iw
    real(kind=c_double) :: alpha_factor

    ! Get the pixbuf from the context
    pixbuf = cairo_get_user_data(cr, c_loc(key))

    ! Get the cairo surface from which to read the pixels
    surface = cairo_get_target(cr)

    ! Get pixbuf information
    height = gdk_pixbuf_get_height(pixbuf)
    width = gdk_pixbuf_get_width(pixbuf)
    n_channels = gdk_pixbuf_get_n_channels(pixbuf)
    
    ! And its pixels
    pp_pixels = gdk_pixbuf_get_pixels(pixbuf)
    call c_f_pointer(pp_pixels, p_pixels, (/ n_channels, width, height/))

    ! Also the pixels of the surface
    ss_pixels = cairo_image_surface_get_data(surface)
    call c_f_pointer(ss_pixels, s_pixels, (/ 4, width, height/))

    ! Set the indexing
    if (is_big_endian()) then
       idx = [4,3,2,1]
    else
       idx = [1,2,3,4]
    end if

    ! Copy the pixels (we are going to assume that if the pixbuf
    ! doesn't have an alpha channel then the surface doesn't have
    ! useful information in it).
    if (n_channels == 3) then
       do ih = 1, height
          do iw = 1, width
             p_pixels(1, iw, ih) = s_pixels(idx(3), iw, ih)
             p_pixels(2, iw, ih) = s_pixels(idx(2), iw, ih)
             p_pixels(3, iw, ih) = s_pixels(idx(1), iw, ih)
          end do
       end do
    else
       do ih = 1, height
          do iw = 1, width
             alpha_factor = 255._c_double / &
                  & real(ichar(s_pixels(idx(4), iw, ih)), c_double)

             p_pixels(1, iw, ih) = &
                  & char(nint(ichar(s_pixels(idx(3), iw, ih))*alpha_factor))
             p_pixels(2, iw, ih) = &
                  & char(nint(ichar(s_pixels(idx(2), iw, ih))*alpha_factor))
             p_pixels(3, iw, ih) = &
                  & char(nint(ichar(s_pixels(idx(1), iw, ih))*alpha_factor))
             p_pixels(4, iw, ih) = s_pixels(idx(4), iw, ih)
          end do
       end do
    end if

    call cairo_destroy(cr)
  end function hl_gtk_pixbuf_cairo_destroy

  !+
  function is_big_endian()
    logical is_big_endian

    ! Determine if the machine is big-endian or not
    !-

    integer(kind=c_int8_t), dimension(2) :: i1 = (/ 0_c_int8_t, 1_c_int8_t /)
    integer(kind=c_int16_t) :: i2

    i2 = transfer(i1, i2)

    is_big_endian = (i2 == 1)
  end function is_big_endian

 end module gtk_draw_hl
