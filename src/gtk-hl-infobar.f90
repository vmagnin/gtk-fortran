! Copyright (C) 2012
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
!------------------------------------------------------------------------------
! Contributed by James Tappin
! Last modification: 2012-08-30, vmagnin 2020-06-04 (GTK 4)
!*
! Infobar
!------------------------------------------------------------------------------
module gtk_hl_infobar
  ! Convenience interface for the GtkInfoBar widget.
  ! This is created because the routine for adding multiple buttons is
  ! variadic, and setting the message involves several levels of indirection.
  !/

  use iso_c_binding
  use gtk_sup
  use g, only: g_list_nth_data
  use gtk, only: gtk_info_bar_add_child, gtk_info_bar_add_button, &
       & gtk_info_bar_new, gtk_info_bar_get_revealed, &
       & gtk_info_bar_set_revealed, & 
       & gtk_info_bar_set_default_response, gtk_info_bar_set_message_type, &
       & gtk_info_bar_set_response_sensitive, gtk_label_new, &
       & gtk_label_set_text, &
       & gtk_widget_show, gtk_widget_hide, g_signal_connect, TRUE, FALSE, &
       & GTK_ORIENTATION_HORIZONTAL, GTK_ORIENTATION_VERTICAL

  implicit none

contains
  !+
  function hl_gtk_info_bar_new(buttons, ids, response, data_response, close, &
       & data_close, auto_show, type, default, horizontal, buttons_below) &
       & result(infobar)
    type(c_ptr) :: infobar
    character(len=*,kind=c_char), dimension(:), intent(in), optional :: buttons
    integer(kind=c_int), dimension(:), intent(in), optional :: ids
    type(c_funptr), optional :: response, close
    type(c_ptr), intent(in), optional :: data_response, data_close
    integer(kind=c_int), intent(in), optional :: auto_show, type, default
    integer(kind=c_int), intent(in), optional :: horizontal, buttons_below

    ! Create a new info bar.
    !
    ! BUTTONS: f_string(): optional: The buttons to add to the 
    ! IDS: c_int(): optional: The response IDs for the buttons. If buttons are
    ! 		specified and no IDS are given, then the button index is used.
    ! RESPONSE: c_funptr: optional: The handler for a button pressed (has
    ! 		an extra argument which is the button ID.
    ! DATA_RESPONSE: c_ptr: optional: User data to pass to the response handler.
    ! CLOSE: c_funptr: optional: The handler for closing the bar by a
    ! 		key action.
    ! DATA_CLOSE: c_ptr: optional: User data to pass to the close handler.
    ! AUTO_SHOW: boolean: optional: Whether the info bar should be displayed
    ! 		when its parent is shown. Default=FALSE.
    ! TYPE: c_int: optional: The initial message type.
    ! DEFAULT: c_int: optional: Set the initial default response ID.
    ! HORIZONTAL: boolean: optional: not in GTK4: Set to TRUE to lay the buttons in a
    ! 		row rather than a column.
    ! BUTTONS_BELOW: boolean: optional: Set to TRUE to place the buttons
    ! 		below the message rather than to the right.
    !-

    integer(kind=c_int) :: i, id
    integer(kind=c_int) :: no_auto
    type(c_ptr) :: label, junk, action

    infobar = gtk_info_bar_new()

    label = gtk_label_new (c_null_char)
    call gtk_info_bar_add_child (infobar, label)
    call gtk_widget_show (label)

    if (present(horizontal)) then
       print *, "In GTK4, GtkOrientable is not implemented in GtkInfoBar"
    end if

    if (present(buttons_below)) then
       if (c_f_logical(buttons_below)) then
         print *, "In GTK4, GtkOrientable is not implemented in GtkInfoBar"
       else
         print *, "In GTK4, GtkOrientable is not implemented in GtkInfoBar"
       end if
    end if

    if (present(buttons)) then 
       do i = 1, size(buttons)
          if (present(ids)) then
             id = ids(i)
          else
             id = i-1
          end if
          junk = gtk_info_bar_add_button(infobar, &
               & trim(buttons(i))//c_null_char, id)
       end do
    end if

    if (present(response)) then
       if (present(data_response)) then
          call g_signal_connect(infobar, "response"//c_null_char, &
               & response, data_response)
       else
          call g_signal_connect(infobar, "response"//c_null_char, &
               & response)
       end if
    end if
    if (present(close)) then
       if (present(data_close)) then
          call g_signal_connect(infobar, "close"//c_null_char, &
               & close, data_close)
       else
          call g_signal_connect(infobar, "close"//c_null_char, &
               & close)
       end if
    end if

    if (present(auto_show)) then
       no_auto = f_c_logical(auto_show /= FALSE) 
    else
       no_auto = TRUE
    end if
    call gtk_info_bar_set_revealed(infobar, no_auto)
    if (no_auto == TRUE) call gtk_widget_hide(infobar)

    if (present(type)) &
         & call gtk_info_bar_set_message_type(infobar,type)
    if (present(default)) &
         & call gtk_info_bar_set_default_response(infobar, default)

  end function hl_gtk_info_bar_new

  !+
  subroutine hl_gtk_info_bar_message(infobar, message, type, default, &
       & ids, state)
    type(c_ptr), intent(in) :: infobar
    character(kind=c_char), dimension(*), intent(in) :: message
    integer(kind=c_int), intent(in), optional :: type, default
    integer(kind=c_int), intent(in), optional, dimension(:) :: ids, state

    ! Show a message in an infobar
    !
    ! INFOBAR: c_ptr: required: The infobar to update.
    ! MESSAGE: c_str: required: The message to display.
    ! TYPE: c_int: optional: The severity level of the message.
    ! DEFAULT: c_int: optional: Set the default response ID.
    ! IDS: c_int(): optional: Response IDs whose sensitivity will be set.
    ! STATE: boolean(): optional: Sensitivity states of the IDS.
    !
    ! To set buttons to be sensitive or not, you must give BOTH ids
    ! and state, and they must be the same length.
    !-

    type(c_ptr) :: content, label, children
    integer :: i

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! GTK 4 
    !content = gtk_info_bar_get_content_area(infobar)
    print *, "Not in GTK4: children = gtk_container_get_children(content)"
    !label = g_list_nth_data(children, 0_c_int)

    !call gtk_label_set_text(label,message)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (present(type)) &
         & call gtk_info_bar_set_message_type(infobar,type)
    if (present(default)) &
         & call gtk_info_bar_set_default_response(infobar, default)

    if (present(ids) .and. present(state)) then
       do i = 1, min(size(ids),size(state))
          call gtk_info_bar_set_response_sensitive (infobar, ids(i), &
               & state(i))
       end do
    end if
    
    if (c_f_logical(gtk_info_bar_get_revealed(infobar))) &
         & call gtk_widget_show(infobar)

  end subroutine hl_gtk_info_bar_message

end module gtk_hl_infobar
