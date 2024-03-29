! This file is part of gtk-fortran, a GTK / Fortran interface library.
! Copyright (C) 2011 The gtk-fortran team
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
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!-----------------------------------------------------------------
! Contributed by James Tappin 2011-11-30
! Last modification: vmagnin 2020-06-17 (GTK 4),
! vmagnin 2020-12-20
!-----------------------------------------------------------------


!*
! Buttons
module gtk_hl_button
  ! Convenience interfaces for regular buttons, checkboxes.
  !/

  use gtk_sup
  use gtk_hl_misc
  use, intrinsic :: iso_c_binding
  ! autogenerated use's
  use gtk, only: gtk_button_new, gtk_button_new_with_label,&
       & gtk_check_button_new, gtk_check_button_new_with_label,&
       & gtk_check_button_set_active, &
       & gtk_toggle_button_set_active, gtk_button_get_child, &
       & gtk_widget_set_sensitive, gtk_widget_set_tooltip_text, &
       & gtk_label_new, gtk_label_set_markup, gtk_button_set_child, &
       & gtk_button_set_label, gtk_toggle_button_new, &
       & gtk_toggle_button_new_with_label, &
       & TRUE, FALSE, g_signal_connect

  use g, only: g_slist_length, g_slist_nth, g_slist_nth_data

  implicit none

contains
  !+
  function hl_gtk_button_new(label, clicked, data, tooltip, sensitive, &
       & is_markup) result(but)

    type(c_ptr) :: but
    character(kind=c_char), dimension(*), intent(in) :: label
    type(c_funptr), optional :: clicked
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(c_int), intent(in), optional :: sensitive
    integer(c_int), optional, intent(in) :: is_markup

    ! Higher-level button
    !
    ! LABEL |  string |  required |  The label on the button
    ! CLICKED |  c_funptr |  optional |  callback routine for the "clicked" signal
    ! DATA |  c_ptr |  optional |  Data to be passed to the clicked callback
    ! TOOLTIP |  string |  optional |  tooltip to be displayed when the pointer is held over the button.
    ! SENSITIVE |  boolean |  optional |  Whether the widget should initially be sensitive or not.
    ! IS_MARKUP |  boolean |  optional |  Set this to TRUE if the label contains Pango markup.
    !-

    type(c_ptr) :: label_w
    logical :: markup

    if (present(is_markup)) then
       markup = c_f_logical(is_markup)
    else
       markup = .false.
    end if

    if(markup) then
       but = gtk_button_new()
       label_w=gtk_label_new(c_null_char)
       call gtk_label_set_markup(label_w, label)
       call gtk_button_set_child(but, label_w)
    else
       but=gtk_button_new_with_label(label)
    end if

    if (present(clicked)) then
       if (present(data)) then
          call g_signal_connect(but, "clicked"//C_NULL_CHAR, &
               & clicked, data)
       else
          call g_signal_connect(but, "clicked"//C_NULL_CHAR, &
               & clicked)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(but, tooltip)
    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(but, sensitive)

  end function hl_gtk_button_new

  !+
  subroutine hl_gtk_button_set_label(button, label, is_markup)
    type(c_ptr), intent(in) :: button
    character(kind=c_char), dimension(*), intent(in) :: label
    integer(c_int), intent(in), optional :: is_markup

    ! Set the label of a button, including using markup.
    !
    ! BUTTON |  c_ptr |  required |  The button to modify.
    ! LABEL |  string |  required |  The new label for the button.
    ! IS_MARKUP |  boolean |  optional |  Set this to TRUE if the label contains pango markup.
    !-

    logical :: markup
    type(c_ptr) :: label_w

    if (present(is_markup)) then
       markup = c_f_logical(is_markup)
    else
       markup = .false.
    end if

    if (markup) then
       label_w = gtk_button_get_child(button)
       call gtk_label_set_markup(label_w, label)
    else
       call gtk_button_set_label(button, label)
    end if
  end subroutine hl_gtk_button_set_label

  !+
  function hl_gtk_check_button_new(label, toggled, data, tooltip, &
       & initial_state, sensitive, is_markup, toggle) result(but)

    type(c_ptr) :: but
    character(kind=c_char), dimension(*), intent(in) :: label
    type(c_funptr), optional :: toggled
    type(c_ptr), optional :: data
    character(kind=c_char), dimension(*), intent(in), optional :: tooltip
    integer(c_int), intent(in), optional :: initial_state
    integer(c_int), intent(in), optional :: sensitive, is_markup, toggle

    ! Higher level check box.
    !
    ! LABEL |  string |  required |   The label on the button.
    ! TOGGLED |  c_funptr |  optional |  Callback function for the "toggled" signal.
    ! DATA |  c_ptr |  optional |  Data to pass to/from the toggled callback.
    ! TOOLTIP |  string |  optional |  A tooltip for the check_button.
    ! INITIAL_STATE |  integer |  optional |  set the initial state of the check_button.
    ! SENSITIVE |  boolean |  optional |  Whether the widget should initially be sensitive or not.
    ! IS_MARKUP |  boolean |  optional |  Set this to TRUE if the label contains Pango markup.
    ! TOGGLE |  boolean |  optional |  Set this to TRUE to make a toggle button rather than a check button.
    !-

    type(c_ptr) :: label_w
    logical :: markup
    logical :: is_toggle


    if (present(is_markup)) then
       markup = c_f_logical(is_markup)
    else
       markup = .false.
    end if
    if (present(toggle)) then
       is_toggle = c_f_logical(toggle)
    else
       is_toggle = .false.
    end if

    if(markup) then
       if (is_toggle) then
          but = gtk_toggle_button_new()
       else
          but = gtk_check_button_new()
       end if
       label_w=gtk_label_new(c_null_char)
       call gtk_label_set_markup(label_w, label)
       call gtk_button_set_child(but, label_w)
    else
       if (is_toggle) then
          but = gtk_toggle_button_new_with_label(label)
       else
          but = gtk_check_button_new_with_label(label)
       end if
    end if

    if (present(initial_state)) then
       if (is_toggle) then
          call gtk_toggle_button_set_active(but, initial_state)
       else
          call gtk_check_button_set_active(but, initial_state)
       end if
    end if

    if (present(toggled)) then
       if (present(data)) then
          call g_signal_connect(but, "toggled"//c_null_char, toggled, data)
       else
          call g_signal_connect(but, "toggled"//c_null_char, toggled)
       end if
    end if

    if (present(tooltip)) call gtk_widget_set_tooltip_text(but, tooltip)

    if (present(sensitive)) &
         & call gtk_widget_set_sensitive(but, sensitive)

  end function hl_gtk_check_button_new

  !+
  subroutine hl_gtk_button_set_label_markup(but, label)
    type(c_ptr) :: but
    character(kind=c_char), dimension(*), intent(in) :: label

    ! Set a markup label on a button
    !
    ! BUT |  c_ptr |  required |  The button to relabel
    ! LABEL |  string |  required |  The string (with Pango markup) to apply.
    !
    ! Normally if the label does not need Pango markup, then
    ! gtk_button_set_label can be used.
    !-

    call hl_gtk_bin_set_label_markup(but, label)

  end subroutine hl_gtk_button_set_label_markup
end module gtk_hl_button
