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
!
! Contributed by James Tappin
! Last modification: 2011-11-21

! --------------------------------------------------------
! gtk-hl-combobox.f90
! Generated: Tue Oct 29 17:12:20 2013 GMT
! Generated for  GTK version: 3.10.0.
! Generated for GLIB version: 2.38.0.
! --------------------------------------------------------


!*
! ComboBox
module gtk_hl_combobox
  ! This interface implements the GtkComboBoxText widget for making a chooser.
  ! While this has more limited capabilities than the full GtkComboBox, it
  ! is adequate for the vast majority of uses.
  !/

  use gtk_sup
  use, intrinsic :: iso_c_binding
  ! Autogenerated use's
  use gtk, only: gtk_combo_box_get_active, gtk_combo_box_new,&
       & gtk_combo_box_set_active, &
       & gtk_widget_set_sensitive, gtk_widget_set_tooltip_text,&
       & gtk_combo_box_text_get_active_text, gtk_combo_box_text_new,&
       & gtk_combo_box_text_new_with_entry, &
       & gtk_combo_box_text_remove, &
       & gtk_combo_box_text_append_text,&
       & gtk_combo_box_text_insert_text,&
       & gtk_combo_box_text_prepend_text,&
       & gtk_combo_box_text_remove_all, &
       & gtk_combo_box_get_model, gtk_tree_model_iter_n_children, &
       & TRUE, FALSE, g_signal_connect

  implicit none

  interface hl_gtk_combo_box_delete
     module procedure hl_gtk_combo_box_delete_single
     module procedure hl_gtk_combo_box_delete_multi
     module procedure hl_gtk_combo_box_delete_all
  end interface hl_gtk_combo_box_delete

contains
  !+
  function hl_gtk_combo_box_new(has_entry, changed, data, initial_choices, &
       & sensitive, tooltip, active) result(cbox)

    type(c_ptr) :: cbox
    integer(c_int), intent(in), optional :: has_entry
    type(c_funptr), optional :: changed
    type(c_ptr), intent(in), optional :: data
    character(len=*), dimension(:), intent(in), optional :: initial_choices
    integer(c_int), intent(in), optional :: sensitive
    character(kind=c_char), dimension(*), optional, intent(in) :: tooltip
    integer(c_int), optional, intent(in) :: active

    ! Creator for the combobox.
    !
    ! HAS_ENTRY |  boolean |  optional |  Set to TRUE to add an entry field.
    ! CHANGED |  c_funptr |  optional |  Callback routine for the "changed" signal.
    ! DATA |  c_ptr |  optional |  User data for the changed callback.
    ! INITIAL_CHOICES |  string() |  optional |  Initial list of choices.
    ! SENSITIVE |  boolean |  optional |  Set to FALSE to make the widget start in an insensitive state.
    ! TOOLTIP |  string |  optional |  A tooltip to display when the pointer is held over the widget.
    ! ACTIVE |  c_int |  optional |  The initial active selection.
    !-

    integer(c_int) :: ientry
    integer(c_int) :: i

    if (present(has_entry)) then
       ientry = has_entry
    else
       ientry = FALSE
    end if

    if (ientry == TRUE) then
       cbox = gtk_combo_box_text_new_with_entry()
    else
       cbox = gtk_combo_box_text_new()
    end if

    if (present(initial_choices)) then
       do i=1,size(initial_choices)
          call gtk_combo_box_text_append_text(cbox, &
               & trim(initial_choices(i))//C_NULL_CHAR)
       end do
    end if

    if (present(active)) call gtk_combo_box_set_active(cbox, active)

    if (present(changed)) then
       if (present(data)) then
          call g_signal_connect(cbox, "changed"//C_NULL_CHAR, changed, data)
       else
          call g_signal_connect(cbox, "changed"//C_NULL_CHAR, changed)
       end if
    end if

    if (present(sensitive)) call gtk_widget_set_sensitive(cbox, sensitive)
    if (present(tooltip)) call gtk_widget_set_tooltip_text(cbox, tooltip)
  end function hl_gtk_combo_box_new

  !+
  subroutine hl_gtk_combo_box_add_text(cbox, text, index, at_start)

    type(c_ptr), intent(in) :: cbox
    character(kind=c_char), dimension(*), optional :: text
    integer(c_int), intent(in), optional :: index
    integer(c_int), intent(in), optional :: at_start

    ! Add a new choice to a combo box.
    !
    ! CBOX |  c_ptr |  required |  The combo box to modify.
    ! TEXT |  string |  required |  The text to add.
    ! INDEX |  c_int |  optional |  The location at which to add the text.
    ! AT_START |  boolean |  optional |  If set to TRUE and INDEX is not given then add the text at the start of the list.
    !
    ! If neither INDEX nor AT_START is present the text is appended.
    !-

    integer(c_int) :: prepend

    if (present(index)) then
       call gtk_combo_box_text_insert_text(cbox, index, text)
    else
       if (present(at_start)) then
          prepend = at_start
       else
          prepend = FALSE
       end if
       if (prepend == TRUE) then
          call gtk_combo_box_text_prepend_text(cbox, text)
       else
          call gtk_combo_box_text_append_text(cbox, text)
       end if
    end if
  end subroutine hl_gtk_combo_box_add_text

  !+
  subroutine hl_gtk_combo_box_delete_single(cbox, index)

    type(c_ptr), intent(in) :: cbox
    integer(c_int), intent(in) :: index

    ! Delete a line from a combo box
    !
    ! CBOX |  c_ptr |  required |  The combo box to update
    ! INDEX |  c_int |  required |  The index of the choce to remove
    !
    ! Usually called via the generic hl_gtk_combo_box_delete interface.
    !-

    call gtk_combo_box_text_remove(cbox, index)

  end subroutine hl_gtk_combo_box_delete_single

  !+
  subroutine hl_gtk_combo_box_delete_multi(cbox, index)

    type(c_ptr), intent(in) :: cbox
    integer(c_int), dimension(:), intent(in) :: index

    ! Delete lines from a combo box
    !
    ! CBOX |  c_ptr |  required |  The combo box to update
    ! INDEX |  c_int() |  required |  The index of the choce to remove
    !
    ! Usually called via the generic hl_gtk_combo_box_delete interface.
    !-

    integer(c_int) :: i

    do i = size(index),1,-1
       call gtk_combo_box_text_remove(cbox, index(i))
    end do
  end subroutine hl_gtk_combo_box_delete_multi

  !+
  subroutine hl_gtk_combo_box_delete_all(cbox)

    type(c_ptr), intent(in) :: cbox

    ! Delete all lines from a combo box
    !
    ! CBOX |  c_ptr |  required |  The combo box to update
    !
    ! Usually called via the generic hl_gtk_combo_box_delete interface.
    !-

    call gtk_combo_box_text_remove_all(cbox)

  end subroutine hl_gtk_combo_box_delete_all
  !+
  function hl_gtk_combo_box_get_active(cbox, text, ftext) result(index)

    integer(c_int) :: index
    type(c_ptr), intent(in) :: cbox
    type(c_ptr), intent(out), optional :: text
    character(len=*), intent(out), optional :: ftext

    ! Get the selection from a combo box
    !
    ! CBOX |  c_ptr |  required |  The combo box to query.
    ! TEXT |  c_ptr |  optional |  C pointer to the text.
    ! FTEXT |  fstring |  optional |  The string as a Fortran string.
    !-

    type(c_ptr), target :: ctext

    index = gtk_combo_box_get_active(cbox)

    if (present(text) .or. present(ftext)) then
      ctext = gtk_combo_box_text_get_active_text(cbox)
       ! This is a bit ugly
       if (present(ftext)) &
            & call convert_c_string(ctext, ftext)

       if (present(text)) text=ctext
    end if
  end function hl_gtk_combo_box_get_active

  !+
  function hl_gtk_combo_box_n_entries(cbox) result(count)
    integer(c_int) :: count
    type(c_ptr), intent(in) :: cbox

    ! Find the number of entries in a combo box.
    !
    ! CBOX |  c_ptr |  required |  The combo box to query.
    !-

    type(c_ptr) :: model

    model = gtk_combo_box_get_model(cbox)
    count = gtk_tree_model_iter_n_children (model, C_NULL_PTR)

  end function hl_gtk_combo_box_n_entries

end module gtk_hl_combobox
