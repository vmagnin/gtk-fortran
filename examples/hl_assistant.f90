! Copyright (C) 2012
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
! Contributed by James Tappin.
! Last modification: vmagnin 2020-02-13

! Based on the C example given in"
! https://www.linuxquestions.org/linux/articles/Technical/New_GTK_Widgets_GtkAssistant

module as_handlers
  use gtk_hl
  use gtk_sup
  use iso_c_binding

  !********************************
  ! Gtk modules for hl_assistant.f90
  use g, only: g_usleep

  use gtk, only: gtk_container_add, gtk_entry_get_text, &
       & gtk_events_pending, gtk_label_new, gtk_main, gtk_main_iteration, &
       & gtk_main_quit, gtk_toggle_button_get_active, gtk_window_destroy, &
       & gtk_widget_set_sensitive, gtk_widget_show, gtk_init, TRUE, FALSE, &
       & GTK_ASSISTANT_PAGE_CONTENT, GTK_ASSISTANT_PAGE_INTRO, &
       & GTK_ASSISTANT_PAGE_CONFIRM, GTK_ASSISTANT_PAGE_PROGRESS, &
       & gtk_widget_set_halign, gtk_widget_set_valign, gtk_widget_set_hexpand, gtk_widget_set_vexpand, &
       & GTK_ALIGN_CENTER, GTK_ALIGN_FILL

  implicit none

  type(c_ptr) :: asstnt

contains
  subroutine destroy_asstnt(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    print *, "Exit called"
    call gtk_main_quit ()
  end subroutine destroy_asstnt

  subroutine asstnt_close(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    print *, "Completed"
    call gtk_window_destroy(widget)
    call gtk_main_quit ()
  end subroutine asstnt_close

  subroutine name_enter(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    type(c_ptr) :: ctext, page, ebox
    character(len=100) :: ftext
    if (c_associated(data)) then
       ebox = data
    else
       ebox = widget
    end if

    ctext = gtk_entry_get_text(ebox)
    call c_f_string(ctext, ftext)
    print *, "Entered name as:",trim(ftext)

    page = hl_gtk_assistant_get_current_page(asstnt)
    call hl_gtk_assistant_set_page_complete(asstnt, &
         & f_c_logical(len_trim(ftext) > 0))
  end subroutine name_enter

  subroutine check_tog(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    call hl_gtk_assistant_set_page_complete(asstnt, &
         & gtk_toggle_button_get_active(widget))

  end subroutine check_tog

  subroutine start_pb(widget, data) bind(c)
    type(c_ptr), value :: widget, data

    integer(kind=c_int) :: i, iev
    type(c_ptr) :: page

    call gtk_widget_set_sensitive(widget, FALSE)

    page = hl_gtk_assistant_get_current_page(asstnt)
    do i = 1, 10

       do
          if (.not. c_f_logical(gtk_events_pending ())) exit
          iev = gtk_main_iteration ()
       end do

       call g_usleep(500000_c_long)
       call hl_gtk_progress_bar_set(data, i, 10_c_int, string=TRUE)
    end do
    call hl_gtk_assistant_set_page_complete(asstnt, TRUE)

  end subroutine start_pb
end module as_handlers

program hl_assistant

  use as_handlers
  implicit none

  type(c_ptr) :: junk, jb, ebox, pbar

  call gtk_init()

  asstnt = hl_gtk_assistant_new(title="GtkAssistant Example"//c_null_char, &
       & destroy=c_funloc(destroy_asstnt), wsize=[450_c_int, 300_c_int], &
       & close=c_funloc(asstnt_close))

  ! Intro page
  junk = gtk_label_new("This is an example of a GtkAssistant"//c_new_line// &
       & "by clicking the forward button,"//c_new_line// &
       & " you can continue to the next section!"//c_null_char);
  
  call hl_gtk_assistant_add_page(asstnt, junk, GTK_ASSISTANT_PAGE_INTRO, &
       & page_title="Introduction"//c_null_char)

  ! Name entry
  jb = hl_gtk_box_new(horizontal=TRUE, spacing=5_c_int)
  call gtk_widget_set_halign (jb, GTK_ALIGN_FILL)
  call gtk_widget_set_valign (jb, GTK_ALIGN_CENTER)
  call gtk_widget_set_hexpand (jb, TRUE)
  call gtk_widget_set_vexpand (jb, FALSE)

  junk = gtk_label_new("Your name:"//c_null_char)
  call hl_gtk_box_pack(jb, junk, expand=FALSE)

  ebox = hl_gtk_entry_new(editable=TRUE, activate=c_funloc(name_enter))
  call hl_gtk_box_pack(jb, ebox)
  
  junk = hl_gtk_button_new("Apply"//c_null_char, clicked=c_funloc(name_enter), &
       & data=ebox)
  call hl_gtk_box_pack(jb, junk, expand=FALSE)

  call hl_gtk_assistant_add_page(asstnt, jb, GTK_ASSISTANT_PAGE_CONTENT)

  ! Check button
  junk = hl_gtk_check_button_new("Click to continue"//c_null_char, &
       &toggled = c_funloc(check_tog))
  call hl_gtk_assistant_add_page(asstnt, junk, GTK_ASSISTANT_PAGE_CONTENT, &
       & page_title="Click the Check Button"//c_null_char)

  ! Progress
  jb = hl_gtk_box_new(horizontal=TRUE)  
  call gtk_widget_set_halign (jb, GTK_ALIGN_FILL)
  call gtk_widget_set_valign (jb, GTK_ALIGN_CENTER)
  call gtk_widget_set_hexpand (jb, TRUE)
  call gtk_widget_set_vexpand (jb, FALSE)

  pbar = hl_gtk_progress_bar_new()
  call hl_gtk_box_pack(jb, pbar)

  junk = hl_gtk_button_new("Click to start"//c_null_char, &
       & clicked=c_funloc(start_pb), data=pbar)
  call hl_gtk_box_pack(jb, junk, expand=false)
  call hl_gtk_assistant_add_page(asstnt, jb, GTK_ASSISTANT_PAGE_PROGRESS, &
       & page_title="Applying"//c_null_char)

  ! Confirmation page
  junk = gtk_label_new ("Text has been entered in the label and"//c_new_line// &
       & "the combo box is clicked. If you are done, then"//c_new_line// &
       & "it is time to leave!"//c_null_char)
  call hl_gtk_assistant_add_page(asstnt, junk, GTK_ASSISTANT_PAGE_CONFIRM, &
       & page_title = "Completed?"//c_null_char)

  call gtk_widget_show(asstnt)
  call gtk_main()

end program hl_assistant
