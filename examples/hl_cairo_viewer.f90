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
! Contributed by: James Tappin
! Last modifications: vmagnin 2020-06-10 (GTK 4)
!------------------------------------------------------------------------------

module v_handlers
  use iso_c_binding

  use gdk_events
  use gtk_hl_container
  use gdk_pixbuf_hl
  use gtk_draw_hl
  use gtk_hl_chooser
  use gtk_sup

  !************************************
  ! Gtk modules for hl_cairo_viewer.f90
  !************************************
  use cairo, only: cairo_status, cairo_status_to_string
  use gdk_pixbuf, only: gdk_pixbuf_get_height, gdk_pixbuf_get_width
  use gtk, only: gtk_combo_box_get_active, gtk_combo_box_set_active, &
       & gtk_widget_set_sensitive, gtk_window_set_child, &
       & gtk_widget_show, gtk_init, TRUE, FALSE

  implicit none
  character(len=256), dimension(:), allocatable :: file_list
  integer(kind=c_int) :: current_file
  type(c_ptr) :: tl_window, view, prev, next, select
  type(c_ptr) :: my_gmainloop

contains
  subroutine delete_v (widget, gdata)  bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    call g_main_loop_quit(my_gmainloop)
  end subroutine delete_v

  recursive subroutine show_image(widget, gdata)  bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    integer(kind=c_int), pointer :: istep
    integer(kind=c_int) :: nx, ny
    type(c_ptr) :: pixbuf
    character(len=120) :: errm=''

    if (.not. c_associated(view)) return

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, istep)
       current_file = current_file + istep
       call gtk_combo_box_set_active(select, current_file)
    else
       current_file = gtk_combo_box_get_active(widget)
       if (current_file < 0) return
    end if

    call gtk_widget_set_sensitive(prev, f_c_logical(current_file > 0))
    call gtk_widget_set_sensitive(next, &
         & f_c_logical(current_file < size(file_list)-1))

    errm = ''
    pixbuf = hl_gdk_pixbuf_new(trim(file_list(current_file+1))//c_null_char, &
         & error=errm)
    if (errm /= "") then
       write(error_unit, "(2A)") "Failed to open: ", &
            & trim(file_list(current_file+1))
       write(error_unit, "(2A)") "        ", trim(errm)
    else
       nx = gdk_pixbuf_get_width(pixbuf)
       ny = gdk_pixbuf_get_height(pixbuf)

       call hl_gtk_drawing_area_resize(view, [nx, ny])
       call hl_gtk_drawing_area_draw_pixbuf(view, pixbuf)
    end if
  end subroutine show_image

  subroutine add_files(widget, gdata)  bind(c)
    type(c_ptr), value :: widget, gdata

    character(len=256), dimension(:), allocatable :: new_files, tmp
    logical, pointer :: idelete
    integer(kind=c_int) :: ipick, i

    ipick = hl_gtk_file_chooser_show(new_files, &
         & create=FALSE, multiple=TRUE, filter=["image/*"], &
         & all=TRUE)

    if (.not. c_f_logical(ipick)) return

    call c_f_pointer(gdata, idelete)

    if (idelete) then
       if (allocated(file_list)) deallocate(file_list)
       allocate(file_list(size(new_files)))
       file_list(:) = new_files(:)
       call hl_gtk_combo_box_delete(select)

    else
       allocate(tmp(size(file_list)))
       tmp(:) = file_list(:)
       if (allocated(file_list)) deallocate(file_list)
       allocate(file_list(size(tmp)+size(new_files)))
       file_list(:size(tmp)) = tmp(:)
       file_list(size(tmp)+1:) = new_files(:)
       if (current_file < 0) current_file = 0
    end if

    do i = 1, size(new_files)
       call hl_gtk_combo_box_add_text(select, trim(new_files(i))//c_null_char)
    end do

    if (current_file < 0 .and. size(file_list) > 0) current_file = 0
    call gtk_combo_box_set_active(select, current_file)
    call gtk_widget_set_sensitive(select, f_c_logical(size(file_list)>0))
  end subroutine add_files
end module v_handlers

program hl_cairo_viewer
  ! A very simple image viewer
  use v_handlers
  use iso_c_binding

  implicit none
  integer(kind=c_int) :: nfiles, i, istat
  integer(kind=c_int), dimension(2), target :: direction = [-1, 1]
  logical, dimension(2), target :: iremove = [.false., .true.]
  type(c_ptr) :: scroll, base, jb, junk, cmsg
  character(len=120) :: err_msg

  call gtk_init()

  nfiles = command_argument_count()
  if (nfiles > 0) then
     allocate(file_list(nfiles))
     do i = 1, nfiles
        call get_command_argument(i, value=file_list(i))
     end do
     current_file = 0
  else 
     current_file = -1
  end if

  tl_window = hl_gtk_window_new("Simple Image Viewer"//c_null_char, &
       & destroy=c_funloc(delete_v), resizable=FALSE)

  base = hl_gtk_box_new()
  call gtk_window_set_child(tl_window, base)

  view = hl_gtk_drawing_area_new(scroll=scroll, ssize=[600_c_int, 600_c_int], &
       & has_alpha=TRUE, cairo_status=istat)
  if (istat /= 0) then
     cmsg = cairo_status_to_string(istat)
     call c_f_string(cmsg, err_msg)
     write(error_unit, "(2a)") "hl_cairo_viewer: ", trim(err_msg)
     stop
  end if

  call hl_gtk_box_pack(base, scroll)

  jb = hl_gtk_box_new(horizontal=TRUE)
  call hl_gtk_box_pack(base, jb)

  prev = hl_gtk_button_new("< Prev"//c_null_char, &
       & clicked=c_funloc(show_image), data=c_loc(direction(1)), &
       & tooltip="Go to the previous image."//c_null_char,&
       & sensitive=FALSE)
  call hl_gtk_box_pack(jb, prev, expand=FALSE)

  select = hl_gtk_combo_box_new(changed=c_funloc(show_image), &
       & sensitive=f_c_logical(nfiles > 0), tooltip=&
       & "Select an image to show"//c_null_char)
  call hl_gtk_box_pack(jb, select, expand=TRUE)

  next = hl_gtk_button_new("Next >"//c_null_char, &
       & clicked=c_funloc(show_image), data=c_loc(direction(2)), &
       & tooltip="Go to the next image."//c_null_char, &
       & sensitive=f_c_logical(nfiles > 0))
  call hl_gtk_box_pack(jb, next, expand=FALSE)

  if (nfiles > 0) then 
     do i = 1, nfiles
        call hl_gtk_combo_box_add_text(select, &
             & trim(file_list(i))//c_null_char)
     end do
  end if

  junk = hl_gtk_button_new("Add files"//c_null_char, &
       & clicked=c_funloc(add_files), data=c_loc(iremove(1)), &
       & tooltip="Pick files to add to the list."//c_null_char)
  call hl_gtk_box_pack(jb, junk, expand=FALSE)

  junk = hl_gtk_button_new("Replace files"//c_null_char, &
       & clicked=c_funloc(add_files), data=c_loc(iremove(2)), &
       & tooltip="Pick files to replace the list."//c_null_char)
  call hl_gtk_box_pack(jb, junk, expand=FALSE)

  junk=hl_gtk_button_new("Quit"//c_null_char, &
       & clicked=c_funloc(delete_v), tooltip=&
       & "Quit the viewer."//c_null_char)
  call hl_gtk_box_pack(base, junk)

  call gtk_widget_show(tl_window)
  if (nfiles == 0) call add_files(c_null_ptr, c_loc(iremove(2)))
     
  if (current_file >= 0) call gtk_combo_box_set_active(select, current_file)

  my_gmainloop = g_main_loop_new(c_null_ptr, FALSE)
  call g_main_loop_run(my_gmainloop)

end program hl_cairo_viewer
