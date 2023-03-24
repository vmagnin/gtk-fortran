! This file is part of gtk-fortran, a GTK / Fortran interface library.
! Copyright (C) 2013 The gtk-fortran team
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
!------------------------------------------------------------------------------
! Contributed by: James Tappin, 2013-01-26
! Last modifications: vmagnin 2020-06-19 (GTK 4), 2023-03-24
!------------------------------------------------------------------------------

module v_handlers
  use, intrinsic :: iso_c_binding

  use gdk_pixbuf_hl
  use gtk_draw_hl
  use gtk_hl_chooser
  use gtk_hl_combobox

  !************************************
  ! GTK modules for hl_cairo_viewer.f90
  !************************************
  use cairo, only: cairo_status, cairo_status_to_string
  use gdk_pixbuf, only: gdk_pixbuf_get_height, gdk_pixbuf_get_width
  use gtk, only: gtk_combo_box_get_active, gtk_combo_box_set_active, &
       & gtk_widget_set_sensitive, gtk_window_set_child, &
       & gtk_widget_show, gtk_window_destroy, TRUE, FALSE, &
       & g_signal_connect, gtk_widget_get_name
  use g, only: g_timeout_add
  use gtk_sup, only: c_f_string_copy_alloc

  implicit none
  character(len=256), dimension(:), allocatable :: file_list
  integer(c_int) :: current_file
  type(c_ptr) :: tl_window, view, prev, next, select

contains

  subroutine delete_v (widget, gdata)  bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    call gtk_window_destroy(tl_window)
  end subroutine delete_v


  function show_at_start(select) bind(c)
    ! This function is needed to show image passed as argument
    ! in the command line.
    ! See https://github.com/vmagnin/gtk-fortran/issues/224
    integer(c_int) :: show_at_start
    type(c_ptr), value, intent(in) :: select

    call gtk_combo_box_set_active(select, current_file)
    ! This function will be launched only once:
    show_at_start = FALSE
  end function


  recursive subroutine show_image(widget, gdata)  bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    integer(c_int), pointer :: istep
    integer(c_int) :: nx, ny
    type(c_ptr) :: pixbuf
    character(len=120) :: errm=''
    character(:), allocatable :: widget_name

    if (.not. c_associated(view)) return

    call c_f_string_copy_alloc(gtk_widget_get_name(widget), widget_name)

    if (widget_name == "GtkApplicationWindow") then
       ! If the widget is the main window, it means
       ! it has been resized. We redraw the same file.
    else
      if (c_associated(gdata)) then
         call c_f_pointer(gdata, istep)
         current_file = current_file + istep
         call gtk_combo_box_set_active(select, current_file)
      else
         current_file = gtk_combo_box_get_active(widget)
         if (current_file < 0) return
      end if
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
    type(c_ptr), value, intent(in) :: widget, gdata
    character(len=256), dimension(:), allocatable :: new_files, tmp
    logical, pointer :: idelete
    integer(c_int) :: ipick, i

    ipick = hl_gtk_file_chooser_show(new_files, &
         & create=FALSE, multiple=TRUE, filter=["image/*"], &
         & parent=tl_window, all=TRUE)

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
    call gtk_widget_set_sensitive(next, &
                                & f_c_logical(current_file < size(file_list)-1))
  end subroutine add_files

  subroutine activate(app, gdata) bind(c)
    use gtk, only: gtk_application_window_new, gtk_window_set_title
    implicit none
    type(c_ptr), value, intent(in)  :: app, gdata
    integer(c_int) :: nfiles, i, istat
    integer(c_int) :: timeid
    integer(c_int), dimension(2), target :: direction = [-1, 1]
    logical, dimension(2), target :: iremove = [.false., .true.]
    type(c_ptr) :: scroll, base, jb, junk, cmsg
    character(len=120) :: err_msg

    nfiles = command_argument_count()
    if (nfiles > 0) then
       allocate(file_list(nfiles))
       do i = 1, nfiles
          call get_command_argument(i, value=file_list(i))
          print *, file_list(i)
       end do
       current_file = 0
    else
       current_file = -1
    end if

    tl_window = gtk_application_window_new(app)
    call gtk_window_set_title(tl_window, "Simple Image Viewer"//c_null_char)
    print *, "Note that you can pass filenames as arguments in the command line"

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

    if (nfiles == 0) call add_files(tl_window, c_loc(iremove(2)))

    ! Will show an image passed in the command line, after the
    ! termination of the activate subroutine:
    if (current_file >= 0) timeid = g_timeout_add(100_c_int, c_funloc(show_at_start), select)

    ! If the window is resized, its default_width property is modified
    ! and this signal means the image needs to be redrawn:
    call g_signal_connect(tl_window, "notify::default-width"//c_null_char, c_funloc(show_image))

  end subroutine activate
end module v_handlers


program hl_cairo_viewer
  ! A very simple image viewer
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
  use v_handlers

  implicit none
  type(c_ptr)        :: app

  app = hl_gtk_application_new("gtk-fortran.examples.hl_cairo_viewer"//c_null_char, &
                             & c_funloc(activate))
end program hl_cairo_viewer
