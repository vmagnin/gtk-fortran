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

! GTK+ Fortran Code Sketcher using Glade3 UI definitions
! gfortran gtk.f90 gtk-sup.f90 gtk-hl.f90 gtkf-sketcher.f90 -o gtkf-sketcher `pkg-config --cflags --libs gtk+-3.0` `pkg-config --cflags --libs gmodule-2.0`
! Contributed by Jens Hunger

module widgets
  ! declares the used GTK widgets
  use iso_c_binding
  implicit none

  type(c_ptr) :: window
  type(c_ptr) :: builder
  type(c_ptr) :: textbuffer
  type(c_ptr) :: license_selector
  type(c_ptr) :: appwindow_selector
  type(c_ptr) :: toplevel_widgets
  type(c_ptr) :: create_subdir_button
  type(c_ptr) :: create_handlerfiles_button
  type(c_ptr) :: overwrite_handlerfiles_button
  type(c_ptr) :: widget_symbols_button
  type(c_ptr) :: update_used_functions_button
  type(c_ptr) :: use_hl_gtk_button
  type(c_ptr) :: include_files_button

  character(len=256,kind=c_char)::filename
  character(len=256,kind=c_char)::working_dir, base_dir
  character(len=65000,kind=c_char)::fileinfo
  logical::files_written=.false.
  logical::file_loaded=.false.

! options
  logical::create_subdir=.true.
  logical::create_handlerfiles=.true.
  logical::overwrite_handlerfiles=.false.
  logical::widget_symbols=.false.
  logical::update_used_functions=.false.
  logical::use_hl_gtk=.true.
  logical::include_files=.true.
   
end module

module strings

  use widgets

  use gtk, only: c_null_char, TRUE, FALSE

contains

! String routine from C_interface_module by Joseph M. Krahn
! http://fortranwiki.org/fortran/show/c_interface_module
! Copy a C string, passed as a char-array reference, to a Fortran string.
   subroutine C_F_string_chars(C_string, F_string)
    character(len=1,kind=C_char), intent(in) :: C_string(*)
    character(len=*), intent(out) :: F_string
    integer :: i
    i=1
    do while(C_string(i)/=c_null_char .and. i<=len(F_string))
      F_string(i:i) = C_string(i)
      i=i+1
    end do
    if (i<len(F_string)) F_string(i:) = ' '
   end subroutine C_F_string_chars
   
! Copy a C string, passed by pointer, to a Fortran string.
! If the C pointer is NULL, the Fortran string is blanked.
! C_string must be NUL terminated, or at least as long as F_string.
! If C_string is longer, it is truncated. Otherwise, F_string is
! blank-padded at the end.
  subroutine C_F_string_ptr(C_string, F_string)
    type(C_ptr), intent(in) :: C_string
    character(len=*), intent(out) :: F_string
    character(len=1,kind=C_char), dimension(:), pointer :: p_chars
    integer :: i
    if (.not. C_associated(C_string)) then
      F_string = ' '
    else
      call C_F_pointer(C_string,p_chars,[huge(0)])
      i=1
      do while(p_chars(i)/=c_null_char .and. i<=len(F_string))
        F_string(i:i) = p_chars(i)
        i=i+1
      end do
      if (i<len(F_string)) F_string(i:) = ' '
    end if
  end subroutine C_F_string_ptr

  function gbool_equal_fbool(gbool,fbool) result(ret)
    integer(c_int), intent(in) :: gbool
    logical, intent(in) :: fbool
    logical :: ret    
    if (((gbool.eq.true).and.(fbool)).or.((gbool.eq.false).and.(.not.fbool))) then
      ret=.true.
    else
      ret=.false.
    endif
    write(*,*),gbool,fbool,ret
  end function gbool_equal_fbool
  
  function gbool(fbool) result(ret)
    logical, intent(in) :: fbool
    integer(c_int) :: ret    
    if (fbool) then
      ret=true
    else
      ret=false
    endif
  end function gbool

  function fbool(gbool) result(ret)
    integer(c_int), intent(in) :: gbool
    logical :: ret    
    if (gbool.eq.true) then
      ret=.true.
    else
      ret=.false.
    endif
  end function fbool

end module strings

module connect

  use strings

  use gtk, only: gtk_builder_add_from_file, gtk_builder_connect_signals, gtk_buil&
  &der_get_object, gtk_builder_new, gtk_main, gtk_main_quit, gtk_widget_show,&
  &FALSE, c_null_char, c_null_ptr, TRUE, gtk_init, gtk_builder_get_objects, gtk_builder_connect_signals_full,&
  gtk_buildable_get_name, gtk_text_view_get_buffer, gtk_text_buffer_set_text,&
  gtk_combo_box_get_active, gtk_combo_box_set_active, gtk_combo_box_get_model, gtk_combo_box_get_active_iter,&
  gtk_tree_model_get_value, gtk_tree_model_iter_nth_child,&
  gtk_toggle_button_get_active, gtk_toggle_button_set_active,GTK_BUTTONS_OK,&
  gtk_widget_is_toplevel, gtk_list_store_append, gtk_list_store_set_value, gtk_list_store_clear
  use g, only: g_object_unref, g_slist_length, g_slist_nth_data, g_object_get_property,&
  g_object_get_valist, g_value_get_string, g_mkdir_with_parents, g_value_init, g_value_set_string, g_value_unset
  use gtk_hl, only: hl_gtk_file_chooser_show, gtktreeiter, gvalue, hl_gtk_message_dialog_show, type_kind, G_TYPE_STRING
  
  implicit none

  type signal_connection
    character(len=64)::object_name
    character(len=64)::signal_name
    character(len=64)::handler_name
  end type signal_connection
      
  integer::n_connections
  type(signal_connection), dimension(:), allocatable::connections
  type(c_ptr) :: gslist !list containing the widgets 
  
  contains
   
  subroutine count_connections (builder, object, signal_name, handler_name, connect_object, flags, user_data) bind(c)
    use iso_c_binding, only: c_ptr, c_char, c_int
    type(c_ptr), value                     :: builder        !a GtkBuilder
    type(c_ptr), value                     :: object         !object to connect a signal to
    character(kind=c_char), dimension(*)   :: signal_name    !name of the signal
    character(kind=c_char), dimension(*)   :: handler_name   !name of the handler
    type(c_ptr), value                     :: connect_object !a GObject, if non-NULL, use g_signal_connect_object()
    integer(c_int), value                  :: flags          !GConnectFlags to use
    type(c_ptr), value                     :: user_data      !user data 
     
    n_connections=n_connections+1
  end subroutine count_connections

  subroutine get_connections (builder, object, signal_name, handler_name, connect_object, flags, user_data) bind(c)
    use iso_c_binding, only: c_ptr, c_char, c_int
    type(c_ptr), value                     :: builder        !a GtkBuilder
    type(c_ptr), value                     :: object         !object to connect a signal to
    character(kind=c_char), dimension(*)   :: signal_name    !name of the signal
    character(kind=c_char), dimension(*)   :: handler_name   !name of the handler
    type(c_ptr), value                     :: connect_object !a GObject, if non-NULL, use g_signal_connect_object()
    integer(c_int), value                  :: flags          !GConnectFlags to use
    type(c_ptr), value                     :: user_data      !user data 
      
    character(len=64)                      :: sname
    character(len=64)                      :: hname
    type(c_ptr)                            :: object_name_ptr
    character(len=64)                      :: oname
  
    call C_F_string_chars(signal_name, sname)
    call C_F_string_chars(handler_name, hname)
    object_name_ptr=gtk_buildable_get_name (object)
    if (.not. C_associated(object_name_ptr)) then
      oname="unknown"
    else
      call C_F_string_ptr(object_name_ptr, oname)
    endif
    fileinfo=fileinfo(1:len_trim(fileinfo))//c_new_line//"object: "//trim(adjustl(oname))//"  signal: "//&
       trim(adjustl(sname))//"  handler: "//trim(adjustl(hname))
    n_connections=n_connections+1
    connections(n_connections)%object_name=oname
    connections(n_connections)%signal_name=sname
    connections(n_connections)%handler_name=hname

  end subroutine get_connections
   
end module connect

module handlers

  use connect

  implicit none

contains

  subroutine copy_file(source,destination)
    character(len=100),intent(in) :: source
    character(len=100),intent(in) :: destination
    character(len=256,kind=c_char)::line
    integer::status_read
    open(50, file=destination, action='write')
    open(60, file=source, action='read')
    do
      read(60,'(A)',iostat=status_read) line
      if ( status_read /= 0 ) exit
      write(50,'(A)')line(1:len_trim(line))
    enddo
    close(60)
    close(50)
  end subroutine copy_file

  function delete_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata
    ret = FALSE
  end function delete_event

  subroutine destroy (widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    logical::lopened
    if (allocated(connections)) deallocate(connections)
    inquire(unit=99,opened=lopened)
    if (lopened) close(99)
    call gtk_main_quit ()
  end subroutine destroy

  subroutine create_subdir_toggled (widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    create_subdir=fbool(gtk_toggle_button_get_active(create_subdir_button))
    write(*,*)"subdir creation = ",create_subdir
  end subroutine create_subdir_toggled

  subroutine create_handlerfiles_toggled (widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    create_handlerfiles=fbool(gtk_toggle_button_get_active(create_handlerfiles_button))
    write(*,*)"handlerfiles creation = ",create_handlerfiles
  end subroutine create_handlerfiles_toggled

  subroutine overwrite_handlerfiles_toggled (widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    overwrite_handlerfiles=fbool(gtk_toggle_button_get_active(overwrite_handlerfiles_button))
    write(*,*)"handlerfiles overwrite = ",overwrite_handlerfiles
  end subroutine overwrite_handlerfiles_toggled

  subroutine widget_symbols_toggled (widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    widget_symbols=fbool(gtk_toggle_button_get_active(widget_symbols_button))
    write(*,*)"symbols for all widgets = ",widget_symbols
  end subroutine widget_symbols_toggled

  subroutine update_used_functions_toggled (widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    update_used_functions=fbool(gtk_toggle_button_get_active(update_used_functions_button))
    write(*,*)"update used functions = ",update_used_functions
  end subroutine update_used_functions_toggled

  subroutine use_hl_gtk_toggled (widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    use_hl_gtk=fbool(gtk_toggle_button_get_active(use_hl_gtk_button))
    write(*,*)"use high level interface = ",use_hl_gtk
  end subroutine use_hl_gtk_toggled

  subroutine include_files_toggled (widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    include_files=fbool(gtk_toggle_button_get_active(include_files_button))
    write(*,*)"generate include files = ",include_files
  end subroutine include_files_toggled

  function file_open (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    
    integer(kind=c_int) :: isel
    character(len=120), dimension(:), allocatable :: chfile
    character(len=30), dimension(2) :: filters
    character(len=30), dimension(2) :: filtnames
    character(len=200) :: inln
    integer :: ios
    integer :: idxs

    integer(c_int) :: guint, i
    type(c_ptr) :: error = c_null_ptr
    type(c_ptr) :: gpointer,object_name_ptr
    type(c_ptr) :: b
    character(len=128) :: F_string
    type(c_ptr) :: val
    type(gtktreeiter), target :: iter
    type(gvalue), target :: value
    integer(kind=type_kind) :: ctype

    filters(1) = "*.glade"
    filtnames(1) = "Glade3 file"

    isel = hl_gtk_file_chooser_show(chfile, cdir=working_dir, create=FALSE,&
         & title="Select input file"//c_null_char, filter=filters, &
         & filter_name=filtnames, wsize=(/ 600_c_int, 400_c_int /), edit_filters=TRUE, &
         & parent=window)

    if (isel == FALSE) return   ! No selection made

    filename = chfile(1)
    deallocate(chfile)
    
    files_written=.false.
    
    val = c_loc(value)
    val = g_value_init(val, G_TYPE_STRING)
    call gtk_list_store_clear(toplevel_widgets)
    
    b = gtk_builder_new ()
    fileinfo=filename(1:len_trim(filename))
    guint = gtk_builder_add_from_file (b, filename(1:len_trim(filename))//c_null_char, error)
    gslist = gtk_builder_get_objects(b)
    write(f_string,*) g_slist_length(gslist)," objects found"
    fileinfo=fileinfo(1:len_trim(fileinfo))//c_new_line//f_string
    do i=0, g_slist_length(gslist)-1
      gpointer=g_slist_nth_data (gslist,i)
      object_name_ptr=gtk_buildable_get_name (gpointer)
      call C_F_string_ptr(object_name_ptr, F_string)
      if (fbool(gtk_widget_is_toplevel(gpointer))) then
        call gtk_list_store_append (toplevel_widgets,c_loc(iter))
        call g_value_set_string(val, f_string(1:len_trim(f_string))//c_null_char)
        call gtk_list_store_set_value (toplevel_widgets,c_loc(iter),0_c_int,val)
      endif
      fileinfo=fileinfo(1:len_trim(fileinfo))//c_new_line//f_string
    enddo
    
    n_connections=0
    call gtk_builder_connect_signals_full (b, c_funloc(count_connections), c_null_ptr)  
    write(f_string,*) n_connections," signal connections found"
    fileinfo=fileinfo(1:len_trim(fileinfo))//c_new_line//f_string
    call g_object_unref (b)
    
    if (allocated(connections)) deallocate(connections)
    allocate(connections(n_connections))
    n_connections=0
    b = gtk_builder_new ()
    guint = gtk_builder_add_from_file (b, filename(1:len_trim(filename))//c_null_char, error)
    call gtk_builder_connect_signals_full (b, c_funloc(get_connections), c_null_ptr)  
    call g_object_unref (b)
    call gtk_text_buffer_set_text (textbuffer, fileinfo(1:len_trim(fileinfo))//c_null_char, -1_c_int)

    call gtk_combo_box_set_active(appwindow_selector,0_c_int)
    call g_value_unset(val)
    
    file_loaded=.true.
    
    ret = FALSE

  end function file_open

  subroutine combobox_get_active_string_value(combobox,column,text)
    type(c_ptr)       :: combobox
    integer(c_int)    :: column
    character(len=256,kind=c_char)::text
 
    type(c_ptr):: model, val, textptr
    type(gtktreeiter), target :: iter
    integer(kind=c_int) :: valid
    type(gvalue), target :: value

    model = gtk_combo_box_get_model(combobox)
    valid = gtk_tree_model_iter_nth_child(model, c_loc(iter), c_null_ptr, gtk_combo_box_get_active (combobox))
    val = c_loc(value)
    call gtk_tree_model_get_value(model, c_loc(iter), column, val)
    textptr = g_value_get_string(val)
    call C_F_string_ptr(textptr, text)

  end subroutine combobox_get_active_string_value
  
  function write_files (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    
    character(len=256,kind=c_char)::subdir, license_file, line, test, handlerfile, appwindow, additional_modules
    integer::status_read
    integer(kind=c_int)::i,j
    logical::already_used, lexist
    type(c_ptr) :: gpointer,object_name_ptr
    character(len=128) :: f_string, f_string_ori
    
    if (.not.file_loaded) then
      status_read=hl_gtk_message_dialog_show((/"Please load some Glade3 UI file first!"/), GTK_BUTTONS_OK, &
        title="No Glade3 file loaded yet")
      return
    else
      call chdir(working_dir)
      subdir=filename(index(filename,"/",.true.)+1:index(filename,".",.true.)-1)
      if (create_subdir) then
        if (g_mkdir_with_parents (subdir(1:len_trim(subdir))//c_null_char,488_c_int) .ge. 0) then
          working_dir=working_dir(1:len_trim(working_dir))//"/"//subdir
          call chdir(working_dir)
          call copy_file(filename(1:len_trim(filename)),filename(index(filename,"/",.true.)+1:len_trim(filename)))
        else
          print*,"Unable to create subdirectory "//subdir
        endif
      endif

      call combobox_get_active_string_value(license_selector, 1_c_int, license_file)
      license_file=adjustl(license_file)

      open(50, file=subdir(1:len_trim(subdir))//".f90", action='write')

      write(50,'(A)')"! "//subdir(1:len_trim(subdir))//" main program generated by gtkf-sketcher, "//fdate()
      write(50,'(A)')"!"
      write(50,'(A)')"! gtkf-sketcher is part of the gtk-fortran GTK+ Fortran Interface Library."
      write(50,'(A)')"!"
      write(50,'(A)')"!"
      open(60, file=base_dir(1:len_trim(base_dir))//"/data/"//license_file(1:len_trim(license_file)), action='read')
      do
        read(60,'(A)',iostat=status_read) line
        if ( status_read /= 0 ) exit
        write(50,'(A)')"! "//line(1:len_trim(line))
      enddo
      close(60)
    
      write(50,'(A)')"!"
      write(50,'(A)')"!"
      write(50,'(A)')"! Compile with:"
      if (use_hl_gtk) then
        additional_modules="gtk-sup.f90 gtk-hl.f90"
      else
        additional_modules=""
      endif
      write(50,'(A)')"! gfortran gtk.f90 "//additional_modules(1:len_trim(additional_modules))//" "//&
        subdir(1:len_trim(subdir))//".f90 -o "//subdir(1:len_trim(subdir))//&
        " `pkg-config --cflags --libs gtk+-3.0` `pkg-config --cflags --libs gmodule-2.0`"
      write(50,'(A)')"!"
      write(50,'(A)')""

      write(50,'(A)')"module widgets"
      write(50,'(A)')"! declares the used GTK widgets"
      write(50,'(A)')"  use iso_c_binding"
      write(50,'(A)')"  implicit none"
      write(50,'(A)')""
      if (widget_symbols) then
        do i=0, g_slist_length(gslist)-1
          gpointer=g_slist_nth_data (gslist,i)
          object_name_ptr=gtk_buildable_get_name (gpointer)
          call C_F_string_ptr(object_name_ptr, F_string)
          if (len_trim(f_string).gt.0) then
            do
              j=index(f_string,"-")
              if (j.gt.0) then
                f_string(j:j)="_"
              else
                exit
              endif
            enddo
            write(50,'(A)')"  type(c_ptr) :: "//f_string(1:len_trim(f_string))
          endif
        enddo
      else
        write(50,'(A)')"  type(c_ptr) :: window"
      endif
      write(50,'(A)')"  type(c_ptr) :: builder"
      write(50,'(A)')""
      write(50,'(A)')"end module"
      write(50,'(A)')""
      write(50,'(A)')""
      
      write(50,'(A)')"module handlers"
      write(50,'(A)')"  use gtk, only: gtk_builder_add_from_file, gtk_builder_connect_signals, gtk_buil&"
      write(50,'(A)')"  &der_get_object, gtk_builder_new, gtk_main, gtk_main_quit, gtk_widget_show,&"
      write(50,'(A)')"  &FALSE, c_null_char, c_null_ptr, gtk_init"
      write(50,'(A)')"  use g, only: g_object_unref"
      if (update_used_functions) then
        call chdir(base_dir(1:len_trim(base_dir))//"/../src")
        call system("./usemodules.py "//working_dir)
        open (40, file="usemodules.txt", action='read')
        do
          read(40,'(A)',iostat=status_read) line
          if ( status_read /= 0 ) exit
          if (index(line,"handler").gt.0) then
            read(40,'(A)',iostat=status_read) line
            do
              read(40,'(A)',iostat=status_read) line
              if ((status_read /= 0).or.(len_trim(line).eq.0)) exit
              write(50,'(A)')"  "//line(1:len_trim(line))
            enddo
          endif
        enddo
        close (40)
        call chdir(working_dir)
      endif
      
      if (use_hl_gtk) then
        write(50,'(A)')"  use gtk_hl"
      endif
  
      if (include_files) then
        inquire(file=subdir(1:len_trim(subdir))//"_used_modules.inc",exist=lexist)
        if ((.not.lexist).or.(overwrite_handlerfiles)) then
          open(70,file=subdir(1:len_trim(subdir))//"_used_modules.inc",action='write')
          write(70,'(A)')"! Additionally used modules for "//subdir(1:len_trim(subdir))
          write(70,'(A)')"!########## INSERT YOUR USE STATEMENTS HERE ##########"
          write(70,'(A)')""
          write(70,'(A)')"!#####################################################"
          close(70)
        endif
        write(50,'(A)')"  include """//subdir(1:len_trim(subdir))//"_used_modules.inc"""
      endif
        
      write(50,'(A)')"  use widgets"
      write(50,'(A)')"  implicit none"
      write(50,'(A)')""

      if (include_files) then
        inquire(file=subdir(1:len_trim(subdir))//"_global_variables.inc",exist=lexist)
        if ((.not.lexist).or.(overwrite_handlerfiles)) then
          open(70,file=subdir(1:len_trim(subdir))//"_global_variables.inc",action='write')
          write(70,'(A)')"! Global variables for "//subdir(1:len_trim(subdir))
          write(70,'(A)')"!########## INSERT YOUR DECLARATIONS HERE ##########"
          write(70,'(A)')""
          write(70,'(A)')"!###################################################"
          close(70)
        endif
        write(50,'(A)')"  include """//subdir(1:len_trim(subdir))//"_global_variables.inc"""
      endif

      write(50,'(A)')""
      write(50,'(A)')"contains"
      write(50,'(A)')"  !*************************************"
      write(50,'(A)')"  ! User defined event handlers go here"
      write(50,'(A)')"  !*************************************"

      do i=1,n_connections
        already_used=.false.
        if (i.gt.1) then
          do j=1,i-1
            if (connections(i)%handler_name.eq.connections(j)%handler_name) then
              already_used=.true.
              exit
            endif
          enddo
        endif
        if (.not.already_used) then
          write(50,'(A)')"! handler function for signal "//connections(i)%signal_name(1:len_trim(connections(i)%signal_name))//&
            " ("//connections(i)%object_name(1:len_trim(connections(i)%object_name))//")"
          if (index(connections(i)%signal_name,"event").gt.0) then
            write(50,'(A)')"  function "//connections(i)%handler_name(1:len_trim(connections(i)%handler_name))//&
              " (widget, event, gdata) result(ret) bind(c)"
            write(50,'(A)')"    use iso_c_binding, only: c_ptr, c_int"
            write(50,'(A)')"    integer(c_int)     :: ret"
            write(50,'(A)')"    type(c_ptr), value :: widget, event, gdata"
          else
            write(50,'(A)')"  function "//connections(i)%handler_name(1:len_trim(connections(i)%handler_name))//&
              " (widget, gdata) result(ret) bind(c)"
            write(50,'(A)')"    use iso_c_binding, only: c_ptr, c_int"
            write(50,'(A)')"    integer(c_int)     :: ret"
            write(50,'(A)')"    type(c_ptr), value :: widget, gdata"
          endif
          if (create_handlerfiles) then
            handlerfile="handler_"//connections(i)%handler_name(1:len_trim(connections(i)%handler_name))//".f90"
            write(50,'(A)')"!########## INSERT YOUR HANDLER CODE IN FILE "//handlerfile(1:len_trim(handlerfile))//" ##########"
            write(50,'(A)')"    INCLUDE '"//handlerfile(1:len_trim(handlerfile))//"'"
            inquire(file=handlerfile,exist=lexist)
            if ((.not.lexist).or.(overwrite_handlerfiles)) then
              open(70,file=handlerfile,action='write')
              write(70,'(A)')"! handler for signal "//connections(i)%signal_name(1:len_trim(connections(i)%signal_name))//&
                " ("//connections(i)%object_name(1:len_trim(connections(i)%object_name))//")"
              write(70,'(A)')"!########## INSERT YOUR HANDLER CODE HERE ##########"
              write(70,'(A)')"print*,""handler function: "//connections(i)%handler_name(1:len_trim(connections(i)%handler_name))//&
                """"
              write(70,'(A)')"!###################################################"
              close(70)
            endif
          else
            write(50,'(A)')"!########## INSERT YOUR HANDLER CODE HERE ##########"
            write(50,'(A)')"print*,""handler function: "//connections(i)%handler_name(1:len_trim(connections(i)%handler_name))//""""
            write(50,'(A)')"!###################################################"
          endif
          write(50,'(A)')"    ret = FALSE"
          write(50,'(A)')"  end function "//connections(i)%handler_name(1:len_trim(connections(i)%handler_name))
          write(50,'(A)')""
        endif
      enddo
      write(50,'(A)')"end module handlers"
      write(50,'(A)')""
      write(50,'(A)')""
      write(50,'(A)')"program "//subdir(1:len_trim(subdir))
      write(50,'(A)')""
      write(50,'(A)')"  use handlers"
      write(50,'(A)')""
      write(50,'(A)')"  implicit none"
      write(50,'(A)')""
      write(50,'(A)')"  integer(c_int) :: guint"
      write(50,'(A)')"  type(c_ptr) :: error"
      write(50,'(A)')"  error = c_null_ptr"
      write(50,'(A)')""
      write(50,'(A)')"  ! Initialize the GTK+ Library"
      write(50,'(A)')"  call gtk_init ()"
      write(50,'(A)')""
      write(50,'(A)')"  ! create a new GtkBuilder object"
      write(50,'(A)')"  builder = gtk_builder_new ()"
      write(50,'(A)')""
      write(50,'(A)')"  ! parse the Glade3 XML file 'gtkbuilder.glade' and add it's contents to the GtkBuilder object"
      write(50,'(A)')"  guint = gtk_builder_add_from_file (builder, """//subdir(1:len_trim(subdir))//".glade""//c_null_char, error)"
      write(50,'(A)')""
      call combobox_get_active_string_value(appwindow_selector, 0_c_int, appwindow)
      if (widget_symbols) then
        write(50,'(A)')"  ! get pointers to all GObjects from GtkBuilder"
        do i=0, g_slist_length(gslist)-1
          gpointer=g_slist_nth_data (gslist,i)
          object_name_ptr=gtk_buildable_get_name (gpointer)
          call C_F_string_ptr(object_name_ptr, F_string)
          if (len_trim(f_string).gt.0) then
            f_string_ori=f_string
            do
              j=index(f_string,"-")
              if (j.gt.0) then
                f_string(j:j)="_"
              else
                exit
              endif
            enddo
            write(50,'(A)')"  "//f_string(1:len_trim(f_string))//" = gtk_builder_get_object (builder, """//&
              f_string_ori(1:len_trim(f_string_ori))//"""//c_null_char)"
          endif
        enddo
      else
        write(50,'(A)')"  ! get a pointer to the application window """//appwindow(1:len_trim(appwindow))//&
          """ from GtkBuilder"
        write(50,'(A)')"  "//appwindow(1:len_trim(appwindow))//" = gtk_builder_get_object (builder, """//&
          appwindow(1:len_trim(appwindow))//"""//c_null_char)"
      endif
      write(50,'(A)')""
      write(50,'(A)')"  ! use GModule to look at the applications symbol table to find the function name"
      write(50,'(A)')"  ! that matches the handler name specified in Glade3"
      write(50,'(A)')"  call gtk_builder_connect_signals (builder, c_null_ptr)"
      write(50,'(A)')""
      write(50,'(A)')"  ! free all memory used by XML stuff"     
      write(50,'(A)')"  call g_object_unref (builder)"
      write(50,'(A)')""
      write(50,'(A)')"  ! show the application window"
      write(50,'(A)')"  call gtk_widget_show ("//appwindow(1:len_trim(appwindow))//")"      
      write(50,'(A)')""
      write(50,'(A)')"  ! enter the GTK+ main loop"
      write(50,'(A)')"  call gtk_main ()"
      write(50,'(A)')""
      write(50,'(A)')"end program "//subdir(1:len_trim(subdir))
      close(50)
      files_written=.true.
    endif
    
    ret = FALSE
  end function write_files

  subroutine save_default_options (widget, gdata ) bind(c)
    use iso_c_binding, only: c_ptr, c_int
    type(c_ptr), value :: widget, gdata
    character(len=20)::defaultsfile="default.options"
   
    open(111,file=base_dir(1:len_trim(base_dir))//"/"//defaultsfile, action='write')
    write(111,'(7L2)')create_subdir,create_handlerfiles,overwrite_handlerfiles,widget_symbols,update_used_functions,&
      use_hl_gtk,include_files
    write(111,'(I2)')gtk_combo_box_get_active(license_selector)
    close(111)
 
  end subroutine save_default_options  

  subroutine load_default_options
    character(len=20)::defaultsfile="default.options"
    integer(c_int) ::license_no
   
    open(111,file=base_dir(1:len_trim(base_dir))//"/"//defaultsfile, action='read')
    read(111,'(7L2)')create_subdir,create_handlerfiles,overwrite_handlerfiles,widget_symbols,update_used_functions,&
      use_hl_gtk,include_files
    read(111,'(I2)')license_no
    call gtk_combo_box_set_active(license_selector,license_no)
    close(111)
 
  end subroutine load_default_options  
  
  subroutine default_options (widget, gdata ) bind(c)
    use iso_c_binding, only: c_ptr, c_int
    type(c_ptr), value :: widget, gdata
   
    filename="example.glade"
    call getcwd(working_dir)
    call load_default_options
    call gtk_toggle_button_set_active (create_subdir_button, gbool(create_subdir))
    call gtk_toggle_button_set_active (create_handlerfiles_button, gbool(create_handlerfiles))
    call gtk_toggle_button_set_active (overwrite_handlerfiles_button, gbool(overwrite_handlerfiles))
    call gtk_toggle_button_set_active (widget_symbols_button, gbool(widget_symbols))
    call gtk_toggle_button_set_active (update_used_functions_button, gbool(update_used_functions))
    call gtk_toggle_button_set_active (use_hl_gtk_button, gbool(use_hl_gtk))
    call gtk_toggle_button_set_active (include_files_button, gbool(include_files))
     
  end subroutine default_options  
  
end module handlers

program gtkfsketcher
  
  use handlers
  
  implicit none

  integer(c_int) :: guint
  type(c_ptr) :: error
  error = c_null_ptr

  call getcwd(base_dir)
  open(99, file="gtkf-sketcher.log", action='write')

  ! Initialize the GTK+ Library
  call gtk_init ()

  ! create a new GtkBuilder object
  builder = gtk_builder_new ()
  
  ! parse the Glade3 XML file 'gtkbuilder.glade' and add it's contents to the GtkBuilder object
  guint = gtk_builder_add_from_file (builder, "gtkf-sketcher.glade"//c_null_char, error)

  ! get a pointer to the GObject "window" from GtkBuilder
  window = gtk_builder_get_object (builder, "window"//c_null_char)

  ! get a pointer to the file info text field buffer
  textbuffer = gtk_builder_get_object (builder, "fileinfo_buffer"//c_null_char)
  
  ! get a pointer to the selection combo boxes
  license_selector = gtk_builder_get_object (builder, "license"//c_null_char)
  appwindow_selector = gtk_builder_get_object (builder, "appwindow"//c_null_char)
  toplevel_widgets = gtk_builder_get_object (builder, "toplevel_widgets"//c_null_char)

  ! get pointers to the option check buttons
  create_subdir_button = gtk_builder_get_object (builder, "create_subdir"//c_null_char)
  create_handlerfiles_button = gtk_builder_get_object (builder, "create_handlerfiles"//c_null_char)
  overwrite_handlerfiles_button = gtk_builder_get_object (builder, "overwrite_handlerfiles"//c_null_char)
  widget_symbols_button = gtk_builder_get_object (builder, "widget_symbols"//c_null_char)
  update_used_functions_button = gtk_builder_get_object (builder, "update_used_functions"//c_null_char)
  use_hl_gtk_button = gtk_builder_get_object (builder, "use_hl_gtk"//c_null_char)
  include_files_button = gtk_builder_get_object (builder, "include_files"//c_null_char)

  ! get default options
  call default_options (builder, error)
 
  ! use GModule to look at the applications symbol table to find the function name 
  ! that matches the handler name we specified in Glade3
  call gtk_builder_connect_signals (builder, c_null_ptr)  

  ! free all memory used by XML stuff      
  call g_object_unref (builder)
  
  ! Show the Application Window      
  call gtk_widget_show (window)       
  
  ! Enter the GTK+ Main Loop
  call gtk_main ()
        
end program gtkfsketcher
