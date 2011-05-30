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

  character(len=256,kind=c_char)::filename
  character(len=256,kind=c_char)::working_dir, base_dir
  character(len=65000,kind=c_char)::fileinfo
  logical::files_written=.false.

! options
  logical::create_subdir=.true.
   
end module

module strings

  use widgets

  use gtk, only: CNULL

contains

! String routine from C_interface_module by Joseph M. Krahn
! http://fortranwiki.org/fortran/show/c_interface_module
! Copy a C string, passed as a char-array reference, to a Fortran string.
   subroutine C_F_string_chars(C_string, F_string)
    character(len=1,kind=C_char), intent(in) :: C_string(*)
    character(len=*), intent(out) :: F_string
    integer :: i
    i=1
    do while(C_string(i)/=CNULL .and. i<=len(F_string))
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
      do while(p_chars(i)/=CNULL .and. i<=len(F_string))
        F_string(i:i) = p_chars(i)
        i=i+1
      end do
      if (i<len(F_string)) F_string(i:) = ' '
    end if
  end subroutine C_F_string_ptr

end module strings

module connect

  use strings

  use gtk, only: gtk_builder_add_from_file, gtk_builder_connect_signals, gtk_buil&
  &der_get_object, gtk_builder_new, gtk_main, gtk_main_quit, gtk_widget_show,&
  &FALSE, CNULL, NULL, TRUE, gtk_init, gtk_builder_get_objects, gtk_builder_connect_signals_full,&
  gtk_buildable_get_name, gtk_text_view_get_buffer, gtk_text_buffer_set_text,&
  gtk_combo_box_get_active, gtk_combo_box_get_model, gtk_combo_box_get_active_iter,&
  gtk_tree_model_get_value, gtk_tree_model_iter_nth_child
  use g, only: g_object_unref, g_slist_length, g_slist_nth_data, g_object_get_property,&
  g_object_get_valist, g_value_get_string, g_mkdir_with_parents
  use gtk_hl, only: hl_gtk_file_chooser_show, gtktreeiter, gvalue
  
   implicit none

   type signal_connection
      character(len=64)::object_name
      character(len=64)::signal_name
      character(len=64)::handler_name
   end type signal_connection
      
   integer::n_connections
   type(signal_connection), dimension(:), allocatable::connections
   
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

  function delete_event (widget, event, gdata) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata
    ret = FALSE
  end function delete_event

  subroutine destroy (widget, gdata) bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    if (allocated(connections)) deallocate(connections)
    call gtk_main_quit ()
  end subroutine destroy

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
    type(c_ptr) :: gslist
    type(c_ptr) :: gpointer,object_name_ptr
    type(c_ptr) :: b
    character(len=128) :: F_string

    filters(1) = "*.glade"
    filtnames(1) = "Glade3 file"

    isel = hl_gtk_file_chooser_show(chfile, cdir=working_dir, create=FALSE,&
         & title="Select input file"//cnull, filter=filters, &
         & filter_name=filtnames, wsize=(/ 600, 400 /), edit_filters=TRUE, &
         & parent=window)

    if (isel == FALSE) return   ! No selection made

    filename = chfile(1)
    deallocate(chfile)
    
    files_written=.false.
    
    b = gtk_builder_new ()
    fileinfo=filename(1:len_trim(filename))
    guint = gtk_builder_add_from_file (b, filename(1:len_trim(filename))//CNULL, error)
    gslist = gtk_builder_get_objects(b)
    write(f_string,*) g_slist_length(gslist)," objects found"
    fileinfo=fileinfo(1:len_trim(fileinfo))//c_new_line//f_string
    do i=0, g_slist_length(gslist)-1
      gpointer=g_slist_nth_data (gslist,i)
      object_name_ptr=gtk_buildable_get_name (gpointer)
      call C_F_string_ptr(object_name_ptr, F_string)
      fileinfo=fileinfo(1:len_trim(fileinfo))//c_new_line//f_string
    enddo
    
    n_connections=0
    call gtk_builder_connect_signals_full (b, c_funloc(count_connections), NULL)  
    write(f_string,*) n_connections," signal connections found"
    fileinfo=fileinfo(1:len_trim(fileinfo))//c_new_line//f_string
    call g_object_unref (b)
    
    if (allocated(connections)) deallocate(connections)
    allocate(connections(n_connections))
    n_connections=0
    b = gtk_builder_new ()
    guint = gtk_builder_add_from_file (b, filename(1:len_trim(filename))//CNULL, error)
    call gtk_builder_connect_signals_full (b, c_funloc(get_connections), NULL)  
    call g_object_unref (b)
    call gtk_text_buffer_set_text (textbuffer, fileinfo(1:len_trim(fileinfo))//CNULL, -1)

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

    model = gtk_combo_box_get_model(license_selector)
    valid = gtk_tree_model_iter_nth_child(model, c_loc(iter), NULL, gtk_combo_box_get_active (combobox))
    val = c_loc(value)
    call gtk_tree_model_get_value(model, c_loc(iter), 1, val)
    textptr = g_value_get_string(val)
    call C_F_string_ptr(textptr, text)

  end subroutine combobox_get_active_string_value
  
  function write_files (widget, gdata ) result(ret)  bind(c)
    use iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    
    character(len=256,kind=c_char)::subdir, license_file, line,test
    integer::status_read
    integer::i,j
    logical::already_used
    
    call chdir(working_dir)
    subdir=filename(index(filename,"/",.true.)+1:index(filename,".",.true.)-1)
    if (create_subdir) then
      if (g_mkdir_with_parents (subdir(1:len_trim(subdir))//CNULL,488) .ge. 0) then
        working_dir=working_dir(1:len_trim(working_dir))//"/"//subdir
        call chdir(working_dir)
        open(50, file=filename(index(filename,"/",.true.)+1:len_trim(filename)), action='write')
        open(60, file=filename(1:len_trim(filename)), action='read')
        do
          read(60,'(A)',iostat=status_read) line
          if ( status_read /= 0 ) exit
          write(50,'(A)')line(1:len_trim(line))
        enddo
        close(60)
        close(50)
      else
        print*,"Unable to create subdirectory "//subdir
      endif
    endif

    call combobox_get_active_string_value(license_selector, 1, license_file)
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
    write(50,'(A)')"! gfortran gtk.f90 "//subdir(1:len_trim(subdir))//".f90 -o "//subdir(1:len_trim(subdir))//&
      " `pkg-config --cflags --libs gtk+-3.0` `pkg-config --cflags --libs gmodule-2.0`"
    write(50,'(A)')"!"
    write(50,'(A)')""

    write(50,'(A)')"module widgets"
    write(50,'(A)')"! declares the used GTK widgets"
    write(50,'(A)')"  use iso_c_binding"
    write(50,'(A)')"  implicit none"
    write(50,'(A)')""
    write(50,'(A)')"  type(c_ptr) :: window"
    write(50,'(A)')"  type(c_ptr) :: builder"
    write(50,'(A)')""
    write(50,'(A)')"end module"
    write(50,'(A)')""
    write(50,'(A)')""
    
    write(50,'(A)')"module handlers"
    write(50,'(A)')"  use gtk, only: gtk_builder_add_from_file, gtk_builder_connect_signals, gtk_buil&"
    write(50,'(A)')"  &der_get_object, gtk_builder_new, gtk_main, gtk_main_quit, gtk_widget_show,&"
    write(50,'(A)')"  &FALSE, CNULL, NULL, gtk_init"
    write(50,'(A)')"  use g, only: g_object_unref"

    write(50,'(A)')"  use widgets"
    write(50,'(A)')"  implicit none"
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
          write(50,'(A)')"    integer(c_int)    :: ret"
          write(50,'(A)')"    type(c_ptr), value :: widget, event, gdata"
        else
          write(50,'(A)')"  function "//connections(i)%handler_name(1:len_trim(connections(i)%handler_name))//&
            " (widget, gdata) result(ret) bind(c)"
          write(50,'(A)')"    use iso_c_binding, only: c_ptr, c_int"
          write(50,'(A)')"    integer(c_int)    :: ret"
          write(50,'(A)')"    type(c_ptr), value :: widget, gdata"
        endif
        write(50,'(A)')"!########## INSERT YOUR HANDLER CODE HERE ##########"
        write(50,'(A)')"print*,""handler function: "//connections(i)%handler_name(1:len_trim(connections(i)%handler_name))//""""
        write(50,'(A)')"!###################################################"
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
    write(50,'(A)')"  error = NULL"
    write(50,'(A)')""
    write(50,'(A)')"  ! Initialize the GTK+ Library"
    write(50,'(A)')"  call gtk_init ()"
    write(50,'(A)')""
    write(50,'(A)')"  ! create a new GtkBuilder object"
    write(50,'(A)')"  builder = gtk_builder_new ()"
    write(50,'(A)')""
    write(50,'(A)')"  ! parse the Glade3 XML file 'gtkbuilder.glade' and add it's contents to the GtkBuilder object"
    write(50,'(A)')"  guint = gtk_builder_add_from_file (builder, """//subdir(1:len_trim(subdir))//".glade""//CNULL, error)"
    write(50,'(A)')""
    write(50,'(A)')"  ! get a pointer to the GObject ""window"" from GtkBuilder"
    write(50,'(A)')"  window = gtk_builder_get_object (builder, ""window""//CNULL)"
    write(50,'(A)')""
    write(50,'(A)')"  ! use GModule to look at the applications symbol table to find the function name"
    write(50,'(A)')"  ! that matches the handler name specified in Glade3"
    write(50,'(A)')"  call gtk_builder_connect_signals (builder, NULL)"
    write(50,'(A)')""
    write(50,'(A)')"  ! free all memory used by XML stuff"     
    write(50,'(A)')"  call g_object_unref (builder)"
    write(50,'(A)')""
    write(50,'(A)')"  ! Show the Application Window"
    write(50,'(A)')"  call gtk_widget_show (window)"      
    write(50,'(A)')""
    write(50,'(A)')"  ! Enter the GTK+ Main Loop"
    write(50,'(A)')"  call gtk_main ()"
    write(50,'(A)')""
    write(50,'(A)')"end program "//subdir(1:len_trim(subdir))

    close(50)

    files_written=.true.
    ret = FALSE

  end function write_files
  
  subroutine default_options (widget, gdata ) bind(c)
    use iso_c_binding, only: c_ptr, c_int
    type(c_ptr), value :: widget, gdata
   
   filename="test.glade"
   call getcwd(working_dir)
   base_dir=working_dir
   create_subdir=.true.
 
  end subroutine default_options  
  
end module handlers

program gtkfsketcher
  
  use handlers
  
  implicit none

  integer(c_int) :: guint
  type(c_ptr) :: error
  error = NULL
  
  ! get default options
  call default_options (builder, error)
 
  ! Initialize the GTK+ Library
  call gtk_init ()

  ! create a new GtkBuilder object
  builder = gtk_builder_new ()
  
  ! parse the Glade3 XML file 'gtkbuilder.glade' and add it's contents to the GtkBuilder object
  guint = gtk_builder_add_from_file (builder, "gtkf-sketcher.glade"//CNULL, error)

  ! get a pointer to the GObject "window" from GtkBuilder
  window = gtk_builder_get_object (builder, "window"//CNULL)

  ! get a pointer to the file info text field buffer
  textbuffer = gtk_builder_get_object (builder, "fileinfo_buffer"//CNULL)
  
  ! get a pointer to the license selection combo box
  license_selector = gtk_builder_get_object (builder, "license"//CNULL)

  ! use GModule to look at the applications symbol table to find the function name 
  ! that matches the handler name we specified in Glade3
  call gtk_builder_connect_signals (builder, NULL)  

  ! free all memory used by XML stuff      
  call g_object_unref (builder)
  
  ! Show the Application Window      
  call gtk_widget_show (window)       
  
  ! Enter the GTK+ Main Loop
  call gtk_main ()
        
end program gtkfsketcher
