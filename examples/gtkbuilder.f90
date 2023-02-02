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
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! Jens Hunger, 03-27-2011
! Last modified: 04-01-2011

module widgets
  ! declares the used GTK widgets
  use, intrinsic :: iso_c_binding
  implicit none

  type(c_ptr) :: window
  type(c_ptr) :: builder

end module

module handlers
  ! This module is just copied from gtkhello2.f90
  use gtk, only: gtk_builder_add_from_file, gtk_builder_connect_signals, gtk_buil&
  &der_connect_signals_full, gtk_builder_get_object, gtk_builder_new, gtk_main, g&
  &tk_main_quit, gtk_widget_show,&
  &FALSE, c_null_char, c_null_ptr, gtk_init, g_signal_connect
  use g, only: g_object_unref, g_signal_connect_object
  use widgets
  implicit none

contains
  !*************************************
  ! User defined event handlers go here
  !*************************************
  ! Note that events are a special type of signals, coming from the
  ! X Window system. Then callback functions must have an event argument:
  function delete_event (widget, event, gdata) result(ret)  bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, event, gdata
    print *, "my delete_event"
    ret = FALSE
  end function delete_event

  ! "destroy" is a GtkObject signal
  subroutine destroy (widget, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    print *, "my destroy"
    call gtk_main_quit ()
  end subroutine destroy

  ! "clicked" is a GtkButton signal
  function hello (widget, gdata ) result(ret)  bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "Hello World!"
    ret = FALSE
  end function hello

  function button1clicked (widget, gdata ) result(ret)  bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata
    print *, "Button 1 clicked!"
    ret = FALSE
  end function button1clicked

  function button2clicked (widget, gdata ) result(ret)  bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int
    integer(c_int)    :: ret
    type(c_ptr), value :: widget, gdata

    integer, pointer :: val

    print *, "Button 2 clicked!"
    ret = FALSE
    if (c_associated(gdata)) then
       call c_f_pointer(gdata, val)
       print *, "Value =", val
       val = val + 1
    end if
  end function button2clicked

end module handlers

module connect
! replacement for gtk_builder_connect_signals from GModule (see also gtkbuilder2.f90)
   use handlers
   implicit none
   
   type handler
      character(kind=c_char,len=30) ::handler_name
      type(C_FUNPTR) :: handler_ptr
   end type handler
   
   type(handler),dimension(5)::h=(/&
    handler("delete_event"//c_null_char, c_null_funptr),&
    handler("destroy"//c_null_char, c_null_funptr),&
    handler("hello"//c_null_char, c_null_funptr),&
    handler("button1clicked"//c_null_char, c_null_funptr),&
    handler("button2clicked"//c_null_char, c_null_funptr)&
    /)

   logical::handlers_initialized=.false.

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
  
!void        (*GtkBuilderConnectFunc)        (GtkBuilder *builder,
!                                             GObject *object,
!                                             const gchar *signal_name,
!                                             const gchar *handler_name,
!                                             GObject *connect_object,
!                                             GConnectFlags flags,
!                                             gpointer user_data);
!This is the signature of a function used to connect signals. 
!It is used by the gtk_builder_connect_signals() and gtk_builder_connect_signals_full() methods. 
!It is mainly intended for interpreted language bindings, but could be useful where the programmer wants 
!more control over the signal connection process.

   subroutine connect_signals (builder, object, signal_name, handler_name, connect_object, flags, user_data) bind(c)
      use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int
      type(c_ptr), value                     :: builder        !a GtkBuilder
      type(c_ptr), value                     :: object         !object to connect a signal to
      character(kind=c_char), dimension(*)   :: signal_name    !name of the signal
      character(kind=c_char), dimension(*)   :: handler_name   !name of the handler
      type(c_ptr), value                     :: connect_object !a GObject, if non-NULL, use g_signal_connect_object()
      integer(c_int), value                  :: flags          !GConnectFlags to use
      type(c_ptr), value                     :: user_data      !user data 
      
      integer                                :: i
      character(len=30)                      :: name1, name2

  ! this is necessary because gfortran gives error on using c_funloc in an initialization expression:
  ! Function 'c_funloc' in initialization expression at must be an intrinsic function
  ! and g95 e.g.:
  ! Variable 'destroy' cannot appear in an initialization expression
      if (.NOT.(handlers_initialized)) then
         h(1)%handler_ptr=c_funloc(delete_event)
         h(2)%handler_ptr=c_funloc(destroy)
         h(3)%handler_ptr=c_funloc(hello)
         h(4)%handler_ptr=c_funloc(button1clicked)
         h(5)%handler_ptr=c_funloc(button2clicked)
         handlers_initialized=.true.
      endif

      call C_F_string_chars(handler_name, name1)
      print*,"connecting signal "//name1
      do i=1,size(h)
         call C_F_string_chars(h(i)%handler_name, name2)
         if (name1 .eq. name2) then
            call g_signal_connect (object, signal_name, h(i)%handler_ptr)
            exit
         endif
      enddo

   end subroutine connect_signals
   
end module connect

program gtkbuilder
  
  use connect
  
  implicit none

  integer(c_int) :: guint
  type(c_ptr) :: error
  error=c_null_ptr

  ! Initialize the GTK+ Library
  call gtk_init ()

  ! create a new GtkBuilder object
  builder = gtk_builder_new ()

  ! parse the Glade3 XML file 'gtkbuilder.glade' and add its contents to the GtkBuilder object
  guint = gtk_builder_add_from_file (builder, "gtkbuilder.glade"//c_null_char, error)
  if (guint == 0) then
     print *, "Could not open gtkbuilder.glade"
     stop
  end if

  ! get a pointer to the GObject "window" from GtkBuilder
  window = gtk_builder_get_object (builder, "window"//c_null_char)
  
  ! connect signals to objects using the subroutine "connect_signals" in the module "connect"
  call gtk_builder_connect_signals_full (builder, c_funloc(connect_signals), c_null_ptr)  
     
  ! free all memory used by XML stuff      
  call g_object_unref (builder)
  
  ! Show the Application Window      
  call gtk_widget_show (window)       
  
  ! Enter the GTK+ Main Loop
  call gtk_main ()
        
end program gtkbuilder
