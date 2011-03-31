module rb_handlers

  use fgtk_h_widgets

  implicit none

  type(c_ptr) :: box, window, qbut, group
  type(c_ptr),dimension(6) :: rbut

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    print *, "Exit called"
    call gtk_object_destroy(window)
    call gtk_main_quit ()
  end subroutine my_destroy
  
  function rb_toggle(widget, gdata) result(res) bind(c)
    type(c_ptr), value :: widget, gdata
    integer(kind=c_int) :: res

    integer(kind=c_int), pointer :: fdata
    integer(kind=c_int) :: sdata

    ! Don't do anything for the implicit release event.
    if (gtk_toggle_button_get_active(widget) == FALSE) return

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       print *, "Selected", fdata
    end if
    sdata = f_gtk_radio_get_select(group)
    print *, "Selection", sdata

    res=FALSE
  end function rb_toggle
end module rb_handlers

program radio

  ! RADIO
  ! Demo of a radio button group

 ! use fgtk_h_widgets
  use rb_handlers

  implicit none
  integer :: i
  integer(kind=c_int), dimension(6), target :: isel=[ (i-1,i=1,6) ]
  character(len=10) :: label
  logical :: changed

  ! Initialize GTK+
  call gtk_init()

  ! Create a window and a vertical box
  window = f_gtk_window('radios'//cnull, destroy=c_funloc(my_destroy))
  box = gtk_vbox_new(TRUE, 0)
  call gtk_container_add(window, box)

  ! make 6 radio buttons and put them into the box (the group is
  ! the list item that links the buttons together and is used for
  ! collective operations on the set of buttons.

  group=NULL

  do i=1,6
     write(label,"('Choice #',i0)") i-1
     rbut(i) = f_gtk_radio_button(group, trim(label)//cnull, &
          & toggled=c_funloc(rb_toggle), data=c_loc(isel(i)))
     call gtk_box_pack_start_defaults(box, rbut(i))
  end do

  ! Set a selection (3)
  call f_gtk_radio_set_select(group, 3)

  ! Make a "quit" button and put it in the box as well, then put the
  ! box in the window
  qbut = f_gtk_button('Quit'//cnull, clicked=c_funloc(my_destroy))
  call gtk_box_pack_start_defaults(box, qbut)

  ! Realize the hierarchy
  call gtk_widget_show_all(window)

  ! Event loop

  call gtk_main

end program radio
