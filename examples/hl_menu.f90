module handlers
  use fgtk_h_widgets

  implicit none

  type(c_ptr) :: win, box, menubar, qbut,lab, smnu,mba
  type(c_ptr), dimension(10) :: mbuts
  type(c_ptr) :: mnu2, sm1, sm2
  type(c_ptr), dimension(4) :: mb1, mb2

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    print *, "Exit called"
    call gtk_object_destroy(win)
    call gtk_main_quit ()
  end subroutine my_destroy

  function mbut_act(widget, gdata) result(res) bind(c)
    integer(kind=c_int) :: res
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int), pointer :: fdata

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       if (fdata < 0) then
          print *, "Chose: Extra"
       else
          print *, "Chose:",fdata
       end if
    end if

    res=FALSE
  end function mbut_act

  function sm1_act(widget, gdata) result(res) bind(c)
    integer(kind=c_int) :: res
    type(c_ptr), value :: widget, gdata
    integer(kind=c_int), pointer :: fdata

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       print *, "Selected:",fdata
    end if
    res=FALSE
  end function sm1_act
  function sm2_act(widget, gdata) result(res) bind(c)
    integer(kind=c_int) :: res
    type(c_ptr), value :: widget, gdata
    integer(kind=c_int), pointer :: fdata

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       print *, "Subselected:",fdata
    end if
    res=FALSE
  end function sm2_act

end module handlers

program menu_test

  ! MENU_TEST
  ! Demo of menubars

  use handlers ! Implicitly includes other needed modules

  implicit none

  integer :: i
  integer(kind=c_int), dimension(10), target :: mclicks = [ (i-1, i=1,10) ]
  integer(kind=c_int), dimension(4), target :: mc1 = [ (i-1, i=1,4) ], &
       & mc2 = [ (i-1, i=1,4) ]

  integer(kind=c_int), target :: mca = -1
  character(len=12) :: holder

  ! Initialize gtk
  call gtk_init()

  ! Make a window for the hierarchy
  win = f_gtk_window("Menu Demo"//cnull, destroy=c_funloc(my_destroy))

  ! Make a vertical box, and add a label to it
  box=gtk_vbox_new(FALSE, 0)
  call gtk_container_add(win, box)

  lab = gtk_label_new("Menu Example"//cnull)
  call gtk_box_pack_start_defaults(box, lab)

  ! Make a menubar with the buttons horizontally aranged and put it in the
  ! box
  menubar = f_gtk_menu(GTK_PACK_DIRECTION_LTR)
  call gtk_box_pack_start_defaults(box, menubar)

  ! Make a submenu in the first (0) location
  smnu = f_gtk_menu_submenu(menubar, "Choose"//cnull)

  ! Populate the submenu with buttons
  do i = 1, size(mbuts)
     write(holder,'("Item: ",I2)') i
     mbuts(i) = f_gtk_menu_item(smnu, trim(holder)//cnull, &
          & activate=c_funloc(mbut_act), data=c_loc(mclicks(i)))
  end do
  ! Add a single button
  mba =  f_gtk_menu_item(menubar, "Extra"//cnull, &
       & activate=c_funloc(mbut_act), data=c_loc(mca))

  ! Now a second menu with just a single tlb
  mnu2 =  f_gtk_menu()
  call gtk_box_pack_start_defaults(box, mnu2)

  sm1 = f_gtk_menu_submenu(mnu2, "Select"//cnull)

  do i = 1, 4
     write(holder,'("Select: ",I2)') i
     mb1(i) = f_gtk_menu_item(sm1, trim(holder)//cnull, &
          & activate=c_funloc(sm1_act), data=c_loc(mc1(i)))
     if (i == 3) sm2 = f_gtk_menu_submenu(sm1, "Sub choice"//cnull)
  end do
  do i = 1, 4
     write(holder,'("Sub Sel: ",I2)') i
     mb2(i) = f_gtk_menu_item(sm2, trim(holder)//cnull, &
          & activate=c_funloc(sm2_act), data=c_loc(mc2(i)))
  end do

  ! Make a quit button and put it in the box, put the box
  ! into the window
  qbut = f_gtk_button("Quit"//cnull, clicked=c_funloc(my_destroy))
  call gtk_box_pack_start_defaults(box, qbut)

  ! Realize the hierarchy
  call gtk_widget_show_all(win)

  ! Event loop
  call gtk_main()

end program menu_test
