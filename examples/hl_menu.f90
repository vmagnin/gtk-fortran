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
!
! gfortran -g gtk.f90 gtk-sup.f90 gtk-hl.f90 hl_menu.f90 `pkg-config --cflags --libs gtk+-3.0`
! Contributed by James Tappin.
! Last modification: vmagnin 02-20-2019

module handlers
  use gtk_hl
  use gtk, only: gtk_button_new, gtk_container_add, gtk_label_new, gtk_main, gtk_&
       &main_quit, gtk_menu_item_new, gtk_menu_new, gtk_widget_destroy, gtk_widget_sho&
       &w, gtk_widget_show_all, gtk_window_new, gtk_init, &
       & gtk_check_menu_item_get_active

  implicit none

  type(c_ptr) :: win, box, menubar, qbut,lab, smnu,mba
  type(c_ptr), dimension(10) :: mbuts
  type(c_ptr) :: mnu2, sm1, sm2
  type(c_ptr), dimension(4) :: mb1, mb2
  type(c_ptr) :: rgroup

contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    print *, "Exit called"
    call gtk_main_quit ()
  end subroutine my_destroy

  subroutine mbut_act(widget, gdata)  bind(c)
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int), pointer :: fdata

    print *, "Menu 1"
    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       if (fdata < 0) then
          print *, "Chose: Extra"
       else
          print *, "Chose:",fdata
       end if
    end if

  end subroutine mbut_act

  subroutine sm1_act(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int), pointer :: fdata
    integer(kind=c_int) :: istat

    print *, "Menu 2"
    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       istat = gtk_check_menu_item_get_active(widget)
       if (istat == TRUE) then
          print *, "Selected:",fdata
       else
          print *, "Deselected:",fdata
       end if
    end if
  end subroutine sm1_act

  subroutine sm2_act(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    integer(kind=c_int), pointer :: fdata
    integer(kind=c_int) :: istat

    print *, "Menu 2 (submenu)"
    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       istat = gtk_check_menu_item_get_active(widget)
       if (istat == TRUE) then
          print *, "Sub-Selected:",fdata
       else
          print *, "Sub-Deselected:",fdata
       end if
    end if
  end subroutine sm2_act

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
  type(c_ptr) :: accel

  ! Initialize gtk
  call gtk_init()

  ! Make a window for the hierarchy
  win = hl_gtk_window_new("Menu Demo"//c_null_char, destroy=c_funloc(my_destroy), &
       & accel_group=accel)

  ! Make a vertical box, and add a label to it
  box=hl_gtk_box_new()
  call gtk_container_add(win, box)

  lab = gtk_label_new("Menu Example"//c_null_char)
  call hl_gtk_box_pack(box, lab)

  ! Make a menubar with the buttons horizontally aranged and put it in the
  ! box
  menubar = hl_gtk_menu_new(GTK_PACK_DIRECTION_LTR)
  call hl_gtk_box_pack(box, menubar)

  ! Make a submenu in the first (0) location
  smnu = hl_gtk_menu_submenu_new(menubar, "Choose"//c_null_char)

  ! Populate the submenu with buttons
  do i = 1, size(mbuts)
     write(holder,'("Item: ",I2)') i
     mbuts(i) = hl_gtk_menu_item_new(smnu, trim(holder)//c_null_char, &
          & activate=c_funloc(mbut_act), data=c_loc(mclicks(i)), &
          & accel_key=char(ichar("a")+i-1)//c_null_char, accel_group=accel)
  end do
  ! Add a single button
  mba =  hl_gtk_menu_item_new(menubar, "Extra"//c_null_char, &
       & activate=c_funloc(mbut_act), data=c_loc(mca))

  ! Now a second menu with just a single tlb
  mnu2 =  hl_gtk_menu_new()
  call hl_gtk_box_pack(box, mnu2)

  sm1 = hl_gtk_menu_submenu_new(mnu2, "Select"//c_null_char)

  do i = 1, 4
     write(holder,'("Select: ",I2)') i
     mb1(i) = hl_gtk_check_menu_item_new(sm1, trim(holder)//c_null_char, &
          & toggled=c_funloc(sm1_act), data=c_loc(mc1(i)))
     if (i == 3) sm2 = hl_gtk_menu_submenu_new(sm1, "Sub choice"//c_null_char)
  end do

  rgroup = C_NULL_PTR
  do i = 1, 4
     write(holder,'("Sub Sel: ",I2)') i
     mb2(i) = hl_gtk_radio_menu_item_new(rgroup, sm2, trim(holder)//c_null_char, &
          & toggled=c_funloc(sm2_act), data=c_loc(mc2(i)))
  end do
  call hl_gtk_radio_menu_group_set_select(rgroup, 2_c_int)

  ! Make a quit button and put it in the box, put the box
  ! into the window
  qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(my_destroy), &
       & accel_key="q"//c_null_char, accel_group=accel)
  call hl_gtk_box_pack(box, qbut)

  ! Realize the hierarchy
  call gtk_widget_show_all(win)

  ! Event loop
  call gtk_main()

end program menu_test
