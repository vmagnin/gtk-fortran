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
! Contributed by James Tappin (2011)
! Last modifications: vmagnin 2020-06-18

module gtk_hl
  !*
  ! High Level GTK Fortran interfaces
  !
  ! A collection of procedures to implement higher level creators for
  ! the gtk-fortran widgets. Some settings and operations are also
  ! provided for the more intricate widgets.
  !
  ! Many ideas in this module were taken from the pilib gtk<->fortran
  ! interface.
  !/

  use gtk_hl_container
  use gtk_hl_button
  use gtk_hl_entry
  use gtk_hl_tree
  use gtk_hl_progress
  use gtk_hl_dialog
  use gtk_hl_chooser
  use gtk_hl_spin_slider
  use gtk_hl_combobox
  use gtk_hl_infobar
  use gtk_hl_assistant

  use gtk_hl_misc

end module gtk_hl
