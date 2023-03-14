! This file is part of gtk-fortran, a GTK / Fortran interface library.
! Copyright (C) 2021 The gtk-fortran team
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
! You should have received a copy of the GNU General Public License along with
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------
! Contributed by: Jerry DeLisle, 2021-02-24
! Last modifications: vmagnin 2023-03-14
! This example demonstrates a menu bar and is based on:
!   https://github.com/ToshioCP/Gtk4-tutorial/blob/main/src/menu/menu2.c
!-------------------------------------------------------------------------------

module handlers
  use gtk, only: gtk_application_new, g_signal_connect, FALSE, TRUE, &
               & gtk_window_maximize, gtk_window_unmaximize, &
               & gtk_css_provider_load_from_data, gtk_application_window_new, &
               & gtk_window_set_title, gtk_window_set_default_size, &
               & gtk_label_new, gtk_widget_set_name, gtk_window_set_child, &
               & gtk_application_set_menubar, &
               & gtk_application_window_set_show_menubar, &
               & gtk_css_provider_new, gtk_widget_get_display, &
               & gtk_css_provider_load_from_data, g_application_flags_none, &
               & gtk_style_context_add_provider_for_display, gtk_window_present
  use gtk_sup, only: convert_c_string
  use g, only: g_variant_new_boolean, g_variant_get_boolean, &
             & g_variant_new_string, g_variant_get_string, g_variant_type_new, &
             & g_simple_action_set_state, g_action_change_state, &
             & g_application_run, g_application_quit, &
             & g_simple_action_new_stateful, &
             & g_simple_action_new, g_menu_new, g_menu_item_new, &
             & g_action_map_add_action, g_menu_append_item, &
             & g_object_unref, g_menu_append_section, g_menu_append_submenu
  use, intrinsic :: iso_c_binding, only: c_null_ptr, c_null_char, c_ptr, c_int, c_char, &
                         & c_funloc, c_size_t

  implicit none
  type(c_ptr) :: app, provider

contains

  !*************************************************
  ! The three callback functions of the menu items:
  !*************************************************
  subroutine fullscreen_changed (act, avalue, win) bind(c)
    type(c_ptr), value, intent(in) :: act, avalue, win
    logical :: state

    ! This is the correct way to convert a boolean integer to logical.
    state = transfer(g_variant_get_boolean (avalue), state)

    if (state) then
      call gtk_window_maximize (win)
    else
      call gtk_window_unmaximize (win)
    end if
    call g_simple_action_set_state (act, avalue)
  end subroutine

  subroutine color_activated (act, param, win) bind(c)
    type(c_ptr), value, intent(in) :: act, param, win
    type(c_ptr) :: param_string
    character(kind=c_char, len=40):: color, color_str

    ! Get the C string from param
    param_string = g_variant_get_string (param, c_null_ptr)

    ! Convert it to a Fortran string
    call convert_c_string(param_string, color_str)
    print *, "The color will be: "//trim(color_str)//"!"

    color = "label#lb {background-color: "//trim(color_str)//"; }"//c_null_char

    print *, "Let's change the color!"
    call gtk_css_provider_load_from_data (provider, color, -1_c_size_t)
    call g_action_change_state (act, param)
  end subroutine

  subroutine quit_activated (act, param, win) bind(c)
    type(c_ptr), value, intent(in) :: act, param, win

    print *, "QUIT!"
    call g_application_quit (app)
  end subroutine

  !*******************************************************
  ! The activate callback function of the GtkApplication:
  !*******************************************************
  subroutine activate (app, user_data) bind(c)
    type(c_ptr), value, intent(in)  :: app, user_data
    type(c_ptr) :: win, display, lb, act_fullscreen, &
                 & act_color, act_quit
    type(c_ptr) :: menubar, menu, section1, section2, section3, &
      & menu_item_red, menu_item_green, &
      & menu_item_blue, menu_item_quit, menu_item_fullscreen
    integer(c_size_t) :: length = -1_c_size_t

    win = gtk_application_window_new (app)
    call gtk_window_set_title (win, "menubar"//c_null_char)
    call gtk_window_set_default_size (win, 400, 300)

    lb = gtk_label_new (""//c_null_char)
    call gtk_widget_set_name (lb, "lb"//c_null_char)  ! the name is used by CSS Selector.
    call gtk_window_set_child (win, lb)

    act_fullscreen = g_simple_action_new_stateful ("fullscreen"//c_null_char, &
                                  & c_null_ptr, g_variant_new_boolean (FALSE))
    act_color = g_simple_action_new_stateful ("color"//c_null_char, &
                                  & g_variant_type_new("s"//c_null_char), &
                                  & g_variant_new_string ("red"//c_null_char))
    act_quit = g_simple_action_new ("quit"//c_null_char, c_null_ptr)

    menubar = g_menu_new ()
    menu = g_menu_new ()
    section1 = g_menu_new ()
    section2 = g_menu_new ()
    section3 = g_menu_new ()
    menu_item_fullscreen = g_menu_item_new ("Full Screen"//c_null_char, "win.fullscreen"//c_null_char)
    menu_item_red = g_menu_item_new ("Red"//c_null_char, "win.color::red"//c_null_char)
    menu_item_green = g_menu_item_new ("Green"//c_null_char, "win.color::green"//c_null_char)
    menu_item_blue = g_menu_item_new ("Blue"//c_null_char, "win.color::blue"//c_null_char)
    menu_item_quit = g_menu_item_new ("Quit"//c_null_char, "app.quit"//c_null_char)

    call g_signal_connect (act_fullscreen, "change-state"//c_null_char, c_funloc(fullscreen_changed), win)
    call g_signal_connect (act_color, "activate"//c_null_char, c_funloc (color_activated), win)
    call g_signal_connect (act_quit, "activate"//c_null_char, c_funloc (quit_activated), app)
    call g_action_map_add_action (win, act_fullscreen)
    call g_action_map_add_action (win, act_color)
    call g_action_map_add_action (app, act_quit)

    call g_menu_append_item (section1, menu_item_fullscreen)
    call g_menu_append_item (section2, menu_item_red)
    call g_menu_append_item (section2, menu_item_green)
    call g_menu_append_item (section2, menu_item_blue)
    call g_menu_append_item (section3, menu_item_quit)
    call g_object_unref (menu_item_red)
    call g_object_unref (menu_item_green)
    call g_object_unref (menu_item_blue)
    call g_object_unref (menu_item_fullscreen)
    call g_object_unref (menu_item_quit)

    call g_menu_append_section (menu, c_null_char, section1)
    call g_menu_append_section (menu, "Color"//c_null_char, section2)
    call g_menu_append_section (menu, c_null_char, section3)
    call g_menu_append_submenu (menubar, "Menu"//c_null_char, menu)

    call gtk_application_set_menubar (app, menubar)
    call gtk_application_window_set_show_menubar (win, TRUE)

    provider = gtk_css_provider_new ()
    display = gtk_widget_get_display (win)
    call gtk_css_provider_load_from_data (provider, "label#lb {background-color: red;}"//c_null_char, length)
    call gtk_style_context_add_provider_for_display (display, provider, 800)

    call gtk_window_present (win)
  end subroutine
end module handlers

program menu_example
  use handlers

  implicit none

  integer(c_int) :: app_stat

  app = gtk_application_new ("gtk-fortran.examples.menubar"//c_null_char, &
                          & G_APPLICATION_FLAGS_NONE)
  call g_signal_connect(app, "activate"//c_null_char, &
                      & c_funloc(activate), c_null_ptr)

  app_stat = g_application_run(app, 0_c_int, [c_null_ptr])
  call g_object_unref (app)
end program
