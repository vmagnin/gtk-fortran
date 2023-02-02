! Copyright (C) 2021
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran GTK / Fortran Interface library.
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
!-------------------------------------------------------------------------------
! Contributed by Vincent Magnin, 2021-03-03
! Last modifications: 2021-03-04
!-------------------------------------------------------------------------------
! This example desmonstrates how to use GLib regular expressions to search a
! pattern in a text, and make replacements. That GLib part is based on the PCRE
! library: http://www.pcre.org/
! https://developer.gnome.org/glib/stable/glib-Perl-compatible-regular-expressions.html

program glib_regex
    use, intrinsic :: iso_c_binding
    use g, only: g_regex_new, g_regex_match, g_match_info_matches, &
               & g_match_info_fetch, g_match_info_next, g_free, g_regex_unref, &
               & g_match_info_free, g_regex_replace
    use gtk_sup, only: c_f_logical, convert_c_string

    implicit none
    logical :: found, matched
    type(c_ptr) :: regex, a_result
    type(c_ptr), target :: matchInfo
    character(*), parameter :: content= &
        &"Our first widget in our app is an empty window: we have defined a C &
        &pointer toward that object and we have created it using &
        &gtk_application_window_new(app). Don't forget to call the &
        &gtk_widget_show_all() function if you want your window to appear on &
        &screen! Note that in GTK 3, all widgets are hidden by default and that&
        & is why we use _show_all(). In GTK 4, on the contrary, the widgets &
        &inside a window are all shown by default and the gtk_widget_show_all()&
        & function was removed: you will use instead call gtk_widget_show(window)."
    character(200) :: fstring
    character(1000) :: modified_text

    print *, "ORIGINAL TEXT:"
    print *, content
    print *
    print *, "LET'S LOOK FOR THE FUNCTIONS CITED IN THE TEXT:"
    regex = g_regex_new("(\w+)\(([a-zA-Z0-9_, ]*)\)"//c_null_char, 0, 0, c_null_ptr)
    found = c_f_logical(g_regex_match(regex, content//c_null_char, 0, c_loc(matchInfo)))

    if (.not. found) then
        print *, "NO MATCH."
    else
        print *, "SOME MATCHES WERE FOUND:"
        do while (c_f_logical(g_match_info_matches(matchInfo)))
            a_result = g_match_info_fetch(matchInfo, 0)
            call convert_c_string(a_result, fstring)
            print *, trim(fstring)
            call g_free(a_result)

            matched = c_f_logical(g_match_info_next(matchInfo, c_null_ptr))
        end do

        print *
        print *, "LET'S REPLACE THOSE FUNCTIONS BY 6 STARS IN THE TEXT:"
        call convert_c_string(g_regex_replace(regex, content//c_null_char, &
         & -1_c_size_t, 0, "******"//c_null_char, 0, c_null_ptr), modified_text)
        print *, trim(modified_text)
    end if

    call g_match_info_free(matchInfo)
    call g_regex_unref(regex)
end program glib_regex
