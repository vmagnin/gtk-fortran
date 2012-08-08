! Copyright (C) 2011
! Free Software Foundation, Inc.

! This file is part of the gtk-fortran GTK+ Fortran Interface library.

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
! Contributed by Vincent MAGNIN, 02-24-2011, last modified: 03-09-2011
! ****************
! Automated tests
! ****************
! gfortran ../src/gtk.f90 tests.f90 `pkg-config --cflags --libs gtk+-2.0`
! If this program generates errors, please send us the tests_errors.txt file
! with informations on your system (OS version, GTK+ version, compiler...)

module tests
  use gtk, only: gtk_false, gtk_true, TRUE, FALSE, c_null_ptr, c_null_char
  use g, only: g_ascii_tolower, g_bit_storage, g_date_get_day, g_date_get_days_in&
  &_month, g_hostname_is_ip_address, g_inet_socket_address_get_port, g_inet_socke&
  &t_address_new, g_random_double, g_random_double_range, g_random_int, g_random_&
  &int_range, g_variant_get_boolean, g_variant_get_byte, g_variant_get_double, g_&
  &variant_get_int16, g_variant_get_int32, g_variant_get_int64, g_variant_get_str&
  &ing, g_variant_get_uint16, g_variant_get_uint32, g_variant_get_uint64, g_varia&
  &nt_new_boolean, g_variant_new_byte, g_variant_new_double, g_variant_new_int16,&
  & g_variant_new_int32, g_variant_new_int64, g_variant_new_string, g_variant_new&
  &_uint16, g_variant_new_uint32, g_variant_new_uint64, guint64
  use iso_c_binding
  
contains
  integer function test_iso_c_binding()
    use iso_c_binding
    implicit none
    integer :: errors
    
    errors = 0
    if (C_SIGNED_CHAR < 0) then
      write(1,*) "C_SIGNED_CHAR", C_SIGNED_CHAR
      errors = errors + 1
    end if
    if (C_SHORT < 0) then
      write(1,*) "C_SHORT", C_SHORT
      errors = errors + 1
    end if
    if (C_INT < 0) then
      write(1,*) "C_INT", C_INT
      errors = errors + 1
    end if
    if (C_LONG < 0) then
      write(1,*) "C_LONG", C_LONG
      errors = errors + 1
    end if
    if (C_LONG_LONG < 0) then
      write(1,*) "C_LONG_LONG", C_LONG_LONG
      errors = errors + 1
    end if
    if (C_SIZE_T < 0) then
      write(1,*) "C_SIZE_T", C_SIZE_T
      errors = errors + 1
    end if
    if (C_INTPTR_T < 0) then
      write(1,*) "C_INTPTR_T", C_INTPTR_T
      errors = errors + 1
    end if
    if (C_INTMAX_T < 0) then
      write(1,*) "C_INTMAX_T", C_INTMAX_T
      errors = errors + 1
    end if
    if (C_INT8_T < 0) then
      write(1,*) "C_INT8_T", C_INT8_T
      errors = errors + 1
    end if
    if (C_INT16_T < 0) then
      write(1,*) "C_INT16_T", C_INT16_T
      errors = errors + 1
    end if
    if (C_INT32_T < 0) then
      write(1,*) "C_INT32_T", C_INT32_T
      errors = errors + 1
    end if
    if (C_INT64_T < 0) then
      write(1,*) "C_INT64_T", C_INT64_T
      errors = errors + 1
    end if
    if (C_INT_LEAST8_T < 0) then
      write(1,*) "C_INT_LEAST8_T", C_INT_LEAST8_T
      errors = errors + 1
    end if
    if (C_INT_LEAST16_T < 0) then
      write(1,*) "C_INT_LEAST16_T", C_INT_LEAST16_T
      errors = errors + 1
    end if
    if (C_INT_LEAST32_T < 0) then
      write(1,*) "C_INT_LEAST32_T", C_INT_LEAST32_T
      errors = errors + 1
    end if
    if (C_INT_LEAST64_T < 0) then
      write(1,*) "C_INT_LEAST64_T", C_INT_LEAST64_T
      errors = errors + 1
    end if
    if (C_INT_FAST8_T < 0) then
      write(1,*) "C_INT_FAST8_T", C_INT_FAST8_T
      errors = errors + 1
    end if
    if (C_INT_FAST16_T < 0) then
      write(1,*) "C_INT_FAST16_T", C_INT_FAST16_T
      errors = errors + 1
    end if
    if (C_INT_FAST32_T < 0) then
      write(1,*) "C_INT_FAST32_T", C_INT_FAST32_T
      errors = errors + 1
    end if
    if (C_INT_FAST64_T < 0) then
      write(1,*) "C_INT_FAST64_T", C_INT_FAST64_T
      errors = errors + 1
    end if
    if (C_FLOAT < 0) then
      write(1,*) "C_FLOAT", C_FLOAT
      errors = errors + 1
    end if
    if (C_DOUBLE < 0) then
      write(1,*) "C_DOUBLE", C_DOUBLE
      errors = errors + 1
    end if
    if (C_LONG_DOUBLE < 0) then
      write(1,*) "C_LONG_DOUBLE", C_LONG_DOUBLE
      errors = errors + 1
    end if
    if (C_FLOAT_COMPLEX < 0) then
      write(1,*) "C_FLOAT_COMPLEX", C_FLOAT_COMPLEX
      errors = errors + 1
    end if
    if (C_DOUBLE_COMPLEX < 0) then
      write(1,*) "C_DOUBLE_COMPLEX", C_DOUBLE_COMPLEX
      errors = errors + 1
    end if
    if (C_LONG_DOUBLE_COMPLEX < 0) then
      write(1,*) "C_LONG_DOUBLE_COMPLEX", C_LONG_DOUBLE_COMPLEX
      errors = errors + 1
    end if
    if (C_BOOL < 0) then
      write(1,*) "C_BOOL", C_BOOL
      errors = errors + 1
    end if
    if (C_CHAR < 0) then
      write(1,*) "C_CHAR", C_CHAR
      errors = errors + 1
    end if
    
    test_iso_c_binding = errors
  end function test_iso_c_binding


  integer function test_c_char_in_out()
    implicit none
    integer :: i
    integer :: errors
    character(kind=c_char) :: c
  !    !  gchar g_ascii_tolower (gchar c) G_GNUC_CONST;
  !    function g_ascii_tolower(c) bind(c) 
  !      use iso_c_binding, only: c_char
  !      character(kind=c_char) :: g_ascii_tolower
  !      character(kind=c_char), value :: c
  !    end function
    errors = 0
    do i=0, 64, +1
      c = char(i)
      if (g_ascii_tolower(int(i,c_int8_t)) /= int(i,c_int8_t)) then
        write(1,*) "ERROR gchar g_ascii_tolower (gchar c): ", i, g_ascii_tolower(int(i,c_int8_t))
        errors = errors + 1
      end if
    end do

    do i=65, 90, +1
      c = char(i)
      if (g_ascii_tolower(int(i,c_int8_t)) /= int(i,c_int8_t)+32) then
        write(1,*) "ERROR gchar g_ascii_tolower (gchar c): ", i, g_ascii_tolower(int(i,c_int8_t))
        errors = errors + 1
      end if
    end do

    do i=91, 255, +1
      c = char(i)
      if (g_ascii_tolower(int(i,c_int8_t)) /= int(i,c_int8_t)) then
        write(1,*) "ERROR gchar g_ascii_tolower (gchar c): ", i, g_ascii_tolower(int(i,c_int8_t))
        errors = errors + 1
      end if
    end do
    test_c_char_in_out = errors
  end function test_c_char_in_out


  integer function test_gdouble_in_out()
    implicit none
    integer :: i
    integer :: errors
    real(c_double) :: a, b, r, rmin, rmax
    !See the official documentation:
    !http://library.gnome.org/devel/glib/stable/glib-GVariant.html
    !http://library.gnome.org/devel/glib/stable/glib-GVariantType.html
    !GVariant *          g_variant_new_double   (gdouble value);
    !gdouble             g_variant_get_double   (GVariant *value);
    errors = 0
    do i = -308, +308
      a = 10d0 ** i
      b = g_variant_get_double(g_variant_new_double(a))
      if (a /= b) then
        write(1,*) "ERROR g_random_double_range:", i, a, b
        errors = errors + 1
      end if
    end do
  !    ! gdouble g_random_double_range (gdouble begin, gdouble end);
  !    function g_random_double_range(begin, end) bind(c) 
  !      use iso_c_binding, only: c_double
  !      real(c_double) :: g_random_double_range
  !      real(c_double), value :: begin
  !      real(c_double), value :: end
  !    end function
    rmin = -10d0
    rmax = +100d0
    do i = 1, 10000, +1
      r = g_random_double_range(rmin, rmax)
      if ((r<rmin).or.(r>rmax)) then
        write(1,*) "ERROR g_random_double_range:", r
        errors = errors + 1
      end if
    end do
    test_gdouble_in_out = errors
  end function test_gdouble_in_out


  integer function test_gulong_in()
    implicit none
    integer :: i
    integer :: errors
    integer(c_long) :: nb
    integer(c_int) :: r
  !    ! guint g_bit_storage (gulong number) G_GNUC_CONST;
  !    function g_bit_storage(number) bind(c) 
  !      use iso_c_binding, only: c_int, c_long
  !      integer(c_int) :: g_bit_storage
  !      integer(c_long), value :: number
  !    end function
  errors = 0
  ! Fortran integers are signed. 32 bits integers are in [-2147483648, +2147483647].
  ! C language: typedef unsigned long   gulong;
    do i = 1, 31, +1
      nb = 2**i-1
      r = g_bit_storage(nb)
      if (i /= r) then
        write(1,*) "ERROR g_bit_storage:", i, nb, r
        errors = errors + 1
      end if
    end do
    test_gulong_in = errors
  end function test_gulong_in


  integer function test_uint16_in_out()
    implicit none
    integer :: i
    integer :: errors
    integer(c_int16_t) :: a, b
    integer(c_int32_t) :: c, d
  !! GVariant * g_variant_new_uint16 (guint16 uint16);
  !function g_variant_new_uint16(uint16) bind(c) 
  !  use iso_c_binding, only: c_ptr, c_int16_t
  !  type(c_ptr) :: g_variant_new_uint16
  !  integer(c_int16_t), value :: uint16
  !end function

  !! guint16 g_variant_get_uint16 (GVariant *value);
  !function g_variant_get_uint16(value) bind(c) 
  !  use iso_c_binding, only: c_int16_t, c_ptr
  !  integer(c_int16_t) :: g_variant_get_uint16
  !  type(c_ptr), value :: value
  !end function
    errors = 0
  ! INTEGER(2) ranges from -32768 to 32767
  ! uint16 ranges from 0 to 65535.
    do a = 0, 32767
      b = g_variant_get_uint16(g_variant_new_uint16 (a))
      if (a /= b) then
        write(1,*) "ERROR g_variant_get_uint16:", a, b
        errors = errors + 1
      end if
    end do
    
    do c = 32768, 65535
      a = transfer(c, a)
      b = g_variant_get_uint16(g_variant_new_uint16 (a))
      d = transfer(b, d)
      do i = bit_size(b),  bit_size(d)-1
        d= ibclr(d, i)
      end do
!      write(1,*) c, a, b, d
!      do i = 0,  bit_size(c)-1
!        write(*, "(l1)", advance="no") btest(c, i)
!      end do
!      write(*,*)
!      do i = 0,  bit_size(a)-1
!        write(*, "(l1)", advance="no") btest(a, i)
!      end do
!      write(*,*)
!      do i = 0,  bit_size(b)-1
!        write(*, "(l1)", advance="no") btest(b, i)
!      end do
!      write(*,*)    
!      do i = 0,  bit_size(d)-1
!        write(*, "(l1)", advance="no") btest(d, i)
!      end do
!      write(*,*)    
      if (a /= b) then
        write(1,*) "ERROR g_variant_get_uint16:", a, b
        errors = errors + 1
      end if
    end do
    test_uint16_in_out = errors
  end function test_uint16_in_out


  integer function test_int16_in_out()
    implicit none
    integer(c_int16_t) :: a, b
    integer :: errors, i
    errors = 0

    do i = -32768, 32767
       a = int(i,c_int16_t)
       b = g_variant_get_int16(g_variant_new_int16 (a))
       if (a /= b) then
          write(1,*) "ERROR g_variant_get_int16:", a, b
          errors = errors + 1
       end if
    end do
    test_int16_in_out = errors
  end function test_int16_in_out


  integer function test_int32_in_out()
    implicit none
    integer :: i
    integer :: errors
    integer(c_int32_t) :: r, rmin, rmax
    integer(c_int32_t) :: a, b
  !GVariant *          g_variant_new_int32    (gint32 value);
  !gint32              g_variant_get_int32                 (GVariant *value);
    errors = 0
    do a = -huge(b), huge(b), +65536
      b = g_variant_get_int32(g_variant_new_int32 (a))
      if (a /= b) then
        write(1,*) "ERROR g_variant_get_int32:", a, b
        errors = errors + 1
      end if
    end do
  !    ! gint32 g_random_int_range (gint32 begin, gint32 end);
  !    function g_random_int_range(begin, end) bind(c) 
  !      use iso_c_binding, only: c_int32_t
  !      integer(c_int32_t) :: g_random_int_range
  !      integer(c_int32_t), value :: begin
  !      integer(c_int32_t), value :: end
  !    end function
    rmin = -10
    rmax = +100
    do i = 1, 10000, +1
      r = g_random_int_range(rmin, rmax)
      if ((r<rmin).or.(r>rmax)) then
        write(1,*) "ERROR g_random_double_range:", r
        errors = errors + 1
      end if
    end do
    test_int32_in_out = errors 
  end function test_int32_in_out


  integer function test_uint32_in_out()
    implicit none
    integer :: i
    integer :: errors
    integer(c_int32_t) :: a, b
    integer(c_int64_t) :: c, d
    errors = 0
  ! INTEGER(4) ranges from -2147483648 to +2147483647
  ! uint32 ranges from 0 to 4294967295
    do a = 0, 2147483647, +65536
      b = g_variant_get_uint32(g_variant_new_uint32(a))
      if (a /= b) then
        write(1,*) "ERROR g_variant_get_uint32:", a, b
        errors = errors + 1
      end if
    end do
    
    do c = 2147483648_8, 4294967295_8, +65536
      a = transfer(c, a)
      b = g_variant_get_uint32(g_variant_new_uint32(a))
      d = transfer(b, d)
      do i = bit_size(b),  bit_size(d)-1
        d= ibclr(d, i)
      end do
!      write(1,*) c, a, b, d
!      do i = 0,  bit_size(c)-1
!        write(*, "(l1)", advance="no") btest(c, i)
!      end do
!      write(*,*)
!      do i = 0,  bit_size(a)-1
!        write(*, "(l1)", advance="no") btest(a, i)
!      end do
!      write(*,*)
!      do i = 0,  bit_size(b)-1
!        write(*, "(l1)", advance="no") btest(b, i)
!      end do
!      write(*,*)    
!      do i = 0,  bit_size(d)-1
!        write(*, "(l1)", advance="no") btest(d, i)
!      end do
!      write(*,*)    
      if (a /= b) then
        write(1,*) "ERROR g_variant_get_uint32:", a, b
        errors = errors + 1
      end if
    end do    
    test_uint32_in_out = errors 
  end function test_uint32_in_out


  integer function test_guchar_in_out()
    implicit none
    integer(c_int16_t) :: i, j
    integer :: errors 
    character(kind=c_char) :: a, b
  !GVariant *          g_variant_new_byte     (guchar value);
  !guchar              g_variant_get_byte     (GVariant *value);
    errors = 0
    do i = 0, 255, +1
      a = char(i)
      j = g_variant_get_byte(g_variant_new_byte (int(i,c_int8_t))) 
      b = achar(j)
      if ((a /= b) .or. (iand(i,255_c_int16_t) /= iand(j, 255_c_int16_t))) then
        write(1,*) "ERROR test_guchar_in_out:", a, b, i, j
        errors = errors + 1
      end if
    end do
    test_guchar_in_out = errors
  end function test_guchar_in_out


  integer function test_gboolean_in_out()
    implicit none
    integer(c_int) :: l1, l2, l3, l4
    integer :: errors
  !GVariant *          g_variant_new_boolean  (gboolean value);
  !gboolean            g_variant_get_boolean               (GVariant *value);
    errors = 0
    l1 = TRUE
    l2 = g_variant_get_boolean(g_variant_new_boolean (l1))
    print *, l1, l2
    if (l1 /= l2) then
        write(1,*) "ERROR g_variant_get_boolean:", l1, l2
        errors = errors + 1
    end if

    l1 = FALSE
    l2 = g_variant_get_boolean(g_variant_new_boolean (l1))
    print *, l1, l2
    if (l1 /= l2) then
        write(1,*) "ERROR g_variant_get_boolean:", l1, l2
        errors = errors + 1
    end if
  !      ! gboolean g_hostname_is_ip_address (const gchar *hostname);
  !    function g_hostname_is_ip_address(hostname) bind(c) 
  !      use iso_c_binding, only: c_bool, c_char
  !      logical(c_bool) :: g_hostname_is_ip_address
  !      character(kind=c_char), dimension(*) :: hostname
  !    end function
    l1 = g_hostname_is_ip_address("192.168.0.1"//c_null_char)
    l2 = g_hostname_is_ip_address("1AA.168.0.1"//c_null_char)
    l3 = g_hostname_is_ip_address("blabla"//c_null_char)
    l4 = g_hostname_is_ip_address("192.168,0.1"//c_null_char)
    if ((l1 /= TRUE) .or. (l2 /= FALSE) .or. (l3 /= FALSE) .or. (l4 /= FALSE)) then
      write(1,*) "ERROR g_hostname_is_ip_address:", l1, l2, l3, l4
      errors = errors + 1
    end if
    ! gboolean gtk_true (void) G_GNUC_CONST;
    ! gboolean gtk_false (void) G_GNUC_CONST;
    if ((gtk_true() /= TRUE) .or. (gtk_false() /= FALSE)) then
      write(1,*) "ERROR gtk_true, gtk_false:", gtk_true(), gtk_false()
      errors = errors + 1
    end if
    
    test_gboolean_in_out = errors
  end function test_gboolean_in_out

end module tests

!guint8              g_date_get_days_in_month            (GDateMonth month,
!                                                         GDateYear year);
!GSocketAddress *    g_inet_socket_address_new           (GInetAddress *address,
!                                                         guint16 port);
!guint16             g_inet_socket_address_get_port      (GInetSocketAddress *address);

!GVariant *                       g_variant_new_int64    (gint64 value);
!GVariant *                       g_variant_new_uint64   (guint64 value);
!GVariant *                       g_variant_new_string   (const gchar *string);

!gint64              g_variant_get_int64                 (GVariant *value);
!guint64             g_variant_get_uint64                (GVariant *value);
!const gchar *                    g_variant_get_string   (GVariant *value,
!                                                         gsize *length);

! iso_c types used in gtk-fortran:
! ['c_ptr', 'c_char', 'c_funptr', 'c_int', 'c_int64_t', 'c_bool', 
! 'c_size_t', 'c_int16_t', 'c_int32_t', 'c_double', 'c_long', 'c_long_double', 
! 'c_int8_t', 'c_float']

program gtk_fortran_test
  use tests
  implicit none
  integer :: errors
  
  open(unit=1, file="tests_errors.txt")
  errors = test_iso_c_binding()
  errors = errors +  test_c_char_in_out()
  errors = errors +  test_guchar_in_out()
  errors = errors +  test_gdouble_in_out()
  errors = errors +  test_gulong_in()
  errors = errors +  test_int16_in_out()
  errors = errors +  test_uint16_in_out()
  errors = errors +  test_int32_in_out()
  errors = errors +  test_uint32_in_out()
  errors = errors +  test_gboolean_in_out()
  close(1)
  
  if (errors == 0) then
    print *, "No error"
  else
    print *, errors, "errors"
    print *, "See the 'tests_errors.txt' file"
  end if
end program gtk_fortran_test
