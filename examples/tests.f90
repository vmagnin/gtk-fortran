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
! Contributed by Vincent MAGNIN, 02-24-2011, last modified: 2022-10-03
! ****************
! Automated tests
! ****************
! This program is testing things about ISO_C_BINDING and the relations
! between Fortran types and GLib types. If it generates errors, post
! an issue on GitHub with information on your system
! (OS version, GTK version, compiler...) and the error message.

module tests
  use gtk, only: TRUE, FALSE, gtk_get_major_version, gtk_get_minor_version, &
                 gtk_get_micro_version
  use gtk_sup, only: c_f_string_copy_alloc
  use g, only: g_ascii_tolower, g_bit_storage, g_hostname_is_ip_address, &
    g_random_double, g_random_double_range, g_random_int, &
    g_random_int_range, g_variant_get_boolean, g_variant_get_byte, g_variant_get_double,&
    g_variant_get_int16, g_variant_get_int32, &
    g_variant_get_uint16, g_variant_get_uint32, &
    g_variant_new_boolean, g_variant_new_byte, g_variant_new_double, g_variant_new_int16,&
    g_variant_new_int32, g_variant_new_uint16,&
    g_variant_new_uint32, g_variant_unref, g_get_os_info
  use, intrinsic :: iso_fortran_env, only: compiler_version
  use, intrinsic :: iso_c_binding

  implicit none

contains
  integer function test_iso_c_binding()
    integer :: errors

    errors = 0
    if (C_SIGNED_CHAR < 0) then
      print *, "C_SIGNED_CHAR", C_SIGNED_CHAR
      errors = errors + 1
    end if
    if (C_SHORT < 0) then
      print *, "C_SHORT", C_SHORT
      errors = errors + 1
    end if
    if (C_INT < 0) then
      print *, "C_INT", C_INT
      errors = errors + 1
    end if
    if (C_LONG < 0) then
      print *, "C_LONG", C_LONG
      errors = errors + 1
    end if
    if (C_LONG_LONG < 0) then
      print *, "C_LONG_LONG", C_LONG_LONG
      errors = errors + 1
    end if
    if (C_SIZE_T < 0) then
      print *, "C_SIZE_T", C_SIZE_T
      errors = errors + 1
    end if
    if (C_INTPTR_T < 0) then
      print *, "C_INTPTR_T", C_INTPTR_T
      errors = errors + 1
    end if
    if (C_INTMAX_T < 0) then
      print *, "C_INTMAX_T", C_INTMAX_T
      errors = errors + 1
    end if
    if (C_INT8_T < 0) then
      print *, "C_INT8_T", C_INT8_T
      errors = errors + 1
    end if
    if (C_INT16_T < 0) then
      print *, "C_INT16_T", C_INT16_T
      errors = errors + 1
    end if
    if (C_INT32_T < 0) then
      print *, "C_INT32_T", C_INT32_T
      errors = errors + 1
    end if
    if (C_INT64_T < 0) then
      print *, "C_INT64_T", C_INT64_T
      errors = errors + 1
    end if
    if (C_INT_LEAST8_T < 0) then
      print *, "C_INT_LEAST8_T", C_INT_LEAST8_T
      errors = errors + 1
    end if
    if (C_INT_LEAST16_T < 0) then
      print *, "C_INT_LEAST16_T", C_INT_LEAST16_T
      errors = errors + 1
    end if
    if (C_INT_LEAST32_T < 0) then
      print *, "C_INT_LEAST32_T", C_INT_LEAST32_T
      errors = errors + 1
    end if
    if (C_INT_LEAST64_T < 0) then
      print *, "C_INT_LEAST64_T", C_INT_LEAST64_T
      errors = errors + 1
    end if
    if (C_INT_FAST8_T < 0) then
      print *, "C_INT_FAST8_T", C_INT_FAST8_T
      errors = errors + 1
    end if
    if (C_INT_FAST16_T < 0) then
      print *, "C_INT_FAST16_T", C_INT_FAST16_T
      errors = errors + 1
    end if
    if (C_INT_FAST32_T < 0) then
      print *, "C_INT_FAST32_T", C_INT_FAST32_T
      errors = errors + 1
    end if
    if (C_INT_FAST64_T < 0) then
      print *, "C_INT_FAST64_T", C_INT_FAST64_T
      errors = errors + 1
    end if
    if (C_FLOAT < 0) then
      print *, "C_FLOAT", C_FLOAT
      errors = errors + 1
    end if
    if (C_DOUBLE < 0) then
      print *, "C_DOUBLE", C_DOUBLE
      errors = errors + 1
    end if
    if (C_LONG_DOUBLE < 0) then
      print *, "C_LONG_DOUBLE", C_LONG_DOUBLE
      errors = errors + 1
    end if
    if (C_FLOAT_COMPLEX < 0) then
      print *, "C_FLOAT_COMPLEX", C_FLOAT_COMPLEX
      errors = errors + 1
    end if
    if (C_DOUBLE_COMPLEX < 0) then
      print *, "C_DOUBLE_COMPLEX", C_DOUBLE_COMPLEX
      errors = errors + 1
    end if
    if (C_LONG_DOUBLE_COMPLEX < 0) then
      print *, "C_LONG_DOUBLE_COMPLEX", C_LONG_DOUBLE_COMPLEX
      errors = errors + 1
    end if
    if (C_BOOL < 0) then
      print *, "C_BOOL", C_BOOL
      errors = errors + 1
    end if
    if (C_CHAR < 0) then
      print *, "C_CHAR", C_CHAR
      errors = errors + 1
    end if

    test_iso_c_binding = errors
  end function test_iso_c_binding


  integer function test_c_char_in_out()
    integer :: i
    integer :: errors
    character(kind=c_char) :: c

    errors = 0
    do i=0, 64, +1
      c = char(i)
      if (g_ascii_tolower(int(i,c_int8_t)) /= int(i,c_int8_t)) then
        print *, "ERROR gchar g_ascii_tolower (gchar c): ", i, g_ascii_tolower(int(i,c_int8_t))
        errors = errors + 1
      end if
    end do

    do i=65, 90, +1
      c = char(i)
      if (g_ascii_tolower(int(i,c_int8_t)) /= int(i,c_int8_t)+32) then
        print *, "ERROR gchar g_ascii_tolower (gchar c): ", i, g_ascii_tolower(int(i,c_int8_t))
        errors = errors + 1
      end if
    end do

    do i=91, 255, +1
      c = char(i)
      if (g_ascii_tolower(int(i,c_int8_t)) /= int(i,c_int8_t)) then
        print *, "ERROR gchar g_ascii_tolower (gchar c): ", i, g_ascii_tolower(int(i,c_int8_t))
        errors = errors + 1
      end if
    end do

    test_c_char_in_out = errors
  end function test_c_char_in_out


  integer function test_gdouble_in_out()
    integer :: i
    integer :: errors
    real(c_double) :: a, b, r, rmin, rmax
    type(c_ptr) :: gv
    !See the official documentation:
    !http://library.gnome.org/devel/glib/stable/glib-GVariant.html
    !http://library.gnome.org/devel/glib/stable/glib-GVariantType.html
    !GVariant *          g_variant_new_double   (gdouble value);
    !gdouble             g_variant_get_double   (GVariant *value);
    errors = 0
    do i = -308, +308
      a = 10.0_c_double ** i
      gv = g_variant_new_double(a)
      b = g_variant_get_double(gv)
      if (a /= b) then
        print *, "ERROR g_random_double_range:", i, a, b
        errors = errors + 1
      end if
      call g_variant_unref(gv)
    end do
    !    ! gdouble g_random_double_range (gdouble begin, gdouble end);
    !    function g_random_double_range(begin, end) bind(c)
    !      use, intrinsic :: iso_c_binding, only: c_double
    !      real(c_double) :: g_random_double_range
    !      real(c_double), value :: begin
    !      real(c_double), value :: end
    !    end function
    rmin = -10.0_c_double
    rmax = +100.0_c_double
    do i = 1, 10000, +1
      r = g_random_double_range(rmin, rmax)
      if ((r<rmin).or.(r>rmax)) then
        print *, "ERROR g_random_double_range:", r
        errors = errors + 1
      end if
    end do

    test_gdouble_in_out = errors
  end function test_gdouble_in_out


  integer function test_gulong_in()
    integer :: i
    integer :: errors
    integer(c_long) :: nb
    integer(c_int) :: r
    !    ! guint g_bit_storage (gulong number) G_GNUC_CONST;
    !    function g_bit_storage(number) bind(c)
    !      use, intrinsic :: iso_c_binding, only: c_int, c_long
    !      integer(c_int) :: g_bit_storage
    !      integer(c_long), value :: number
    !    end function
    errors = 0
    ! Fortran integers are signed. 32 bits integers are in [-2147483648, +2147483647],
    ! and the C language guarantee [-2147483647, +2147483647] for long.
    ! C language: typedef unsigned long   gulong;
    do i = 1, 31, +1
      ! Writing 2**i - 1 could overflow with some compilers (ifort):
      nb = 2_c_long**i - 1
      r = g_bit_storage(nb)
      if (i /= r) then
        print *, "ERROR g_bit_storage:", i, nb, r
        errors = errors + 1
      end if
    end do

    test_gulong_in = errors
  end function test_gulong_in


  integer function test_uint16_in_out()
    integer :: i
    integer :: errors
    integer(c_int16_t) :: a, b
    integer(c_int32_t) :: c, d
    type(c_ptr) :: gv
    !! GVariant * g_variant_new_uint16 (guint16 uint16);
    !function g_variant_new_uint16(uint16) bind(c)
    !  use, intrinsic :: iso_c_binding, only: c_ptr, c_int16_t
    !  type(c_ptr) :: g_variant_new_uint16
    !  integer(c_int16_t), value :: uint16
    !end function

    !! guint16 g_variant_get_uint16 (GVariant *value);
    !function g_variant_get_uint16(value) bind(c)
    !  use, intrinsic :: iso_c_binding, only: c_int16_t, c_ptr
    !  integer(c_int16_t) :: g_variant_get_uint16
    !  type(c_ptr), value :: value
    !end function

    ! INTEGER(2) ranges from -32768 to 32767
    ! uint16 ranges from 0 to 65535.
    !***********************************
    ! We must be careful because the following loop is undefined in the Fortran Standard:
    ! do a = 0, 32767
    !                 1
    ! Warning: DO loop at (1) is undefined as it overflows [-Wundefined-do-loop]
    ! With some compilers, it will run OK, with others a will become <0 and the
    ! loop will never end...
    !***********************************
    errors = 0
    do a = 0, 32766
      gv = g_variant_new_uint16(a)
      b = g_variant_get_uint16(gv)
      if (a /= b) then
        print *, "ERROR g_variant_get_uint16:", a, b
        errors = errors + 1
      end if
      call g_variant_unref(gv)
    end do

    a = 32767
    gv = g_variant_new_uint16(a)
    b = g_variant_get_uint16(gv)
    if (a /= b) then
      print *, "ERROR g_variant_get_uint16:", a, b
      errors = errors + 1
    end if
    call g_variant_unref(gv)

    do c = 32768, 65535
      a = transfer(c, a)
      gv = g_variant_new_uint16(a)
      b = g_variant_get_uint16(gv)
      d = transfer(b, d)
      do i = bit_size(b),  bit_size(d)-1
        d= ibclr(d, i)
      end do

      if (a /= b) then
        print *, "ERROR g_variant_get_uint16:", a, b
        errors = errors + 1
      end if
      call g_variant_unref(gv)
    end do

    test_uint16_in_out = errors
  end function test_uint16_in_out


  integer function test_int16_in_out()
    integer(c_int16_t) :: a, b
    integer :: errors, i
    type(c_ptr) :: gv

    errors = 0

    do i = -32768, 32767
       a = int(i,c_int16_t)
       gv = g_variant_new_int16(a)
       b = g_variant_get_int16(gv)
       if (a /= b) then
          print *, "ERROR g_variant_get_int16:", a, b
          errors = errors + 1
       end if
       call g_variant_unref(gv)
    end do

    test_int16_in_out = errors
  end function test_int16_in_out


  integer function test_int32_in_out()
    integer :: i
    integer :: errors
    integer(c_int32_t) :: r, rmin, rmax
    integer(c_int32_t) :: a, b
    type(c_ptr) :: gv
    !GVariant *          g_variant_new_int32    (gint32 value);
    !gint32              g_variant_get_int32    (GVariant *value);
    errors = 0
    !***********************************
    ! We must be careful because the following loop is undefined in the Fortran Standard:
    ! do a = -huge(b), huge(b), +65536
    !
    ! Warning: DO loop at is undefined as it overflows [-Wundefined-do-loop]
    ! With some compilers, it will run OK (GFortran), with others a will become <0 and the
    ! loop will never end (ifort)...
    !***********************************
    do a = -huge(b), huge(b)-65536, +65536
      gv = g_variant_new_int32(a)
      b = g_variant_get_int32(gv)
      if (a /= b) then
        print *, "ERROR g_variant_get_int32:", a, b
        errors = errors + 1
      end if
      call g_variant_unref(gv)
    end do

    !    ! gint32 g_random_int_range (gint32 begin, gint32 end);
    !    function g_random_int_range(begin, end) bind(c)
    !      use, intrinsic :: iso_c_binding, only: c_int32_t
    !      integer(c_int32_t) :: g_random_int_range
    !      integer(c_int32_t), value :: begin
    !      integer(c_int32_t), value :: end
    !    end function
    rmin = -10
    rmax = +100
    do i = 1, 10000, +1
      ! Returns a random number over the range [rmin..rmax-1]:
      r = g_random_int_range(rmin, rmax)
      if ((r < rmin).or.(r >= rmax)) then
        print *, "ERROR g_random_double_range:", r
        errors = errors + 1
      end if
    end do

    test_int32_in_out = errors
  end function test_int32_in_out


  integer function test_uint32_in_out()
    integer :: i
    integer :: errors
    integer(c_int32_t) :: a, b
    integer(c_int64_t) :: c, d
    type(c_ptr) :: gv

    errors = 0
    ! INTEGER(4) ranges from -2147483648 to +2147483647
    ! uint32 ranges from 0 to 4294967295
    !***********************************
    ! We must be careful because the following loop is undefined in the Fortran Standard:
    ! do a = 0, 2147483647, +65536
    !
    ! With some compilers, it will run OK, with others a will become <0 and the
    ! loop will never end...
    !***********************************
    do a = 0, 2147483647-65536, +65536
      gv = g_variant_new_uint32(a)
      b = g_variant_get_uint32(gv)
      if (a /= b) then
        print *, "ERROR g_variant_get_uint32:", a, b
        errors = errors + 1
      end if
      call g_variant_unref(gv)
    end do

    a = 2147483647
    gv = g_variant_new_uint32(a)
    b = g_variant_get_uint32(gv)
    if (a /= b) then
      print *, "ERROR g_variant_get_uint32:", a, b
      errors = errors + 1
    end if
    call g_variant_unref(gv)

    do c = 2147483648_8, 4294967295_8, +65536
      a = transfer(c, a)
      gv = g_variant_new_uint32(a)
      b = g_variant_get_uint32(gv)
      d = transfer(b, d)
      do i = bit_size(b),  bit_size(d)-1
        d= ibclr(d, i)
      end do

      if (a /= b) then
        print *, "ERROR g_variant_get_uint32:", a, b
        errors = errors + 1
      end if
      call g_variant_unref(gv)
    end do

    test_uint32_in_out = errors
  end function test_uint32_in_out


  integer function test_guchar_in_out()
    integer(c_int16_t) :: i, j
    integer :: errors
    character(kind=c_char) :: a, b
    type(c_ptr) :: gv
    !GVariant *          g_variant_new_byte     (guchar value);
    !guchar              g_variant_get_byte     (GVariant *value);
    errors = 0
    do i = 0, 255, +1
      a = char(i)
      gv = g_variant_new_byte(int(i,c_int8_t))
      j = g_variant_get_byte(gv)
      b = achar(j)
      if ((a /= b) .or. (iand(i,255_c_int16_t) /= iand(j, 255_c_int16_t))) then
        print *, "ERROR test_guchar_in_out:", a, b, i, j
        errors = errors + 1
      end if
      call g_variant_unref(gv)
    end do

    test_guchar_in_out = errors
  end function test_guchar_in_out


  integer function test_gboolean_in_out()
    integer(c_int) :: l1, l2, l3, l4
    integer :: errors
    type(c_ptr) :: gv
    !GVariant *          g_variant_new_boolean  (gboolean value);
    !gboolean            g_variant_get_boolean               (GVariant *value);
    errors = 0
    l1 = TRUE
    gv = g_variant_new_boolean(l1)
    l2 = g_variant_get_boolean(gv)
    print *, l1, l2
    if (l1 /= l2) then
        print *, "ERROR g_variant_get_boolean:", l1, l2
        errors = errors + 1
    end if
    call g_variant_unref(gv)

    l1 = FALSE
    gv = g_variant_new_boolean(l1)
    l2 = g_variant_get_boolean(gv)
    print *, l1, l2
    if (l1 /= l2) then
        print *, "ERROR g_variant_get_boolean:", l1, l2
        errors = errors + 1
    end if

  !      ! gboolean g_hostname_is_ip_address (const gchar *hostname);
  !    function g_hostname_is_ip_address(hostname) bind(c)
  !      use, intrinsic :: iso_c_binding, only: c_bool, c_char
  !      logical(c_bool) :: g_hostname_is_ip_address
  !      character(kind=c_char), dimension(*) :: hostname
  !    end function
    l1 = g_hostname_is_ip_address("192.168.0.1"//c_null_char)
    l2 = g_hostname_is_ip_address("1AA.168.0.1"//c_null_char)
    l3 = g_hostname_is_ip_address("blabla"//c_null_char)
    l4 = g_hostname_is_ip_address("192.168,0.1"//c_null_char)
    if ((l1 /= TRUE) .or. (l2 /= FALSE) .or. (l3 /= FALSE) .or. (l4 /= FALSE)) then
      print *, "ERROR g_hostname_is_ip_address:", l1, l2, l3, l4
      errors = errors + 1
    end if
    call g_variant_unref(gv)

    test_gboolean_in_out = errors
  end function test_gboolean_in_out

end module tests


program gtk_fortran_test
  use tests

  implicit none
  integer :: errors
  character(:), allocatable :: os_string
  type(c_ptr) :: ret

  print '(A)', "Testing iso_c_binding with GTK and GLib..."

  ! That function may return NULL with some OS:
  ret = g_get_os_info("PRETTY_NAME"//c_null_char)
  if (c_associated(ret)) then
    call c_f_string_copy_alloc(ret, os_string)
  else
    os_string = "?"
  end if

  print '(3A,I0,A1,I0,A1,I0)', "Compiled with "//compiler_version()//" on ", os_string, &
      & ", linked to GTK ", gtk_get_major_version(),".", gtk_get_minor_version(), ".", gtk_get_micro_version()

  print '(A)', "test_iso_c_binding()"
  errors = test_iso_c_binding()
  print '(A)', "test_c_char_in_out()"
  errors = errors +  test_c_char_in_out()
  print '(A)', "test_guchar_in_out()"
  errors = errors +  test_guchar_in_out()
  print '(A)', "test_gdouble_in_out()"
  errors = errors +  test_gdouble_in_out()
  print '(A)', "test_gulong_in()"
  errors = errors +  test_gulong_in()
  print '(A)', "test_int16_in_out()"
  errors = errors +  test_int16_in_out()
  print '(A)', "test_uint16_in_out()"
  errors = errors +  test_uint16_in_out()
  print '(A)', "test_int32_in_out()"
  errors = errors +  test_int32_in_out()
  print '(A)', "test_uint32_in_out()"
  errors = errors +  test_uint32_in_out()
  print '(A)', "test_gboolean_in_out()"
  errors = errors +  test_gboolean_in_out()

  print *
  print '(I3, A)', errors, " errors"

end program gtk_fortran_test
