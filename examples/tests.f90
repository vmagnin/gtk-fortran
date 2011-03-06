! Automated tests
! gfortran ../src/gtk.f90 tests.f90 `pkg-config --cflags --libs gtk+-2.0`
! If this program prints an ERROR message, please copy and send the message to
! vincent.magnin@libertysurf.fr, with informations on your system (OS version,
! GTK+ version, compiler...)
! You can attach an errors.txt file obtained by the following command line:
! ./a.out > errors.txt
! Vincent MAGNIN, 02-24-2011, last modified: 02-26-2011


subroutine test_iso_c_binding
  use iso_c_binding

  if (C_SIGNED_CHAR < 0) then
    print *, C_SIGNED_CHAR
  end if
  if (C_SHORT < 0) then
    print *, "C_SHORT", C_SHORT
  end if
  if (C_INT < 0) then
    print *, "C_INT", C_INT
  end if
  if (C_LONG < 0) then
    print *, "C_LONG", C_LONG
  end if
  if (C_LONG_LONG < 0) then
    print *, "C_LONG_LONG", C_LONG_LONG
  end if
  if (C_SIZE_T < 0) then
    print *, "C_SIZE_T", C_SIZE_T
  end if
  if (C_INTPTR_T < 0) then
    print *, "C_INTPTR_T", C_INTPTR_T
  end if
  if (C_INTMAX_T < 0) then
    print *, "C_INTMAX_T", C_INTMAX_T
  end if
  if (C_INT8_T < 0) then
    print *, "C_INT8_T", C_INT8_T
  end if
  if (C_INT16_T < 0) then
    print *, "C_INT16_T", C_INT16_T
  end if
  if (C_INT32_T < 0) then
    print *, "C_INT32_T", C_INT32_T
  end if
  if (C_INT64_T < 0) then
    print *, "C_INT64_T", C_INT64_T
  end if
  if (C_INT_LEAST8_T < 0) then
    print *, "C_INT_LEAST8_T", C_INT_LEAST8_T
  end if
  if (C_INT_LEAST16_T < 0) then
    print *, "C_INT_LEAST16_T", C_INT_LEAST16_T
  end if
  if (C_INT_LEAST32_T < 0) then
    print *, "C_INT_LEAST32_T", C_INT_LEAST32_T
  end if
  if (C_INT_LEAST64_T < 0) then
    print *, "C_INT_LEAST64_T", C_INT_LEAST64_T
  end if
  if (C_INT_FAST8_T < 0) then
    print *, "C_INT_FAST8_T", C_INT_FAST8_T
  end if
  if (C_INT_FAST16_T < 0) then
    print *, "C_INT_FAST16_T", C_INT_FAST16_T
  end if
  if (C_INT_FAST32_T < 0) then
    print *, "C_INT_FAST32_T", C_INT_FAST32_T
  end if
  if (C_INT_FAST64_T < 0) then
    print *, "C_INT_FAST64_T", C_INT_FAST64_T
  end if
  if (C_FLOAT < 0) then
    print *, "C_FLOAT", C_FLOAT
  end if
  if (C_DOUBLE < 0) then
    print *, "C_DOUBLE", C_DOUBLE
  end if
  if (C_LONG_DOUBLE < 0) then
    print *, "C_LONG_DOUBLE", C_LONG_DOUBLE
  end if
  if (C_FLOAT_COMPLEX < 0) then
    print *, "C_FLOAT_COMPLEX", C_FLOAT_COMPLEX
  end if
  if (C_DOUBLE_COMPLEX < 0) then
    print *, "C_DOUBLE_COMPLEX", C_DOUBLE_COMPLEX
  end if
  if (C_LONG_DOUBLE_COMPLEX < 0) then
    print *, "C_LONG_DOUBLE_COMPLEX", C_LONG_DOUBLE_COMPLEX
  end if
  if (C_BOOL < 0) then
    print *, "C_BOOL", C_BOOL
  end if
  if (C_CHAR < 0) then
    print *, "C_CHAR", C_CHAR
  end if
end subroutine test_iso_c_binding


subroutine test_c_char_in_out
  use gtk
  implicit none
  integer :: i
  character(c_char) :: c
!    !  gchar g_ascii_tolower (gchar c) G_GNUC_CONST;
!    function g_ascii_tolower(c) bind(c) 
!      use iso_c_binding, only: c_char
!      character(c_char) :: g_ascii_tolower
!      character(c_char), value :: c
!    end function

  do i=0, 64, +1
    c = char(i)
    if (iachar(g_ascii_tolower(c)) /= i) then
      print *, "ERROR gchar g_ascii_tolower (gchar c): ", i, iachar(g_ascii_tolower(c))
    end if
  end do

  do i=65, 90, +1
    c = char(i)
    if (iachar(g_ascii_tolower(c)) /= i+32) then
      print *, "ERROR gchar g_ascii_tolower (gchar c): ", i, iachar(g_ascii_tolower(c))
    end if
  end do

  do i=91, 255, +1
    c = char(i)
    if (iachar(g_ascii_tolower(c)) /= i) then
      print *, "ERROR gchar g_ascii_tolower (gchar c): ", i, iachar(g_ascii_tolower(c))
    end if
  end do
end subroutine test_c_char_in_out


subroutine test_gboolean_out
  use gtk
  implicit none
  logical(c_bool) :: l1, l2, l3, l4
!      ! gboolean g_hostname_is_ip_address (const gchar *hostname);
!    function g_hostname_is_ip_address(hostname) bind(c) 
!      use iso_c_binding, only: c_bool, c_char
!      logical(c_bool) :: g_hostname_is_ip_address
!      character(kind=c_char), dimension(*) :: hostname
!    end function
  l1 = g_hostname_is_ip_address("192.168.0.1"//CNULL)
  l2 = g_hostname_is_ip_address("1AA.168.0.1"//CNULL)
  l3 = g_hostname_is_ip_address("blabla"//CNULL)
  l4 = g_hostname_is_ip_address("192.168,0.1"//CNULL)
  if ((l1 .neqv. .true.) .or. (l2 .neqv. .false.) .or. (l3 .neqv. .false.) .or. (l4 .neqv. .false.)) then
    print *, "ERROR g_hostname_is_ip_address:", l1, l2, l3, l4
  end if
end subroutine test_gboolean_out


subroutine test_gdouble_in_out
  use gtk
  implicit none
  integer :: i
  real(c_double) :: r, rmin, rmax
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
      print *, "ERROR g_random_double_range:", r
    end if
  end do
end subroutine test_gdouble_in_out
    

subroutine test_gint32_in_out
  use gtk
  implicit none
  integer :: i
  integer(c_int32_t) :: r, rmin, rmax
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
      print *, "ERROR g_random_double_range:", r
    end if
  end do
end subroutine test_gint32_in_out


subroutine test_gulong_in
  use gtk
  implicit none
  integer :: i
  integer(c_long) :: nb
  integer(c_int) :: r
!    ! guint g_bit_storage (gulong number) G_GNUC_CONST;
!    function g_bit_storage(number) bind(c) 
!      use iso_c_binding, only: c_int, c_long
!      integer(c_int) :: g_bit_storage
!      integer(c_long), value :: number
!    end function

! Fortran integers are signed. 32 bits integers are in [-2147483648, +2147483647].
! C language: typedef unsigned long   gulong;
  do i = 1, 31, +1
    nb = 2**i-1
    r = g_bit_storage(nb)
    if (i /= r) then
      print *, "ERROR g_bit_storage:", i, nb, r
    end if
  end do
end subroutine test_gulong_in


subroutine test_uint16_in_out
  use gtk
  implicit none
  integer :: i
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

! INTEGER(2) range from -32768 to 32767
! uint16 range from 0 to 65535.

  do a = 0, 32767
    b = g_variant_get_uint16(g_variant_new_uint16 (a))
    if (a /= b) then
      print *, "ERROR g_variant_get_uint16:", a, b
    end if
  end do
  
  do c = 65535, 65535
    a = transfer(c, a)
    b = g_variant_get_uint16(g_variant_new_uint16 (a))
    d = transfer(b, d)
    do i = bit_size(b),  bit_size(d)-1
      d= ibclr(d, i)
    end do

    print *, c, a, b, d
    do i = 0,  bit_size(c)-1
      write(*, "(l1)", advance="no") btest(c, i)
    end do
    write(*,*)
    do i = 0,  bit_size(a)-1
      write(*, "(l1)", advance="no") btest(a, i)
    end do
    write(*,*)
    do i = 0,  bit_size(b)-1
      write(*, "(l1)", advance="no") btest(b, i)
    end do
    write(*,*)    
    do i = 0,  bit_size(d)-1
      write(*, "(l1)", advance="no") btest(d, i)
    end do
    write(*,*)    
    if (a /= b) then
      print *, "ERROR g_variant_get_uint16:", a, b
    end if
  end do
end subroutine test_uint16_in_out


subroutine test_int16_in_out
  use gtk
  implicit none
  integer(c_int16_t) :: a, b

  do a = -32768, 32767
    b = g_variant_get_int16(g_variant_new_int16 (a))
    if (a /= b) then
      print *, "ERROR g_variant_get_int16:", a, b
    end if
  end do
end subroutine test_int16_in_out

!guint8              g_date_get_days_in_month            (GDateMonth month,
!                                                         GDateYear year);
!GSocketAddress *    g_inet_socket_address_new           (GInetAddress *address,
!                                                         guint16 port);
!guint16             g_inet_socket_address_get_port      (GInetSocketAddress *address);

!See the official documentation:
!http://library.gnome.org/devel/glib/stable/glib-GVariant.html
!http://library.gnome.org/devel/glib/stable/glib-GVariantType.html

!GVariant *                       g_variant_new_boolean  (gboolean value);
!GVariant *                       g_variant_new_byte     (guchar value);
!GVariant *                       g_variant_new_int16    (gint16 value);
!GVariant *                       g_variant_new_uint16   (guint16 value);
!GVariant *                       g_variant_new_int32    (gint32 value);
!GVariant *                       g_variant_new_uint32   (guint32 value);
!GVariant *                       g_variant_new_int64    (gint64 value);
!GVariant *                       g_variant_new_uint64   (guint64 value);
!GVariant *                       g_variant_new_handle   (gint32 value);
!GVariant *                       g_variant_new_double   (gdouble value);
!GVariant *                       g_variant_new_string   (const gchar *string);

!gboolean            g_variant_get_boolean               (GVariant *value);
!guchar              g_variant_get_byte                  (GVariant *value);
!gint16              g_variant_get_int16                 (GVariant *value);
!guint16             g_variant_get_uint16                (GVariant *value);
!gint32              g_variant_get_int32                 (GVariant *value);
!guint32             g_variant_get_uint32                (GVariant *value);
!gint64              g_variant_get_int64                 (GVariant *value);
!guint64             g_variant_get_uint64                (GVariant *value);
!gint32              g_variant_get_handle                (GVariant *value);
!gdouble             g_variant_get_double                (GVariant *value);
!const gchar *                    g_variant_get_string   (GVariant *value,
!                                                         gsize *length);

program gtk_fortran_test
  implicit none
  
  call test_iso_c_binding
  print *, "test_c_char_in_out"
  call test_c_char_in_out
  print *, "test_g_boolean_out"
  call test_gboolean_out
  print *, "test_g_double_in_out"
  call test_gdouble_in_out
  print *, "test_gint32_in_out"
  call test_gint32_in_out
  print *, "test_gulong_in"
  call test_gulong_in
  print *, "test_int16_in_out"
  call test_int16_in_out
  print *, "test_uint16_in_out"
  call test_uint16_in_out
end program gtk_fortran_test
