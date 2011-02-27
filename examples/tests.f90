! Automated tests
! If this program prints an ERROR message, please copy and send the message to
! vincent.magnin@libertysurf.fr
! gfortran ../src/gtk.f90 tests.f90 `pkg-config --cflags --libs gtk+-2.0`
! Vincent MAGNIN, 02-24-2011, last modified: 02-26-2011

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


!guint8              g_date_get_days_in_month            (GDateMonth month,
!                                                         GDateYear year);


program gtk_fortran_test
  implicit none
  
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
end program gtk_fortran_test
