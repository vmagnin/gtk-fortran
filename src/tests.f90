! 
! gfortran gtk2.o tests.f90 `pkg-config --cflags --libs gtk+-2.0`
! Vincent MAGNIN, 02-24-2011

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
      print *, "gchar g_ascii_tolower (gchar c): ", i, iachar(g_ascii_tolower(c))
    end if
  end do

  do i=65, 90, +1
    c = char(i)
    if (iachar(g_ascii_tolower(c)) /= i+32) then
      print *, "gchar g_ascii_tolower (gchar c): ", i, iachar(g_ascii_tolower(c))
    end if
  end do

  do i=91, 255, +1
    c = char(i)
    if (iachar(g_ascii_tolower(c)) /= i) then
      print *, "gchar g_ascii_tolower (gchar c): ", i, iachar(g_ascii_tolower(c))
    end if
  end do
end subroutine test_c_char_in_out


subroutine test_g_boolean_out
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
    print *, "g_hostname_is_ip_address:", l1, l2, l3, l4
  end if
end subroutine test_g_boolean_out


program gtk_fortran_test
  implicit none
  
  print *, "test_c_char_in_out"
  call test_c_char_in_out
  print *, "test_g_boolean_out"
  call test_g_boolean_out
end program gtk_fortran_test
