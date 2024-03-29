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
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
! GNU General Public License for more details.
!
! Under Section 7 of GPL version 3, you are granted additional
! permissions described in the GCC Runtime Library Exception, version
! 3.1, as published by the Free Software Foundation.
!
! You should have received a copy of the GNU General Public
! License along with
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!------------------------------------------------------------------------------
! Contributed by James Tappin, Ian Harvey (IanH0073)
! Last modifications: 2012-06-20, vmagnin+IanH0073 2019-02-21
! vmagnin 2020-06-08 (GTK 4), jtappin 2023-09-22
!------------------------------------------------------------------------------
!*
! Supplementary material
!
! This module contains some supplementary material useful for writing GTK
! programs in Fortran.  The functions are not listed in the
! gtk-fortran-index.csv file, either because they are not (currenty) extracted
! by the automatic tools, or because they are just convenient functions
! added here.
module gtk_sup
  ! Currently it contains:
  !
  ! * GTYPE: Definitions of the integer length and the values for each type.
  ! * GtkTreeIter: Type definition, and its clear procedure.
  ! * GValue: Pseudo type definition, and its clear procedure.
  ! * GtkTextIter: Type definition.
  ! * GError: Type definition.
  ! * Interfaces for string conversions.
  ! * Boolean conversion routines.
  ! * Miscellaneous functions.
  !
  !/

  use, intrinsic :: iso_c_binding
  use gtk, only: TRUE, FALSE
  use g, only: g_type_fundamental

  implicit none

  public :: clear_gtktreeiter, clear_gvalue, c_f_string_chars, c_f_string_ptr,&
          & c_f_string_copy_alloc, c_f_string_copy, &
          & convert_c_string_scalar, convert_c_string_array, &
          & convert_c_string_scalar_cptr, convert_c_string_array_cptr, &
          & convert_f_string_a, convert_f_string_s, &
          & c_f_logical, f_c_logical4, &
          & f_c_logical1, is_UNIX_OS, fdate, copy_file

  !============================================================================
  !+
  ! Gtype
  ! The various Gtype definitions from the gtype.h file.
  !-

  ! Gtype definitions
  integer, parameter :: type_kind = c_size_t
  integer(c_int), parameter :: g_type_fundamental_shift=2
  integer(type_kind), parameter :: G_TYPE_INVALID = &
       & ishft(0_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_NONE = &
       & ishft(1_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_INTERFACE = &
       & ishft(2_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_CHAR = &
       & ishft(3_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_UCHAR = &
       & ishft(4_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_BOOLEAN = &
       & ishft(5_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_INT = &
       & ishft(6_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_UINT = &
       & ishft(7_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_LONG = &
       & ishft(8_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_ULONG = &
       & ishft(9_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_INT64 = &
       & ishft(10_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_UINT64 = &
       & ishft(11_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_ENUM = &
       & ishft(12_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_FLAGS = &
       & ishft(13_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_FLOAT = &
       & ishft(14_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_DOUBLE = &
       & ishft(15_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_STRING = &
       & ishft(16_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_POINTER = &
       & ishft(17_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_BOXED = &
       & ishft(18_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_PARAM = &
       & ishft(19_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_OBJECT = &
       & ishft(20_type_kind, g_type_fundamental_shift)
  integer(type_kind), parameter :: G_TYPE_VARIANT = &
       & ishft(21_type_kind, g_type_fundamental_shift)

  !============================================================================
  !+
  ! Iterators and Gvalues
  ! These structures are always allocated in the calling program, rather
  ! than being declared as pointers and leaving GTK to allocate them.
  !
  ! * GtkTreeIter, Iterator for TreeView widgets
  ! * GtkTextIter, Iterator for TextView widgets
  ! * GValue, A value container.
  !-

  ! Define a GtkTreeIter (this has to be pre-allocated in the calls)
  type, bind(c) :: gtktreeiter
     integer(c_int) :: intv = 0
     type(c_ptr) :: p0 = C_NULL_PTR, p1 = C_NULL_PTR, p2 = C_NULL_PTR
  end type gtktreeiter

  ! Define a spacemaker for GValue It's 24 bytes on 64 bit & 20 on 32,
  ! i.e. one long and 2 64-bit integers
  type, bind(c) :: gvalue
     integer(c_long) :: il = 0
     integer(c_int64_t), dimension(2) :: i64 = [0, 0]
  end type gvalue

  ! Define a GtkTextIter (this has to be pre-allocated in the calls)
  type, bind(c) :: gtktextiter
     type(c_ptr)    :: d1, d2
     integer(c_int) :: d3, d4, d5, d6, d7, d8
     type(c_ptr)    :: d9, d10
     integer(c_int) :: d11, d12, d13
     type(c_ptr)    :: d14
  end type gtktextiter

  !+
  ! GError
  ! GError is a transparent structure that returns error information.
  !-
  type, bind(c) :: gerror
     integer(c_int32_t) :: domain
     integer(c_int) :: code
     type(c_ptr) :: message    ! A C pointer to the error message.
  end type gerror

  !============================================================================
  ! Interfaces for conversions
  ! Some are available with two names:
  ! convert_c_string = c_f_string
  ! convert_f_string = f_c_string
  !============================================================================
  interface c_f_string
     module procedure convert_c_string_scalar
     module procedure convert_c_string_array
     module procedure convert_c_string_scalar_cptr
     module procedure convert_c_string_array_cptr
  end interface c_f_string

  interface convert_c_string
     module procedure convert_c_string_scalar
     module procedure convert_c_string_array
     module procedure convert_c_string_scalar_cptr
     module procedure convert_c_string_array_cptr
  end interface convert_c_string

  interface f_c_string
     module procedure convert_f_string_a
     module procedure convert_f_string_aa
     module procedure convert_f_string_s
  end interface f_c_string

  interface convert_f_string
     module procedure convert_f_string_a
     module procedure convert_f_string_aa
     module procedure convert_f_string_s
  end interface convert_f_string

  ! Interfaces for logical conversion
  interface f_c_logical
     module procedure f_c_logical4
     module procedure f_c_logical1
  end interface f_c_logical

  ! String conversion routines below lazily use the C standard library function
  ! strlen(). If this is not available for some bizarre reason, then a
  ! simple index of the null character will suffice.
  ! Contributed by Ian Harvey, 2014.
  interface
    function strlen(str) bind(c, name='strlen')
      import :: c_size_t, c_ptr
      implicit none
      type(c_ptr), intent(in), value :: str
      integer(c_size_t) :: strlen
    end function strlen
  end interface

contains
  !============================================================================
  ! These 2 clear_ routines are only needed of you need to re-initialize
  ! the types. The definitions include the initial setting to zero or NULL.
  !============================================================================

  !+
  subroutine clear_gtktreeiter(iter)
    type(gtktreeiter), intent(inout) :: iter

    ! Clear a tree iterator
    !
    ! ITER |  gtktreeiter |  required |  The iterator to clear
    !-

    iter%intv = 0
    iter%p0   = C_NULL_PTR
    iter%p1   = C_NULL_PTR
    iter%p2   = C_NULL_PTR
  end subroutine clear_gtktreeiter

  !+
  subroutine clear_gvalue(gval)
    type(gvalue), intent(inout) :: gval

    ! Clear a GValue
    !
    ! GVAL |  gvalue |  required |  The GValue to clear.
    !-
    gval%il  = 0
    gval%i64 = [0,0]
  end subroutine clear_gvalue

  !============================================================================
  ! Some string conversion routines
  !============================================================================
  !+
  subroutine c_f_string_copy_alloc(the_ptr, f_string)
    type(c_ptr), intent(in) :: the_ptr
    character(:), intent(out), allocatable :: f_string
    character(kind=c_char), pointer :: f_array(:)
    integer :: i

    ! Create a default character deferred length allocatable copy of the
    ! value of a C string. This function should be preferred to
    ! c_f_string_copy() when using a Fortran>=2008 compiler. An advantage is
    ! that the trim() function will generally not be needed.
    !
    ! THE_PTR |  string |  required |   The C string to be converted.
    ! F_STRING |  f_string |  required |  A Scalar Fortran string.
    !
    !-
    ! Contributed by Ian Harvey, 2014

    call c_f_pointer(the_ptr, f_array, [strlen(the_ptr)])
    allocate(character(size(f_array)) :: f_string)

    do concurrent (i = 1:size(f_array))
      f_string(i:i) = f_array(i)
    end do
  end subroutine c_f_string_copy_alloc

  !+
  subroutine c_f_string_copy(the_ptr, f_string, status)
    type(c_ptr), intent(in) :: the_ptr
    character(*), intent(out) :: f_string
    integer, intent(out), optional :: status
    character(kind=c_char), pointer :: f_array(:)
    integer :: i

    ! Create a default character fixed length copy of the value of a C string.
    !
    ! THE_PTR |  string |  required |   The C string to be converted.
    ! F_STRING |  f_string |  required |  A Scalar Fortran string.
    ! STATUS |  integer |  optional |  Is set to -1 if the Fortran string is too short.
    !
    ! If the Fortran string is too short, the C string is truncated.
    !-

    call c_f_pointer(the_ptr, f_array, [strlen(the_ptr)])

    do i = 1, size(f_array)
      if (i > len(f_string)) then
        if (present(status)) status = -1
        return
      end if

      f_string(i:i) = f_array(i)
    end do

    ! i here is size(f_array) + 1. Define the remainder of f_string:
    f_string(i:) = ''

    if (present(status)) status = 0
  end subroutine c_f_string_copy

  !+
  subroutine c_f_string_chars(c_string, f_string)
    character(len=1, kind=C_char), intent(in) :: c_string(*)
    character(len=*), intent(out) :: f_string
    integer :: i

    ! Copy a C string, passed as a char-array reference, to a Fortran string.
    ! String routine from C_interface_module by Joseph M. Krahn:
    ! http://fortranwiki.org/fortran/show/c_interface_module
    !
    ! C_STRING |  chars array |  required |   The C chars array to be converted.
    ! F_STRING |  f_string |  required |  A Scalar Fortran string.
    !
    !-

    i = 1
    do while (c_string(i) /= c_null_char .and. i <= len(f_string))
      f_string(i:i) = c_string(i)
      i = i + 1
    end do

    if (i < len(f_string)) f_string(i:) = ' '
  end subroutine c_f_string_chars

  !+
  subroutine c_f_string_ptr(c_string, f_string)
    type(C_ptr), intent(in) :: c_string
    character(len=*), intent(out) :: f_string
    character(len=1, kind=C_char), dimension(:), pointer :: p_chars
    integer :: i

    ! Copy a C string, passed by pointer, to a Fortran string.
    ! If the C pointer is NULL, the Fortran string is blanked.
    ! c_string must be NUL terminated, or at least as long as f_string.
    ! If c_string is longer, it is truncated. Otherwise, f_string is
    ! blank-padded at the end.
    !
    ! C_STRING |  string |  required |   The C string to be converted.
    ! F_STRING |  f_string |  required |  A Scalar Fortran string.
    !
    !-

    if (.not. c_associated(c_string)) then
      f_string = ' '
    else
      call c_f_pointer(c_string, p_chars, [huge(0)])

      i = 1
      do while (p_chars(i) /= c_null_char .and. i <= len(f_string))
        f_string(i:i) = p_chars(i)
        i = i + 1
      end do

      if (i < len(f_string)) f_string(i:) = ' '
    end if
  end subroutine c_f_string_ptr

  !============================
  ! convert_c_string interface
  !============================
  !+
  subroutine convert_c_string_scalar(textptr, f_string, status)
    character(kind=c_char), dimension(:), intent(in) :: textptr
    character(len=*), intent(out) :: f_string
    integer(c_int), intent(out), optional :: status

    ! Convert a null-terminated C-string to a Fortran string
    !
    ! TEXTPTR |  string |  required |   The C string to be converted.
    ! F_STRING |  f_string |  required |  A Scalar Fortran string.
    ! STATUS |  integer |  optional |  Is set to -1 if the Fortran string is too short.
    !
    ! Usually called via the convert_c_string generic interface.
    !-
    ! Contributed by jtappin and Ian Harvey.

    integer :: i

    do i = 1, size(textptr)
      if (textptr(i) == c_null_char) exit
      if (i > len(f_string)) then
        if (present(status)) status = -1  ! Output string not long enough
        return
      end if
      f_string(i:i) = textptr(i)
    end do

    f_string(i:) = ''

    if (present(status)) status = 0
  end subroutine convert_c_string_scalar

  !+
  subroutine convert_c_string_array(textptr, f_string, status)
    character(kind=c_char), dimension(:), intent(in) :: textptr
    character(len=*), intent(out), dimension(:), allocatable :: f_string
    integer, intent(out), optional :: status

    ! Convert a null-terminated LF-separated C-string into a Fortran
    ! string array
    !
    ! TEXTPTR |  string |  required |   The C string to be converted.
    ! F_STRING |  f_string() |  required |  A Fortran string array.
    ! STATUS |  integer |  optional |  Is set to -1 if the Fortran string is too short for any line.
    !
    ! Usually called via the convert_c_string generic interface.
    !-

    integer :: i, j, ii, count

    count = 1
    i = 1
    do
       if (i > size(textptr)) exit
       if (textptr(i) == c_null_char) exit
       if (textptr(i) == c_new_line) count = count+1
       i = i + 1
    end do
    allocate(f_string(count))

    if (present(status)) status = 0
    ii = 1
    do j = 1, count
       f_string(j) = ""
       do i = 1, len(f_string)
          if (ii > size(textptr)) then
             if (j < count .and. present(status)) status = -1
             return
          end if

          if (textptr(ii) == c_null_char) return
          if (textptr(ii) == c_new_line) then
             ii = ii + 1
             exit
          end if
          f_string(j)(i:i) = textptr(ii)
          ii = ii + 1
       end do
       if (i > len(f_string) .and. present(status)) &
            & status = -1  ! Output string not long enough
    end do
  end subroutine convert_c_string_array

  !+
  subroutine convert_c_string_scalar_cptr(ctext, f_string, status)
    type(c_ptr), intent(in) :: ctext
    character(len=*), intent(out) :: f_string
    integer, intent(out), optional :: status

    ! Convert a null-terminated C-string to a Fortran string
    !
    ! CTEXT |  c_ptr |  required |   A C poiner to string to be converted.
    ! F_STRING |  f_string |  required |  A Scalar Fortran string.
    ! STATUS |  integer |  optional |  Is set to -1 if the Fortran string is too short.
    !
    ! Usually called via the convert_c_string generic interface.
    !-
    ! Contributed by jtappin and Ian Harvey.

    integer :: i
    character(kind=c_char), dimension(:), pointer :: textptr

    call c_f_pointer(ctext, textptr, [ strlen(ctext) ])

    do i = 1, size(textptr)
      if (i > len(f_string)) then
        if (present(status)) status = -1  ! Output string not long enough
        return
      end if
      f_string(i:i)=textptr(i)
    end do

    f_string(i:) = ''
    if (present(status)) status = 0
  end subroutine convert_c_string_scalar_cptr

  !+
  subroutine convert_c_string_array_cptr(ctext, f_string, status)
    type(c_ptr), intent(in) :: ctext
    character(len=*), intent(out), dimension(:), allocatable :: f_string
    integer, intent(out), optional :: status

    ! Convert a null-terminated LF-separated C-string into a Fortran
    ! string array
    !
    ! CTEXT |  c_ptr |  required |   A C poiner to string to be converted.
    ! F_STRING |  f_string() |  required |  A  Fortran string. array
    ! STATUS |  integer |  optional |  Is set to -1 if the Fortran string is too short for any of the lines.
    !
    ! Usually called via the convert_c_string generic interface.
    !-

    integer :: i, j, ii, count
    character(kind=c_char), dimension(:), pointer :: textptr

    call c_f_pointer(ctext, textptr, [ strlen(ctext) ])
    ! count = COUNT(textptr == c_new_line)
    count = 1
    i = 1
    do
       if (i > size(textptr)) exit
       if (textptr(i) == c_new_line) count = count + 1
       i = i + 1
    end do
    allocate(f_string(count))

    if (present(status)) status = 0

    ii = 1
    do j = 1, count
       f_string(j) = ""
       do i = 1, len(f_string)
          if (ii > size(textptr)) then
             return
          end if

          if (textptr(ii) == c_new_line) then
             ii = ii + 1
             exit
          end if
          f_string(j)(i:i) = textptr(ii)
          ii = ii + 1
       end do
       if (i > len(f_string) .and. present(status)) &
            & status = -1  ! Output string not long enough
    end do
  end subroutine convert_c_string_array_cptr

  !============================
  ! convert_f_string interface
  !============================
  !+
  subroutine convert_f_string_a(f_string, textptr, length)
    character(len=*), intent(in), dimension(:) :: f_string
    character(kind=c_char), dimension(:), intent(out), allocatable :: textptr
    integer(c_int), intent(out), optional :: length

    ! Convert a Fortran string array into a null-terminated, LF_separated
    ! C-string
    !
    ! F_STRING |  f_string |  required |  The Fortran string to convert
    ! TEXTPR |  string |  required |  A C type string, (allocatable).
    ! LENGTH |  c_int |  optional |  The length of the generated C string.
    !-

    integer :: lcstr, i, j, ii, nfstr
    integer, dimension(:), allocatable :: lfstr

    nfstr = size(f_string)
    allocate(lfstr(nfstr))
    lfstr = len_trim(f_string)

    lcstr = sum(lfstr)
    do i = 1, nfstr-1
       if (lfstr(i) == 0) then
          lcstr = lcstr + 1
       else if (f_string(i)(lfstr(i):lfstr(i)) /= c_null_char .and. &
            & f_string(i)(lfstr(i):lfstr(i)) /= c_new_line) then
          lcstr = lcstr + 1
       end if
    end do
    if (lfstr(nfstr) == 0) then
       lcstr = lcstr + 1
    else if (f_string(nfstr)(lfstr(nfstr):lfstr(nfstr)) /= c_null_char) then
       lcstr = lcstr + 1
    end if

    allocate(textptr(lcstr))
    if (present(length)) length = lcstr

    ii = 1
    do i = 1, nfstr
       do j = 1, lfstr(i)
          if (j == lfstr(i) .and. &
               & (f_string(i)(j:j) == c_null_char .or. &
               & (f_string(i)(j:j) == c_new_line .and. i /= nfstr))) exit
          textptr(ii) = f_string(i)(j:j)
          ii = ii + 1
       end do
       if (i < nfstr) then
          textptr(ii) = c_new_line
       else
          textptr(ii) = c_null_char
       end if
       ii = ii + 1
    end do
  end subroutine convert_f_string_a
  
  !+
  subroutine convert_f_string_aa(f_string, textptr, length)
    character(len=*), intent(in), dimension(:) :: f_string
    character(kind=c_char), dimension(:,:), intent(out), &
         & allocatable :: textptr
    integer(kind=c_int), intent(out), optional, &
         & dimension(:), allocatable :: length

    ! Convert a fortran string array into an array of null-terminated
    ! C strings. 
    !
    ! F_STRING: f_string: required: The fortran string to convert
    ! TEXTPR: string: required: A C type string, (allocatable) 2-D
    ! 	fortran array.
    ! LENGTH: c_int: optional: An allocatable integer array to return
    ! the lengths of the strings.	
    !-

    integer :: lcstr, i, j, nfstr
    integer, dimension(:), allocatable :: lfstr

    nfstr = size(f_string)
    allocate(lfstr(nfstr))
    lfstr = len_trim(f_string)
    lcstr = maxval(lfstr)+1    ! Add 1 for the NULL terminator on the
    ! longest element

    allocate(textptr(lcstr,nfstr))
    if (present(length)) then
       allocate(length(nfstr))
       length = lfstr
    end if

    do i = 1, nfstr
       do j = 1, lfstr(i)
          textptr(j,i) = f_string(i)(j:j)
       end do
       textptr(j,i) = c_null_char
    end do
  end subroutine convert_f_string_aa
  

  !+
  subroutine convert_f_string_s(f_string, textptr, length)
    character(len=*), intent(in) :: f_string
    character(kind=c_char), dimension(:), intent(out), allocatable :: textptr
    integer(c_int), intent(out), optional :: length

    ! Convert a Fortran string into a null-terminated C-string
    !
    ! F_STRING |  f_string |  required |  The Fortran string to convert
    ! TEXTPR |  string |  required |  A C type string, (allocatable).
    ! LENGTH |  c_int |  optional |  The length of the generated C string.
    !-

    integer :: lcstr, j
    logical :: add_null

    lcstr = len_trim(f_string)
    if (lcstr == 0) then
       lcstr = lcstr + 1
       add_null = .true.
    else if (f_string(lcstr:lcstr) /= c_null_char) then
       lcstr = lcstr + 1
       add_null = .true.
    else
       add_null = .false.
    end if

    allocate(textptr(lcstr))
    if (present(length)) length = lcstr

    do j = 1, len_trim(f_string)
       textptr(j) = f_string(j:j)
    end do

    if (add_null) textptr(lcstr) = c_null_char
  end subroutine convert_f_string_s

  !============================================================================
  ! Boolean conversion routines
  ! f_c_logical interface is available for logical(1) and logical(4)
  !============================================================================
  !+
  function c_f_logical(cbool)
    logical :: c_f_logical
    integer(c_int), intent(in) :: cbool

    ! Convert a gboolean to a Fortran logical
    !
    ! CBOOL |  boolean |  required |  The Gboolean to convert.
    !-

    if (cbool == FALSE) then
       c_f_logical = .false.
    else
       c_f_logical = .true.
    end if
  end function c_f_logical

  !+
  function f_c_logical4(flog)
    integer(c_int) :: f_c_logical4
    logical, intent(in) :: flog

    ! Convert a Fortran default logical to a gboolean
    !
    ! FLOG |  logical |  required |  The Fortran logical to convert.
    !
    ! Usually accessed via the generic f_c_logical interface
    !-

    if (flog) then
       f_c_logical4 = TRUE
    else
       f_c_logical4 = FALSE
    end if
  end function f_c_logical4

  !+
  function f_c_logical1(flog)
    integer(c_int) :: f_c_logical1
    logical(1), intent(in) :: flog

    ! Convert a Fortran 1-byte logical to a gboolean
    !
    ! FLOG |  logical*1 |  required |  The Fortran logical to convert.
    !
    ! Usually accessed via the generic f_c_logical interface
    !-

    if (flog) then
       f_c_logical1 = TRUE
    else
       f_c_logical1 = FALSE
    end if
  end function f_c_logical1

  !============================================================================
  ! Miscellaneous
  !============================================================================

  !+
  function is_UNIX_OS()
    use g, only: g_get_current_dir

    logical :: is_UNIX_OS
    character(:), allocatable :: path

    ! Returns .true. if the OS is of the UNIX type. On a Windows system, it
    ! will return .false. because an absolute path can not begin by a slash.
    !-

    call c_f_string_copy_alloc(g_get_current_dir(), path)

    if (path(1:1) == "/") then
        is_UNIX_OS = .true.
    else
        is_UNIX_OS = .false.
    endif
  end function is_UNIX_OS

  !+
  function fdate()
    character(29) :: fdate
    character(8)  :: date
    character(10) :: time
    character(5)  :: zone

    ! Returns date, time and timezone in a string without spaces,
    ! for example: 2022-05-06T15:58:43.790+02:00
    !-
    ! Contributed by IanH0073 (issue #81)

    call date_and_time(date, time, zone)
    fdate = date(1:4) // '-' // date(5:6) // '-' // date(7:8)  &
            // 'T' // time(1:2) // ':' // time(3:4) // ':' // time(5:10)  &
            // zone(1:3) // ':' // zone(4:5)
  end function fdate

  ! A function to copy a text file
  ! Used especially in sketcher/gtkf-sketcher.f90
  subroutine copy_file(source, destination)
    character(*), intent(in) :: source
    character(*), intent(in) :: destination
    character(len=256, kind=c_char) :: line
    integer :: status_read, input, output

    open(newunit=output, file=destination, action='write')
    open(newunit=input,  file=source, action='read')
    do
      read(input, '(A)', iostat=status_read) line
      if ( status_read /= 0 ) exit
      write(output, '(A)') line(1:len_trim(line))
    enddo
    close(input)
    close(output)
  end subroutine copy_file

end module gtk_sup
