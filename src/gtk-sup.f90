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

! You should have received a copy of the GNU General Public
!  License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
!
! Contributed by James Tappin
! Last modification: 04-01-2011

module gtk_sup
  ! The definitions in this supplementary module are ones that are not (yet)
  !  automatically extracted from the GTK headers.

  ! Currently it contains:
  ! GTYPE: Definitions of the integer length and the values for each type.
  ! GtkTreeIter: Type definition.
  ! GValue: Pseudo type definition.

  use iso_c_binding
  use gtk

  implicit none

  ! Gtype definitions
  integer, parameter :: type_kind=c_long
  integer(kind=c_int), parameter :: g_type_fundamental_shift=2
  integer(kind=type_kind), parameter :: G_TYPE_INVALID = &
       & ishft(0_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_NONE = &
       & ishft(1_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_INTERFACE = &
       & ishft(2_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_CHAR = &
       & ishft(3_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_UCHAR = &
       & ishft(4_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_BOOLEAN = &
       & ishft(5_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_INT = &
       & ishft(6_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_UINT = &
       & ishft(7_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_LONG = &
       & ishft(8_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_ULONG = &
       & ishft(9_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_INT64 = &
       & ishft(10_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_UINT64 = &
       & ishft(11_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_ENUM = &
       & ishft(12_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_FLAGS = &
       & ishft(13_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_FLOAT = &
       & ishft(14_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_DOUBLE = &
       & ishft(15_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_STRING = &
       & ishft(16_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_POINTER = &
       & ishft(17_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_BOXED = &
       & ishft(18_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_PARAM = &
       & ishft(19_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_OBJECT = &
       & ishft(20_type_kind, g_type_fundamental_shift)
  integer(kind=type_kind), parameter :: G_TYPE_VARIANT = &
       & ishft(21_type_kind, g_type_fundamental_shift)


  ! Define a GtkTreeIter (this has to be pre-allocated in the calls)
  type, bind(c) :: gtktreeiter
     integer(kind=c_int) :: intv=0
     type(c_ptr) :: p0=NULL, p1=NULL, p2=NULL
  end type gtktreeiter

! Define a spacemaker for GValue It's 24 bytes on 64 bit & 20 on 32,
! i.e. one long and 2 64-bit integers
  type, bind(c) :: gvalue
     integer(kind=c_long) :: il = 0
     integer(kind=c_int64_t), dimension(2) :: i64 = (/0, 0/)
  end type gvalue

! Stock button and icon names extracted from gtk/gtkstock.h
  character(len=len("gtk-about")+1), parameter :: GTK_STOCK_ABOUT = &
       & "gtk-about"//cnull
!!$  GTK_STOCK_ADD clashes with the routine gtk_stock_add
!!$  character(len=len("gtk-add")+1), parameter :: GTK_STOCK_ADD = &
!!$       & "gtk-add"//cnull
  character(len=len("gtk-apply")+1), parameter :: GTK_STOCK_APPLY = &
       & "gtk-apply"//cnull
  character(len=len("gtk-bold")+1), parameter :: GTK_STOCK_BOLD = &
       & "gtk-bold"//cnull
  character(len=len("gtk-cancel")+1), parameter :: GTK_STOCK_CANCEL = &
       & "gtk-cancel"//cnull
  character(len=len("gtk-caps-lock-warning")+1), parameter :: GTK_STOCK_CAPS_LOCK_WARNING= &
       & "gtk-caps-lock-warning"//cnull
  character(len=len("gtk-cdrom")+1), parameter :: GTK_STOCK_CDROM = &
       & "gtk-cdrom"//cnull
  character(len=len("gtk-clear")+1), parameter :: GTK_STOCK_CLEAR = &
       & "gtk-clear"//cnull
  character(len=len("gtk-close")+1), parameter :: GTK_STOCK_CLOSE = &
       & "gtk-close"//cnull
  character(len=len("gtk-color-picker")+1), parameter :: GTK_STOCK_COLOR_PICKER = &
       & "gtk-color-picker"//cnull
  character(len=len("gtk-connect")+1), parameter :: GTK_STOCK_CONNECT = &
       & "gtk-connect"//cnull
  character(len=len("gtk-convert")+1), parameter :: GTK_STOCK_CONVERT = &
       & "gtk-convert"//cnull
  character(len=len("gtk-copy")+1), parameter :: GTK_STOCK_COPY = &
       & "gtk-copy"//cnull
  character(len=len("gtk-cut")+1), parameter :: GTK_STOCK_CUT = &
       & "gtk-cut"//cnull
  character(len=len("gtk-delete")+1), parameter :: GTK_STOCK_DELETE = &
       & "gtk-delete"//cnull
  character(len=len("gtk-dialog-authentication")+1), parameter :: GTK_STOCK_DIALOG_AUTHENTICATION= &
       & "gtk-dialog-authentication"//cnull
  character(len=len("gtk-dialog-info")+1), parameter :: GTK_STOCK_DIALOG_INFO = &
       & "gtk-dialog-info"//cnull
  character(len=len("gtk-dialog-warning")+1), parameter :: GTK_STOCK_DIALOG_WARNING = &
       & "gtk-dialog-warning"//cnull
  character(len=len("gtk-dialog-error")+1), parameter :: GTK_STOCK_DIALOG_ERROR = &
       & "gtk-dialog-error"//cnull
  character(len=len("gtk-dialog-question")+1), parameter :: GTK_STOCK_DIALOG_QUESTION = &
       & "gtk-dialog-question"//cnull
  character(len=len("gtk-directory")+1), parameter :: GTK_STOCK_DIRECTORY = &
       & "gtk-directory"//cnull
  character(len=len("gtk-discard")+1), parameter :: GTK_STOCK_DISCARD = &
       & "gtk-discard"//cnull
  character(len=len("gtk-disconnect")+1), parameter :: GTK_STOCK_DISCONNECT = &
       & "gtk-disconnect"//cnull
  character(len=len("gtk-dnd")+1), parameter :: GTK_STOCK_DND = &
       & "gtk-dnd"//cnull
  character(len=len("gtk-dnd-multiple")+1), parameter :: GTK_STOCK_DND_MULTIPLE = &
       & "gtk-dnd-multiple"//cnull
  character(len=len("gtk-edit")+1), parameter :: GTK_STOCK_EDIT = &
       & "gtk-edit"//cnull
  character(len=len("gtk-execute")+1), parameter :: GTK_STOCK_EXECUTE = &
       & "gtk-execute"//cnull
  character(len=len("gtk-file")+1), parameter :: GTK_STOCK_FILE = &
       & "gtk-file"//cnull
  character(len=len("gtk-find")+1), parameter :: GTK_STOCK_FIND = &
       & "gtk-find"//cnull
  character(len=len("gtk-find-and-replace")+1), parameter :: GTK_STOCK_FIND_AND_REPLACE= &
       & "gtk-find-and-replace"//cnull
  character(len=len("gtk-floppy")+1), parameter :: GTK_STOCK_FLOPPY = &
       & "gtk-floppy"//cnull
  character(len=len("gtk-fullscreen")+1), parameter :: GTK_STOCK_FULLSCREEN = &
       & "gtk-fullscreen"//cnull
  character(len=len("gtk-goto-bottom")+1), parameter :: GTK_STOCK_GOTO_BOTTOM = &
       & "gtk-goto-bottom"//cnull
  character(len=len("gtk-goto-first")+1), parameter :: GTK_STOCK_GOTO_FIRST = &
       & "gtk-goto-first"//cnull
  character(len=len("gtk-goto-last")+1), parameter :: GTK_STOCK_GOTO_LAST = &
       & "gtk-goto-last"//cnull
  character(len=len("gtk-goto-top")+1), parameter :: GTK_STOCK_GOTO_TOP = &
       & "gtk-goto-top"//cnull
  character(len=len("gtk-go-back")+1), parameter :: GTK_STOCK_GO_BACK = &
       & "gtk-go-back"//cnull
  character(len=len("gtk-go-down")+1), parameter :: GTK_STOCK_GO_DOWN = &
       & "gtk-go-down"//cnull
  character(len=len("gtk-go-forward")+1), parameter :: GTK_STOCK_GO_FORWARD = &
       & "gtk-go-forward"//cnull
  character(len=len("gtk-go-up")+1), parameter :: GTK_STOCK_GO_UP = &
       & "gtk-go-up"//cnull
  character(len=len("gtk-harddisk")+1), parameter :: GTK_STOCK_HARDDISK = &
       & "gtk-harddisk"//cnull
  character(len=len("gtk-help")+1), parameter :: GTK_STOCK_HELP = &
       & "gtk-help"//cnull
  character(len=len("gtk-home")+1), parameter :: GTK_STOCK_HOME = &
       & "gtk-home"//cnull
  character(len=len("gtk-index")+1), parameter :: GTK_STOCK_INDEX = &
       & "gtk-index"//cnull
  character(len=len("gtk-indent")+1), parameter :: GTK_STOCK_INDENT = &
       & "gtk-indent"//cnull
  character(len=len("gtk-info")+1), parameter :: GTK_STOCK_INFO = &
       & "gtk-info"//cnull
  character(len=len("gtk-italic")+1), parameter :: GTK_STOCK_ITALIC = &
       & "gtk-italic"//cnull
  character(len=len("gtk-jump-to")+1), parameter :: GTK_STOCK_JUMP_TO = &
       & "gtk-jump-to"//cnull
  character(len=len("gtk-justify-center")+1), parameter :: GTK_STOCK_JUSTIFY_CENTER = &
       & "gtk-justify-center"//cnull
  character(len=len("gtk-justify-fill")+1), parameter :: GTK_STOCK_JUSTIFY_FILL = &
       & "gtk-justify-fill"//cnull
  character(len=len("gtk-justify-left")+1), parameter :: GTK_STOCK_JUSTIFY_LEFT = &
       & "gtk-justify-left"//cnull
  character(len=len("gtk-justify-right")+1), parameter :: GTK_STOCK_JUSTIFY_RIGHT = &
       & "gtk-justify-right"//cnull
  character(len=len("gtk-leave-fullscreen")+1), parameter :: GTK_STOCK_LEAVE_FULLSCREEN= &
       & "gtk-leave-fullscreen"//cnull
  character(len=len("gtk-missing-image")+1), parameter :: GTK_STOCK_MISSING_IMAGE = &
       & "gtk-missing-image"//cnull
  character(len=len("gtk-media-forward")+1), parameter :: GTK_STOCK_MEDIA_FORWARD = &
       & "gtk-media-forward"//cnull
  character(len=len("gtk-media-next")+1), parameter :: GTK_STOCK_MEDIA_NEXT = &
       & "gtk-media-next"//cnull
  character(len=len("gtk-media-pause")+1), parameter :: GTK_STOCK_MEDIA_PAUSE = &
       & "gtk-media-pause"//cnull
  character(len=len("gtk-media-play")+1), parameter :: GTK_STOCK_MEDIA_PLAY = &
       & "gtk-media-play"//cnull
  character(len=len("gtk-media-previous")+1), parameter :: GTK_STOCK_MEDIA_PREVIOUS = &
       & "gtk-media-previous"//cnull
  character(len=len("gtk-media-record")+1), parameter :: GTK_STOCK_MEDIA_RECORD = &
       & "gtk-media-record"//cnull
  character(len=len("gtk-media-rewind")+1), parameter :: GTK_STOCK_MEDIA_REWIND = &
       & "gtk-media-rewind"//cnull
  character(len=len("gtk-media-stop")+1), parameter :: GTK_STOCK_MEDIA_STOP = &
       & "gtk-media-stop"//cnull
  character(len=len("gtk-network")+1), parameter :: GTK_STOCK_NETWORK = &
       & "gtk-network"//cnull
  character(len=len("gtk-new")+1), parameter :: GTK_STOCK_NEW = &
       & "gtk-new"//cnull
  character(len=len("gtk-no")+1), parameter :: GTK_STOCK_NO = &
       & "gtk-no"//cnull
  character(len=len("gtk-ok")+1), parameter :: GTK_STOCK_OK = &
       & "gtk-ok"//cnull
  character(len=len("gtk-open")+1), parameter :: GTK_STOCK_OPEN = &
       & "gtk-open"//cnull
  character(len=len("gtk-orientation-portrait")+1), parameter :: GTK_STOCK_ORIENTATION_PORTRAIT= &
       & "gtk-orientation-portrait"//cnull
  character(len=len("gtk-orientation-landscape")+1), parameter :: GTK_STOCK_ORIENTATION_LANDSCAPE= &
       & "gtk-orientation-landscape"//cnull
  character(len=len("gtk-orientation-reverse-landscape")+1), parameter :: GTK_STOCK_ORIENTATION_REVERSE_LANDSCAPE= &
       & "gtk-orientation-reverse-landscape"//cnull
  character(len=len("gtk-orientation-reverse-portrait")+1), parameter :: GTK_STOCK_ORIENTATION_REVERSE_PORTRAIT= &
       & "gtk-orientation-reverse-portrait"//cnull
  character(len=len("gtk-page-setup")+1), parameter :: GTK_STOCK_PAGE_SETUP = &
       & "gtk-page-setup"//cnull
  character(len=len("gtk-paste")+1), parameter :: GTK_STOCK_PASTE = &
       & "gtk-paste"//cnull
  character(len=len("gtk-preferences")+1), parameter :: GTK_STOCK_PREFERENCES = &
       & "gtk-preferences"//cnull
  character(len=len("gtk-print")+1), parameter :: GTK_STOCK_PRINT = &
       & "gtk-print"//cnull
  character(len=len("gtk-print-error")+1), parameter :: GTK_STOCK_PRINT_ERROR = &
       & "gtk-print-error"//cnull
  character(len=len("gtk-print-paused")+1), parameter :: GTK_STOCK_PRINT_PAUSED = &
       & "gtk-print-paused"//cnull
  character(len=len("gtk-print-preview")+1), parameter :: GTK_STOCK_PRINT_PREVIEW = &
       & "gtk-print-preview"//cnull
  character(len=len("gtk-print-report")+1), parameter :: GTK_STOCK_PRINT_REPORT = &
       & "gtk-print-report"//cnull
  character(len=len("gtk-print-warning")+1), parameter :: GTK_STOCK_PRINT_WARNING = &
       & "gtk-print-warning"//cnull
  character(len=len("gtk-properties")+1), parameter :: GTK_STOCK_PROPERTIES = &
       & "gtk-properties"//cnull
  character(len=len("gtk-quit")+1), parameter :: GTK_STOCK_QUIT = &
       & "gtk-quit"//cnull
  character(len=len("gtk-redo")+1), parameter :: GTK_STOCK_REDO = &
       & "gtk-redo"//cnull
  character(len=len("gtk-refresh")+1), parameter :: GTK_STOCK_REFRESH = &
       & "gtk-refresh"//cnull
  character(len=len("gtk-remove")+1), parameter :: GTK_STOCK_REMOVE = &
       & "gtk-remove"//cnull
  character(len=len("gtk-revert-to-saved")+1), parameter :: GTK_STOCK_REVERT_TO_SAVED = &
       & "gtk-revert-to-saved"//cnull
  character(len=len("gtk-save")+1), parameter :: GTK_STOCK_SAVE = &
       & "gtk-save"//cnull
  character(len=len("gtk-save-as")+1), parameter :: GTK_STOCK_SAVE_AS = &
       & "gtk-save-as"//cnull
  character(len=len("gtk-select-all")+1), parameter :: GTK_STOCK_SELECT_ALL = &
       & "gtk-select-all"//cnull
  character(len=len("gtk-select-color")+1), parameter :: GTK_STOCK_SELECT_COLOR = &
       & "gtk-select-color"//cnull
  character(len=len("gtk-select-font")+1), parameter :: GTK_STOCK_SELECT_FONT = &
       & "gtk-select-font"//cnull
  character(len=len("gtk-sort-ascending")+1), parameter :: GTK_STOCK_SORT_ASCENDING = &
       & "gtk-sort-ascending"//cnull
  character(len=len("gtk-sort-descending")+1), parameter :: GTK_STOCK_SORT_DESCENDING = &
       & "gtk-sort-descending"//cnull
  character(len=len("gtk-spell-check")+1), parameter :: GTK_STOCK_SPELL_CHECK = &
       & "gtk-spell-check"//cnull
  character(len=len("gtk-stop")+1), parameter :: GTK_STOCK_STOP = &
       & "gtk-stop"//cnull
  character(len=len("gtk-strikethrough")+1), parameter :: GTK_STOCK_STRIKETHROUGH = &
       & "gtk-strikethrough"//cnull
  character(len=len("gtk-undelete")+1), parameter :: GTK_STOCK_UNDELETE = &
       & "gtk-undelete"//cnull
  character(len=len("gtk-underline")+1), parameter :: GTK_STOCK_UNDERLINE = &
       & "gtk-underline"//cnull
  character(len=len("gtk-undo")+1), parameter :: GTK_STOCK_UNDO = &
       & "gtk-undo"//cnull
  character(len=len("gtk-unindent")+1), parameter :: GTK_STOCK_UNINDENT = &
       & "gtk-unindent"//cnull
  character(len=len("gtk-yes")+1), parameter :: GTK_STOCK_YES = &
       & "gtk-yes"//cnull
  character(len=len("gtk-zoom-100")+1), parameter :: GTK_STOCK_ZOOM_100 = &
       & "gtk-zoom-100"//cnull
  character(len=len("gtk-zoom-fit")+1), parameter :: GTK_STOCK_ZOOM_FIT = &
       & "gtk-zoom-fit"//cnull
  character(len=len("gtk-zoom-in")+1), parameter :: GTK_STOCK_ZOOM_IN = &
       & "gtk-zoom-in"//cnull
  character(len=len("gtk-zoom-out")+1), parameter :: GTK_STOCK_ZOOM_OUT = &
       & "gtk-zoom-out"//cnull

contains
  ! These 2 clear_ routines are only needed of you need to re-initialize
  ! the types. The definitions include the intial setting to zero or NULL.
subroutine clear_gtktreeiter(iter)
type(gtktreeiter), intent(inout) :: iter

iter%intv=0
iter%p0=NULL
iter%p1=NULL
iter%p2=NULL
end subroutine clear_gtktreeiter
subroutine clear_gvalue(gval)
type(gvalue) :: gval
gval%il=0
gval%i64=(/0,0/)
end subroutine clear_gvalue
end module gtk_sup
