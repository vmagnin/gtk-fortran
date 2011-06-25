*** scstubs.c~	2010-10-03 07:11:57.000000000 -0600
--- scstubs.c	2011-06-23 16:58:54.000000000 -0600
***************
*** 985,990 ****
--- 985,996 ----
  }
  
  void
+ PLSMEMA( PLINT *maxx, PLINT *maxy, void *plotmem )
+ {
+     c_plsmema( *maxx, *maxy, plotmem );
+ }
+ 
+ void
  PLSMIN( PLFLT *def, PLFLT *scale )
  {
      c_plsmin( *def, *scale );
*** plstubs.h~	2010-10-03 07:11:57.000000000 -0600
--- plstubs.h	2011-06-23 16:56:45.000000000 -0600
***************
*** 327,332 ****
--- 327,333 ----
  #define    PLSLABELFUNC_NONEa        FNAME( PLSLABELFUNC_NONE_, plslabelfunc_none_ )
  #define    PLSMAJ                    FNAME( PLSMAJ, plsmaj )
  #define    PLSMEM                    FNAME( PLSMEM, plsmem )
+ #define    PLSMEMA                   FNAME( PLSMEMA, plsmema )
  #define    PLSMIN                    FNAME( PLSMIN, plsmin )
  #define    PLSORI                    FNAME( PLSORI, plsori )
  #define    PLSPAGE                   FNAME( PLSPAGE, plspage )
