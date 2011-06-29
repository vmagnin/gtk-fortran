F90=gfortran
FFLAGS= `pkg-config --cflags gtk-fortran plplotd-f95`
LDFLAGS=`pkg-config --libs gtk-fortran plplotd-f95`

all: hl_plplot1 hl_plplot8 hl_plplot1e hl_plplot8e hl_plplot30e

hl_plplot1: hl_plplot1.f90
	$(F90) -o hl_plplot1 hl_plplot1.f90 $(FFLAGS) $(LDFLAGS)

hl_plplot1e: hl_plplot1e.f90 plplot_extra.mod
	$(F90) -o hl_plplot1e hl_plplot1e.f90 $(FFLAGS) $(LDFLAGS)

hl_plplot8: hl_plplot8.f90
	$(F90) -o hl_plplot8 hl_plplot8.f90 $(FFLAGS) $(LDFLAGS)

hl_plplot8e: hl_plplot8e.f90 plplot_extra.mod
	$(F90) -o hl_plplot8e hl_plplot8e.f90 $(FFLAGS) $(LDFLAGS)

hl_plplot30e: hl_plplot30e.f90 plplot_extra.mod
	$(F90) -o hl_plplot30e hl_plplot30e.f90 $(FFLAGS) $(LDFLAGS)

plplot_extra.mod:  plplot_extra.f90
	$(F90) -c plplot_extra.f90 $(FFLAGS)

clean:
	rm -f *.mod

clean_all:
	rm -f *.mod hl_plplot8 hl_plplot8e hl_plplot1 hl_plplot1e *~
