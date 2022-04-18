# gtk-fortran & PLplot

In this directory there are a few examples of using the [PLplot library](http://plplot.sourceforge.net/) in conjunction with gtk-fortran. PLplot is a scientific graphics library that has a modern Fortran binding.

## Requirements

- Either CMake and pkg-config, or fpm (Fortran Package Manager).
- gtk-fortran: including for these examples the high-level modules gtk\_hl and gtk\_draw\_hl.
- PLplot>=5.13: including the cairo drivers.

A wrapper module `plplot_extra.mod` is created by the gtk-fortran system to allow access to the `pl_cmd()` routine in PLplot which is not in its Fortran binding. It provides low-level access to the system and is needed to correctly configure the "extcairo" driver.

## Concept

The example codes here use the "extcairo" driver in PLplot to write to the backing surface of a GTK drawable created by `hl_gtk_drawing_area_new`.

The typical program structure is summarized as:

### Main:

- Create the gtk widgets, including a drawing area.
- Realize the widget heirarchy.
- Call the PLplot drawing routine(s).
- Enter the event loop.

### Handlers:

- Handle events and if needed, call the PLplot drawing routine(s) with updated settings.

### PLplot drawing:

- Connect plplot's output to the backing surface (see below).
- Make the plot(s).
- Call `gtk_widget_queue_draw` on the drawing area to force a redraw.

### Globals:

- For convenience in all examples I've put all the `use` statements and any global variables into a separate module that can be used by all of the other units.

###  In the example codes these are:

* The main program (called `cairo_plplot_ex<n>`).
* The common module (`common_ex<n>`).
* The drawing module (`plplot_code_ex<n>`).
* The handlers (`handlers_ex<n>`).


## Connecting PLplot's output to the drawing area

For the "extcairo" driver, output is to an externally-created cairo context. So here you need to create a cairo context connected to the backing surface (this is most easily done with
`hl_gtk_drawing_area_cairo_new`). And then use `pl_cmd` to connect PLplot's output to the context.  To do this the following code needs to precede the call to `plinit` (or `plstar`):

    ! Get a cairo context from the drawing area.
    cc = hl_gtk_drawing_area_cairo_new(area)

    !  Initialize plplot
    call plsdev("extcairo")

    ! By default the "extcairo" driver does not reset the background
    ! This is equivalent to the command line option "-drvopt set_background=1"
    plsetopt_rc = plsetopt("drvopt", "set_background=1")
    if (plsetopt_rc /= 0) stop "plsetopt error"

    ! The "extcairo" device doesn't read the size from the context.
    write(geometry, "(I0,'x',I0)") width, height
    plsetopt_rc = plsetopt( 'geometry', geometry)
    if (plsetopt_rc /= 0) stop "plsetopt error"

And then after `plinit` you need:

    call pl_cmd(PLESC_DEVINIT, cc)

A fortran interface to `pl_cmd` is provided by the `plplot_extra` module.


## Building the examples

If PLplot is found when building gtk-fortran, then the interface module for `pl_cmd` will be built and installed and the examples will be built by CMake. The instructions that follow are therefore only needed if you want to build them manually for some reason.

The PLplot example(s) are most easily built if gtk-fortran has been installed. Then it is simply a matter of:

    gfortran -o hl_plplot<x>e hl_plplot<x>e.f90 $(pkg-config --cflags --libs \
             gtk-<n>-fortran  plplot-fortran plplot)

where `<x>` is the number of the example, and `<n>` is the GTK major version you are using.

The examples are derived from the Fortran versions of examples 1, 4, 8, 30 and 17 on the PLplot web site:

- hl_plplot1e: Basic x-y plots on multiple pages.
- hl_plplot4e: Log plots, using 2 drawing areas.
- hl_plplot8e: 3-D data display.
- hl_plplot17e: Strip charts, shows continuous updating.
- hl_plplot30e: Transparency.

## Known issues

Different releases of GFortran, let alone different fortran compilers, may not read each other's module files, therefore PLplot must be built with the same compiler as gtk-fortran.

If you wish to explicitly exclude building and installing PLplot support, then when running cmake, include the option `-D
EXCLUDE_PLPLOT=true` (e.g.: `cmake -D EXCLUDE_PLPLOT=true ..`)

## When using gtk-fortran as a fpm dependency

Put in the `fpm.toml` manifest of your project:

```toml
[dependencies]
gtk-fortran = { git = "https://github.com/vmagnin/gtk-fortran.git", branch = "gtk4" }

[build]
link = ["plplot", "plplotfortran"]
external-modules = ["plplot"]
```

and build and run your project with:

```bash
$  fpm run --flag '$(pkg-config --cflags --libs plplot plplot-fortran)'
```
