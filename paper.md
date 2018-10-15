---
title: 'gtk-fortran: a GTK+ binding to build Graphical User Interfaces in Fortran'
tags:
- GTK+
- Fortran
- Graphical User Interface
authors:
- name: Vincent MAGNIN
  orcid: 0000-0002-9016-9955
  affiliation: 1
- name: James TAPPIN
  orcid: 0000-0002-1878-5243
  affiliation: 2
- name: Jens HUNGER
  orcid: 0000-0001-7639-1229
  affiliation: 3
- name: Jerry DE LISLE
  affiliation: 4
affiliations:
- name: Univ. Lille, CNRS, Centrale Lille, ISEN, Univ. Valenciennes, UMR 8520 - IEMN, F-59000 Lille, France
  index: 1
- name: RAL Space, STFC Rutherford Appleton Laboratory, Harwell Campus​, Didcot,Oxfordshire OX11 0QX, United Kingdom
  index: 2
- name:  Technische Universität Dresden, Department of Chemistry and Food Chemistry, Dresden, Germany
  index: 3
- name: GFortran Team, USA
  index: 4
date: 11 October 2018
bibliography: paper.bib
---

# Summary

A Graphical User Interface (GUI) can be useful in a research software to visualize results in real time during a computation, to plot results or to interact with the user, especially if the software is used by other people or distributed outside the laboratory. But despite the venerable age of Fortran, few solutions exist in this language [@markus2012; @metcalf2011] and are often limited in their functionalities or are not open source, or not cross-platform, etc. For example, you can plot results with PLplot but not create a GUI [@plplot],  DISLIN has a restrictive licence [@dislin], Intel Visual Fortran offers some interesting features for GUI and graphics but is limited to the Microsoft Windows operating system and is commercial software [@visual_fortran]...

Gtk-fortran [@gtk_fortran] is a GTK+ / Fortran binding using the ISO\_C\_BINDING intrinsic module for interoperability between C and Fortran defined since the Fortran 2003 standard [@markus2012; @metcalf2011; @fortran]. Although gtk-fortran development is mainly focused on Linux, it can also be built under Windows, Mac OS X... It is licensed under the GNU General Public License version 3. It offers interfaces to around 10000 GTK+ and GLib functions [@gtk], and also to the PLplot library.  A higher-level interface (at a level comparable to the widget routines available in IDL [@idl]) has been developed to offer an easier access to the main GTK+ widgets. Moreover, even though gtk-fortran was mainly developed to visualise computing results and build GUIs, Fortran developers can also access the large set of utility functions offered by the GLib library. Along the wiki documentation of the project, the user can learn to use gtk-fortran thanks to tens of various example programs available in the repository.

![A Fortran program using gtk-fortran to visualize Julia Sets.](paper.png)

Among the publicly available research softwares using gtk-fortran, we can cite for example UncertRadio [@Kanisch2016], designed for a generalized evaluation of environmental radioactivity measurements, and the gene network editor NetworkMaker [@NetworkMaker] software. Coupling a modelling code with gtk-fortran can also be useful for teaching at the college level, for instance to simulate interactively the behaviour of a nuclear power plant [@Tincher2017].

The GTK+ and GLib libraries being in constant development, gtk-fortran is maintained to stay in tune with their evolutions. New projects should use GTK+ 3 but the GTK+ 2 version is still maintained, at least until the release of the future GTK+ 4.

# Acknowledgements

We would like to thank all those who have contributed in any way to the gtk-fortran project.

# References

