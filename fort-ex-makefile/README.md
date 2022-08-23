Simple Fortran makefile example

constructed upon the numerical recipe modules
using the vegas multi-dimensional integration as example
to compute the volume of an n-dimension hyper sphere with radius R

The makefile takes into account the source files (modules or procedures)
in both the nr/ folder and the src/ folder.

The dependencies are automatically generated via makedepf90

.mod files are output to the mod/ folder
.o files are output to the obj/ folder

the main.f90 program file remain in the root directory for overview

Chen Lin
Aug 2022
