## Fortran Examples

Here are some neat examples that is written in the Fortran language

---

### makefile

Most of the time a Fortran program is written in many separate modules, procedures and files, with complicated
dependency structure.
This example shows that we can be organize the program into different directories, using a well designed `makefile`, with
dependencies generated via `makedepf90`.

- I'm using the gfortran compiler, with is freely available.
- I'm also using the Numerical Recipe f90 codes as base for this example.
- the modules `.mod` files will be output to the `mod/` folder, and the object `.o` files will be output to the `obj/` folder.
- in the example program, I'm computing the volume of a hyper-sphere, with dimension and radius given.
- the dependencies are generated using `makedepf90`

*Aug 2022*

### nr

This is just a test program, showing that we can include almost all Numerical-Recipe codes in the `nr/` folder (without producing
error).
A check for the Fortran compiler flags used in the makefile.
- note that not all NR fortran 90 codes are included in the `nr/` folder
- some of the NR codes have extra dependencies, I have excluded them
- some fo the NR codes are not numerical algorithms, but some system validation codes, I have excluded as well.
- the example used is similar to the 'makefile example'.

*Jul 2022*

### vegas

This is a minimum work example using the Numerical-Recipe vegas subroutine
- all vegas dependent codes in NR have been included in the `vegas.f` file.
- the multidimensional integration calculates the volume of a 3D sphere with given radius.
- the `makefile` is also minimized (but canbe modified for general use.

*Mar 2022*

### helper

This example include the usage and design of an object oriented class structure in Fortran
The example is a few helper classes (modules) that will assist in a future program that I'm designing
- the `fourmom` module creates a data type that work with Lorentz 4-momentums
- the `histogram` module allows users to work with histogram book-keeping
The above help classes are all useful in cross-section calculations of high energy particle scatterings

*Aug 2022*
