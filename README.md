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

Jul 2022
