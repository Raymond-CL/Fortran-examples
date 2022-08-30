target = omp_pi.exe

fc = gfortran
flag = 	-fopenmp

prog = main.f90

$(target): $(prog)
	$(fc) $(flag) -c $^
	$(fc) *.o -o $(target)
	rm -rf *.o *.mod

default: $(target)

run: $(target)
	./$(target)

clean:
	rm -rf *.o *.mod $(target)	
