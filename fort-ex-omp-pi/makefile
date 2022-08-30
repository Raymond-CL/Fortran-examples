target = omp_pi.exe

fc = gfortran
flag = -ffree-form -fopenmp

prog = main.f90

$(target): $(prog)
	$(fc) $(flag) $^ -o $(target)
	rm -rf *.o

default: $(target)

run: $(target)
	./$(target)

clean:
	rm -rf *.o $(target)	
