target = buffon.exe

fc = gfortran
flag = 	-Winteger-division\
	-fcheck=all\
	-fno-strict-overflow\
	-std=gnu\
	-ffast-math\
	-O3

prog = main.f90
mod = nrtype.f90 nr.f90 nrutil.f90
src = ran_state.f90 ran0.f90 ran1.f90 ran2.f90 

$(target): $(mod) $(src) $(prog)
	$(fc) $(flag) -c $^
	$(fc) *.o -o $(target)
	rm -rf *.o *.mod

default: $(target)

run: $(target)
	./$(target)

clean:
	rm -rf *.o *.mod $(target)	
