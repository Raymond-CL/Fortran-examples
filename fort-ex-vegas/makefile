target = vegas.exe

fc = gfortran
flag = 	-fno-strict-overflow

prog = main.f
mod = vegas.f

$(target): $(mod) $(src) $(prog)
	$(fc) $(flag) -c $^
	$(fc) *.o -o $(target)
	rm -rf *.o *.mod

default: $(target)

run: $(target)
	./$(target)

clean:
	rm -rf *.o *.mod $(target)	
