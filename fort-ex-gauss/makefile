target = gauss.exe

fc = gfortran

prog = gauss.f90 main.f90

$(target): $(prog)
	@$(fc) -c $^
	@$(fc) *.o -o $(target)
	@rm -rf *.o *.mod

default: $(target)

run: $(target)
	@./$(target)

clean:
	@rm -rf *.o *.mod $(target)	
