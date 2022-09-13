target = color.exe

fc = gfortran

prog = main.f90

$(target): $(prog)
	@$(fc) -c $^
	@$(fc) *.o -o $(target)
	@rm -rf *.o

default: $(target)

run: $(target)
	@./$(target)

clean:
	@rm -rf *.o $(target)	
