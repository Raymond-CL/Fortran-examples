# you should have the latest GNU fortran compiler installed, 
# go to gcc.gnu.org/fortran for documentation
# on a Ubuntu, along with other C compilers, simply:
# sudo apt install gfortran build-essential makedepf90


# please use the latest GNU compiler
FC = gfortran

# name of executable
TARGET = example.exe

# fortran dialect flags
fdial = -ffree-form -fimplicit-none -fno-strict-overflow -finit-local-zero -std=gnu
# fortran warning flags
fwarn = -Wall -Wextra -Wcharacter-truncation -Walign-commons \
	-Wfrontend-loop-interchange -Wimplicit-interface -Wimplicit-procedure -Wunderflow
# fortran warning exceptions
fnowarn = -Wno-aliasing -Wno-conversion -Wno-tabs -Wno-intrinsic-shadow -Wno-target-lifetime \
	-Wno-compare-reals -Wno-unused-parameter -Wno-unused-dummy-argument
# fortran runtime checks
fchek = -fcheck=all -fbacktrace
# optimization flags
foptm = -ffast-math
# flags use for develop (warn and check every possible bug)
fdevelop = $(fdial) -Og $(fwarn) $(fnowarn) $(fchek)
# flags use for release (full optimize for speed)
frelease = $(fdial) -O3 $(foptm)
# use debugging flags while code testing, use release flags to run results
FFLAGS = $(fdevelop)

# shortcuts to directories and files
nrdir = nr
srcdir = src
objdir = obj
moddir = mod
progrm = main.f90
depend = .dep
recipe = '@$$(FC) $$(FFLAGS) -I $$(moddir) $$(FOBJ) -o $$@'

.SUFFIXES:
.SUFFIXES: .f90 .o

# create directories if needed
$(moddir):
	$(shell mkdir -p $(moddir))
$(objdir):
	$(shell mkdir -p $(objdir))

# recipe for every object files
$(objdir)/%.o: | $(moddir) $(objdir)
	$(FC) $(FFLAGS) -J $(moddir) -c $< -o $@

# include dependency file
include $(depend)

# run executable
run: $(TARGET)
	./$(TARGET)

# clean all objects, mods, dependencies and executables
clean:
	@rm -f $(objdir)/*.o $(moddir)/*.mod $(depend) $(TARGET)

# recipe to generate dependency file
depend $(depend):
	@makedepf90 $(nrdir)/*.f90 $(srcdir)/*.f90 $(progrm) \
	-u intrinsic -l $(recipe) \
	-W -b $(objdir) -o $(TARGET) > $(depend)
