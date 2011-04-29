#
# Makefile to compile and maintain a fortran executable in SunOS
#
# Usage: type in your Sun > make -f cc.make
#
#  created: I.Romero                   20/05/2008
#-------------------------------------------------------------------
#
# Directory Paths for binaries and libraries
#
BIN_HOME = ../cc2noncc/bin
#
# List of Main Programs
#	(Put a list of FORTRAN source files containing main programs
#	 here) 
#
MAINS = cc2noncc.f 
#
#
# Tools Libraries
#	(Specify libraries to be linked in here. (e.g. orbit/attitude libs
#	 MIND the ORDER!)
#
# Use any of the following libraries if needed. Use ONLY the necessary ones!!
LIBS =
#
# Include Directory
#	(Specify directories to pick up include files (MIND the ORDER!)
#
INCL =  ./inc ../../inc
#
# Compiler (f77, g77, f90, etc)
FC = f77
#
# Compilation flags 
#	(Put the compilation flags here (-g is for debugging))
#
FFLAGS = -O3
#
#------------------------------------------------------------------------
#		the rest is not likely to be changed
#------------------------------------------------------------------------
#
BIN_SUFFIX = .bin
#
#
# List of system libraries
# for example:
#LIBFDDB   = -lfortran -lgen -l++ -lF77 -lsunmath -lC
LIBFDDB   = 
#
# List of source files, objects and binaries
#
LS_FORT    = echo `ls *.f | grep -v "^," 2>/dev/null` 
LIST_MOD   = $(LS_FORT:sh)
LIST_OBJ   = $(LIST_MOD:%.f=%.o)
XLS_SH     = for file in $(LIST_OBJ);do X="0";for key in $(MAINS:%.f=%.o);\
do if test $$file = $$key;then X="1";fi;done;\
if test $$X = "0";then echo $$file;fi;done
LIST_SUBS  = $(XLS_SH:sh)
LIST_EXEC  = $(MAINS:%.f=$(BIN_HOME)/%.bin)
#
FFLAGS    += $(INCL:%=-I%) 
#
# Keep make status
#
.KEEP_STATE:
#
# default target (build when executing make w/o
# specifying target - maintains all binaries)
#
all:	$(LIST_SUBS) $(LIST_EXEC)

# link binary (from library) and place in $(BIN_HOME)
#
$(LIST_EXEC):	$$(@:$(BIN_HOME)/%$(BIN_SUFFIX)=%.o) $(LIST_OBJ) $(LIBS)
	@echo "building" $@ 
	@echo "    since" $? "has changed"
	$(LINK.f) $(@:$(BIN_HOME)/%$(BIN_SUFFIX)=%.o) -o $(@:$(BIN_HOME)/%=%) $(LIST_SUBS) $(LIBS) $(LIBFDDB)
	@mv $(@:$(BIN_HOME)/%=%) $(BIN_HOME)
	@touch .libs_status

#
# Build objects from source files
#
$(LIST_OBJ):	$$(@:%.o=%.f)
	@echo "compiling "
	@$(COMPILE.f) $*.f -o $*.o
#
# CHECK
#
check:	check_src check_libs
#
check_libs:	.libs_status
#
.libs_status:	$(DLIBS)
	@echo $? has been changed
#
check_src:	.src_status
#
.src_status:	$(LIST_MOD)
	@echo $? has been changed





