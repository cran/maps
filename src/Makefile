#-*- Makefile -*-
.SUFFIXES: .G .L .N .gon .line .n

PKGNAME = maps
CP = cp
RM = rm

OBJS = mapget.o		\
	smooth.o	\
	thin.o

GDATA = county.G state.G usa.G nz.G world.G world2.G 
LDATA = county.L state.L usa.L nz.L world.L world2.L 
NDATA = county.N state.N usa.N nz.N world.N world2.N 

all: $(PKGNAME).so Gmake Lmake $(LDATA) $(GDATA) $(NDATA)

.line.L:
	./Lmake 0 s b ${*}.line ${*}.linestats ../inst/mapdata/${*}.L

.gon.G:
	./Gmake b ${*}.gon ${*}.gonstats ../inst/mapdata/${*}.G ../inst/mapdata/${*}.L

.n.N:
	$(CP) ${*}.n ../inst/mapdata/${*}.N

$(PKGNAME).so: $(OBJS)
	$(R_HOME)/bin/R CMD SHLIB -o $(PKGNAME).so $(OBJS)

clean:
	$(RM) $(PKGNAME).so $(OBJS) Gmake Lmake ../inst/mapdata/*
