#-*- Makefile -*-
.SUFFIXES: .G .L .N .gon .line .n

PKGNAME = maps
CP = cp
RM = rm
ECHO = echo

OBJS = mapget.o		\
	smooth.o	\
	thin.o

GDATA = county.G state.G usa.G nz.G world.G world2.G italy.G france.G
LDATA = county.L state.L usa.L nz.L world.L world2.L italy.L france.L

all: $(PKGNAME).so Gmake Lmake $(LDATA) $(GDATA)

.line.L:
	./Lmake 0 s b ${*}.line ${*}.linestats ../inst/mapdata/${*}.L

.gon.G:
	./Gmake b ${*}.gon ${*}.gonstats ../inst/mapdata/${*}.G ../inst/mapdata/${*}.L
	@$(CP) ${*}.n ../inst/mapdata/${*}.N	# needed here for Mac

$(PKGNAME).so: $(OBJS)
	$(R_HOME)/bin/R CMD SHLIB -o $(PKGNAME).so $(OBJS)

world2.line: world.line
	@$(ECHO) "Converting world to world2"
	$(AWK) -f convert.awk < world.line > world2.line
	@$(CP) world.linestats world2.linestats

world2.gon: world.gon world.n
	@$(CP) world.gon world2.gon
	@$(CP) world.gonstats world2.gonstats
	@$(CP) world.n world2.n

clean:
	$(RM) -f $(PKGNAME).so $(OBJS) Gmake Lmake ../inst/mapdata/* world2.*
