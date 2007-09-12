#-*- Makefile -*-
.SUFFIXES: .G .L .N .gon .line .name

PKGNAME = maps
CP = cp
ECHO = echo
RM = rm

CFLAGS = -g -Wall -pedantic -O6

OBJS = mapget.o		\
	smooth.o	\
	thin.o

GDATA = county.G state.G usa.G nz.G world.G world2.G italy.G france.G
LDATA = county.L state.L usa.L nz.L world.L world2.L italy.L france.L
NDATA = county.N state.N usa.N nz.N world.N world2.N italy.N france.N

.line.L:
	./Lmake 0 s b ${*}.line ${*}.linestats ../inst/mapdata/${*}.L

.gon.G:
	./Gmake b ${*}.gon ${*}.gonstats ../inst/mapdata/${*}.G ../inst/mapdata/${*}.L

.name.N:
	@$(CP) ${*}.name ../inst/mapdata/${*}.N

.PHONY: all gdata ldata ndata

all: gdata ndata $(PKGNAME)$(SHLIB_EXT)

gdata: Gmake ldata
	$(MAKE) $(GDATA)

ldata: Lmake world2.line
	$(MAKE) $(LDATA)

ndata: world2.line
	$(MAKE) $(NDATA)

$(PKGNAME)$(SHLIB_EXT): $(OBJS)
	$(R_HOME)/bin/R CMD SHLIB -o $(PKGNAME)$(SHLIB_EXT) $(OBJS)

world2.line: world.line
	@$(ECHO) "Converting world to world2"
	$(AWK) -f convert.awk < world.line > world2.line
	@$(CP) world.linestats world2.linestats
	@$(CP) world.gon world2.gon
	@$(CP) world.gonstats world2.gonstats
	@$(CP) world.name world2.name

clean:
	$(RM) -f $(OBJS) Gmake Lmake ../inst/mapdata/* world2.* $(PKGNAME).s[lo] $(PKGNAME).dylib
