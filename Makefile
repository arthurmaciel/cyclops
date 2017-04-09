# Cyclops - package installer for Cyclone Scheme
# Copyright (c) 2017, Justin Ethier
# All rights reserved.
#
# Configuration options for the makefile
COMP_CFLAGS ?= -O2 -Wall -I$(PREFIX)/include -L$(PREFIX)/lib

CC      ?= cc
CYCLONE ?= cyclone
LIBS 	?= -pthread -lcyclone -lck -lm -ltommath -ldl -lcurl

RM        ?= rm -f
INSTALL   ?= install
MKDIR     ?= $(INSTALL) -d
RMDIR     ?= rmdir

PREFIX    ?= /usr/local
BINDIR    ?= $(PREFIX)/bin
DATADIR   ?= $(PREFIX)/share/cyclone

DESTDIR   ?=

cyclops: cyclops.scm download.o util.o
# This was generated via 'cyclone -d cyclops.scm' and -lcurl was added manually
	$(CYCLONE) -CE "$(CC) cyclops.o $(DATADIR)/scheme/cyclone/common.o $(DATADIR)/scheme/base.o $(DATADIR)/scheme/char.o $(DATADIR)/scheme/cyclone/util.o $(DATADIR)/scheme/process-context.o $(DATADIR)/scheme/write.o $(DATADIR)/srfi/2.o $(DATADIR)/scheme/file.o $(DATADIR)/scheme/read.o util.o download.o $(LIBS) $(COMP_CFLAGS) -o cyclops" cyclops.scm

util.o: util.sld
	$(CYCLONE) util.sld

download.o: download.sld download-header.h
	$(CYCLONE) download.sld

install: cyclops
	$(MKDIR) $(DESTDIR)$(BINDIR)
	$(INSTALL) -m0755 cyclops $(DESTDIR)$(BINDIR)/

uninstall:
	$(RM) $(DESTDIR)$(BINDIR)/cyclops

.PHONY: clean

clean:
	$(RM) cyclops *.c *.o *.so *.meta
