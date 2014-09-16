# Copyright Simon Wright <simon@pushface.org>

# This file is part of the utility package Scripted_Testing, hosted at
# Sourceforge by Simon Wright.

# Scripted_Testing is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3, or (at
# your option) any later version.  It is distributed in the hope that
# it will be useful, but WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.

# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING3.  If not, see
# <http://www.gnu.org/licenses/>.

# This file contains targets to install a relocatable version of the
# library, but they are disabled at present (27.vii.14) because the
# GNAT Project file (scripted_testing.gpr) hasn't been updated to
# support building a relocatable version.

all::
.PHONY: all

GPRBUILD ?= gprbuild

all:: lib-static-stamp

lib-static-stamp: scripted_testing.gpr
	$(GPRBUILD) -p -P scripted_testing.gpr -XLIBRARY_TYPE=static
	touch $@

lib-relocatable-stamp: scripted_testing.gpr
	$(GPRBUILD) -p -P scripted_testing.gpr -XLIBRARY_TYPE=relocatable
	touch $@

# Installation

prefix ?= $(realpath $(dir $(shell which gnatls))/..)

install: install-static-lib
.PHONY: install

install-static-lib: lib-static-stamp
	gprinstall				\
	  -P scripted_testing.gpr		\
	  --prefix=$(prefix)			\
	  --mode=dev				\
	  --project-subdir=lib/gnat		\
	  --build-var=LIBRARY_TYPE		\
	  --build-name=static			\
	  -XLIBRARY_TYPE=static			\
	  -f					\
	  -p
.PHONY: install-static-lib

install-relocatable-lib: lib-relocatable-stamp
	gprinstall				\
	  -P scripted_testing.gpr		\
	  --prefix=$(prefix)			\
	  --mode=dev				\
	  --project-subdir=lib/gnat		\
	  --build-var=LIBRARY_TYPE		\
	  --build-name=relocatable		\
	  -XLIBRARY_TYPE=relocatable		\
	  -f					\
	  -p
.PHONY: install-relocatable-lib

# Clean

clean:
	-gnatclean -P scripted_testing.gpr -XLIBRARY_TYPE=static
	#-gnatclean -P scripted_testing.gpr -XLIBRARY_TYPE=relocatable
	rm -f *-stamp
.PHONY: clean

# Docs

DOCS = doc/index.html doc/sf.css
RSYNC ?= rsync
SFUSER ?= simonjwright

upload-docs: $(DOCS) force
	$(RSYNC)							\
	  --compress							\
	  --copy-unsafe-links						\
	  --cvs-exclude							\
	  --perms							\
	  --recursive							\
	  --rsh=ssh							\
	  --times							\
	  --update							\
	  --verbose							\
	  $(DOCS)							\
	  $(SFUSER),coldframe@web.sourceforge.net:htdocs/scripted_testing

.PHONY: force

############################
# Distribution construction

# Create the current date, in the form yyyymmdd.
SUBRELEASE = hg
DATE = $(shell date +%Y%m%d)$(SUBRELEASE)

DOCS =						\
COPYING3					\
doc/sf.css					\
doc/index.html

SRC =						\
src/scripted_testing.adb			\
src/scripted_testing.ads

TEST =						\
test/test.gpr					\
test/test.tcl					\
test/test.ads					\
test/test.adb					\
test/test-except.adb				\
test/test-first.adb				\
test/test-lists.adb				\
test/test-main.adb

BUILDING =					\
Makefile					\
scripted_testing.gpr

DISTRIBUTION_FILES =				\
scripted_testing-$(DATE).tgz			\
scripted_testing-$(DATE).zip

scripted_testing-$(DATE).tgz: scripted_testing-$(DATE)
	-rm $@
	tar zcvf $@ $</

scripted_testing-$(DATE).zip: scripted_testing-$(DATE)
	-rm $@
	zip -r $@ $</*

scripted_testing-$(DATE): $(DOCS) $(SRC) $(TEST) $(BUILDING)
	-rm -rf $@
	mkdir $@ $@/docs $@/src $@/test
	cp -p $(DOCS) $@/docs/
	cp -p $(SRC) $@/src/
	cp -p $(TEST) $@/test/
	cp -p $(BUILDING) $@

dist:: $(DISTRIBUTION_FILES)
