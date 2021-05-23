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

include ../Makefile.paths

all:: test-main

test-main: force
	gprbuild test.gpr

clean:
	gprclean test.gpr
	-rm ivp

.PHONY: all clean force
