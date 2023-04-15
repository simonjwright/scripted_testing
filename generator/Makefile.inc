# -*- Makefile -*-

# Copyright (C) 2023 Simon Wright <simon@pushface.org>
# SPDX-License-Identifier: GPL-3.0-or-later

# Generates stub bodies from specs.

# Work out the location of this Makefile, without the trailing /
location := $(shell echo $(dir $(abspath $(lastword $(MAKEFILE_LIST)))) \
        | sed -e "s;/$$;;")

# Generate XML from the spec.
%.xml: %.ads
	libadalang2xml $< >$@

# Generate stub code, no attention paid to formatting, all in one file.
%.adb-raw: %.xml $(location)/generator.xsl
	xsltproc $(location)/generator.xsl $< >$@

# Format the stub code (still all in one file).
%.adb-form: %.adb-raw
	gnatpp					\
	  --no-separate-return			\
	  --par-threshold=1			\
	  --output-force=$@			\
	  $<

# Chop the stub code into individual files.
%.adb: %.adb-form
	gnatchop -w $<
