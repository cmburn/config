#!/bin/sh
#
# Copyright (c) 2020 Charlie Burnett
#
# Permission to use, copy, modify, and/or distribute this software for any 
# purpose with or without fee is hereby granted, provided that the above 
# copyright notice and this permission notice appear in all copies.
# 
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH 
# REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY 
# AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, 
# INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM 
# LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
# OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
# PERFORMANCE OF THIS SOFTWARE.
# 
# Just a personal script to build a list of packages from ports, if package is 
# not installed yet.

PKG_LIST=$(pkg_info -zm)

for var in "$@"
do
	if [ $(echo $PKG_LIST | grep -ic $var) = 0 ]
	then
		echo "Installing $var...\n"
		cd $var
		make 
		make install 
		cd ../
		$PKG_LIST += "\n$var"
	else
		echo "Package $var already installed, skipping...\n"
	fi
done
