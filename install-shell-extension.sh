#!/bin/sh
# Simple script to manually install a gnome shell extension, since the browser
# extension doesn't seem to work on OpenBSD.
for var in "$@"
do
	EXTID=$(unzip -c $var metadata.json | grep uuid | cut -d \" -f4)
	echo "Extension UUID= $EXTID"
	INSTDIR="/home/$USER/.local/share/gnome-shell/extensions/$EXTID"
	echo "Install Directory set to $INSTDIR"
	mkdir -p $INSTDIR
	unzip $var -d $INSTDIR
done
