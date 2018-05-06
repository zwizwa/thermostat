#!/bin/bash
# set -x
DIR=/sys/class/gpio
IO=48

# pin 15.

# Circuit is not ideal.  First attempt used 3V3 as off.
# PIN      OPTO  LED
# 0Vn      1.1V  1.8V
# 3V3      0.3V  0.8V
# in=2.1V  0.9V  1.9V

# So current is very low in 3V3 but is worse in input.
# Let's keep 3V3

DIRECTION=$DIR/gpio${IO}/direction
VALUE=$DIR/gpio${IO}/value

# off by default
on() {
	echo 'low' >$DIRECTION
}

off() {
	echo 'high' >$DIRECTION
}
init() {
	if echo "$IO" >$DIR/export ; then off ; fi
	chgrp furnace $DIRECTION $VALUE
	chmod g+rw $DIRECTION $VALUE
}
# active low
value() {
	cat $VALUE
}


case $1 in
	on|off|init|value)
		$1
		;;
	*)
		echo "unknown command: $1"
		exit 1
		;;
esac




