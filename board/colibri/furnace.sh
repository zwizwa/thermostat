#!/bin/bash

init() {
	cd /sys/class/gpio
	set -x
	mkdir -p /tmp/relay
	echo '89' >export ; (cd /tmp/relay ; ln -s /sys/class/gpio/gpio89 1)
	echo '46' >export ; (cd /tmp/relay ; ln -s /sys/class/gpio/gpio46 2)
	echo '88' >export ; (cd /tmp/relay ; ln -s /sys/class/gpio/gpio88 3)
	echo '48' >export ; (cd /tmp/relay ; ln -s /sys/class/gpio/gpio48 4)

	for relay in 1 2 3 4; do
		echo low >/tmp/relay/$relay/direction
	done
        chown tom:tom /tmp/relay/1/value
}

FURNACE=/tmp/relay/1/value

# Backwards compatible with beaglebone relay board, which is wired active low.
on() {
	echo '1' >$FURNACE
}
off() {
	echo '0' >$FURNACE
}
state() {
	case $(cat $FURNACE) in
		1)
			echo "on"
			;;
		0)
			echo "off"
			;;
	esac
}

case $1 in
	on|off|init|state)
		$1
		;;
	*)
		echo "unknown command: $1"
		;;
esac


# 3 naming schemes:
#
# - Viola Carrier Board
#   Extension Connector (X9)
#   https://docs.toradex.com/102879-colibri-arm-viola-carrier-board-datasheet.pdf
#
# - SODIMM pins are described here:
#   https://docs.toradex.com/101355-colibri-vf61-datasheet.pdf
#
# - GPIO pins as they appear on the X9 Viola connector.
#   PORTx[y] = gpio(32*x+y)
#
# relay X9 Colibri SODIMM     name  gpio
# --------------------------------------
#    1  8 SODIMM_135 (GPIO)  PTD10 gpio89
#    2  9 SODIMM_98  (GPIO)  PTC1  gpio46
#    3 10 SODIMM_133 (GPIO)  PTD9  gpio88
#    4 11 SODIMM_103 (GPIO)  PTC3  gpio48
#
# info on using pins:
# https://developer.toradex.com/getting-started/module-3-hardware-peripherals/basic-gpio-usage-colibri-evaluation-board-colibri-vfxx




