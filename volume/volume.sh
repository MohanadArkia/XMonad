#!/bin/sh

send_notification() {
	volume=$(pamixer --get-volume)
	dunstify -a "changevolume" -u low -r 9999 -h int:value:"$volume" -i "volume-$1" "Volume: ${volume}%" -t 2000
}

case $1 in
up)
	pamixer -u
	pamixer -i 5 --allow-boost
	send_notification "$1"
	;;
down)
	pamixer -u
	pamixer -d 5 --allow-boost
	send_notification "$1"
	;;
mute)
	pamixer -t
	if eval "$(pamixer --get-mute)"; then
		dunstify -a "changevolume" -t 2000 -r 9999 -u low -i "volume-mute" "Muted"
	else
		send_notification up
	fi
	;;
esac
