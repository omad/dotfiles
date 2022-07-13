#!/usr/bin/env fish


set current_sink (pactl get-default-sink)

set new_sink (pactl list short sinks | grep -v $current_sink | awk '{print $2}')

pactl set-default-sink $new_sink

set device_name (pactl -f json list sinks | jq ' .[] | select(.name == "'$new_sink'") | .properties."device.description"')
notify-send -h int:transient:1 "Switched Audio Output" "Now Active: $device_name"
