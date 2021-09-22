#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Check for a number of outputs
outputs=$(polybar --list-monitors | cut -d":" -f1)
outputs_length=$(wc -w <<< "$outputs")

# Launch the bar
if [ "$outputs_length" == 1 ]; then
    MONITOR=$m polybar -q main &
else
    for m in $outputs; do
        MONITOR=$m polybar -q main &
        MONITOR=$m polybar -q aux &
    done
fi

