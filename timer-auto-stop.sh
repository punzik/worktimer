#!/bin/bash

timer_prog="timer"
xset_prog="xset"

timer_is_start()
{
    if [ "$(timer current)" = "NO TASKS" ]; then
        echo "off"
    else
        echo "on"
    fi
}

screen_status()
{
    local xset_status=$($xset_prog q | grep "Monitor is On")

    if [ -z "$xset_status" ]; then
        echo "off"
    else
        echo "on"
    fi
}

timer_on=$(timer_is_start)
scr_status=$(screen_status)

while true
do
    scr=$(screen_status)

    if [ $scr != $scr_status ]; then
        case $scr in
            on)
                if [ $timer_on = "on" ]; then
                    $timer_prog start
                fi
            ;;
            off)
                timer_on=$(timer_is_start)
                if [ $timer_on = "on" ]; then
                    $timer_prog stop
                fi
            ;;
        esac
    fi

    scr_status=$scr

    sleep 5
done
