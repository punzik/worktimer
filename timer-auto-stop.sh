#!/bin/bash

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
    local xset_status=$(xset q | grep "Monitor is")

    if [ "$xset_status" = "  Monitor is On" ]; then
        echo "on"
    elif [ "$xset_status" = "  Monitor is Off" ]; then
        echo "off"
    else
        echo "unknown"
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
                    timer start
                fi
            ;;
            off)
                timer_on=$(timer_is_start)
                if [ $timer_on = "on" ]; then
                    timer stop
                fi
            ;;
        esac
    fi

    scr_status=$scr

    sleep 5
done
