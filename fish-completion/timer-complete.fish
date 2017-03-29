# -*- tab-width: 4 -*-

## Copy file to the fish config dir and add next lines to config.fish:
#
# set -l configdir ~/.config
# if set -q XDG_CONFIG_HOME
#     set configdir $XDG_CONFIG_HOME
# end
#
# source $configdir/fish/timer-complete.fish

function __cmd_contains
    set -l cmdl (commandline -cpo)
    if test (count $cmdl) -le 1
        return 1
    end
    set -l cmdl $cmdl[2..-1]

    for i in $argv
        if contains $i $cmdl
            return 0
        end
    end
    return 1
end

function __cmd_contains_only
    set -l cmdl (commandline -cpo)
    if test (count $cmdl) -le 1
        return 1
    end
    set -l cmdl $cmdl[2..-1]

    for i in $cmdl
        if not contains $i $argv
            return 1
        end
    end
    return 0
end

function __cmd_empty
    test (count (commandline -cpo)) -eq 1
end

set -l __tasklist (timer tasklist)
set -l __lasttask (timer lasttask)
set -l __deadlist (timer deadlist)
set -l __archlist (timer archlist)

# do not use files for complete timer
complete -c timer --no-files

# start
complete -c timer -n "__cmd_empty" -a start -d "Start new task"
complete -c timer -n "__cmd_contains start" -a $__tasklist

# stop
complete -c timer -n "__cmd_empty" -a stop -d "Stop task"
complete -c timer -n "__cmd_contains stop" -a $__lasttask

# report
complete -c timer -n "__cmd_empty" -a report -d "Show report"
complete -c timer -n "__cmd_contains_only report" -a "day week month"
complete -c timer -n "__cmd_contains report; and __cmd_contains day week month" -a (date +'%Y-%m-%d')

#deadline
complete -c timer -n "__cmd_empty" -a deadline -d "Show report"
complete -c timer -n "__cmd_contains_only deadline" -a $__tasklist
complete -c timer -n "__cmd_contains_only deadline" -a "set clear all"
complete -c timer -n "__cmd_contains deadline; and __cmd_contains set" -a "$__tasklist (date +'%Y-%m-%d') (date +'%H:%M:%S')"
complete -c timer -n "__cmd_contains deadline; and __cmd_contains clear" -a $__deadlist

#timesheet
complete -c timer -n "__cmd_empty" -a timesheet -d "Show all raw events"
complete -c timer -n "__cmd_contains_only timesheet" -a $__tasklist
complete -c timer -n "__cmd_contains_only timesheet" -a "day week month"
complete -c timer -n "__cmd_contains timesheet; and __cmd_contains day week month" -a (date +'%Y-%m-%d')

#archive
complete -c timer -n "__cmd_empty" -a archive -d "Show/add archive tasks"
complete -c timer -n "__cmd_contains archive" -a $__tasklist

#unarch
complete -c timer -n "__cmd_empty" -a unarch -d "Remove task from archive"
complete -c timer -n "__cmd_contains unarch" -a $__archlist

#refresh
complete -c timer -n "__cmd_empty" -a refresh -d "Refresh worksheet file"

#help
complete -c timer -n "__cmd_empty" -a help -d "Help"
