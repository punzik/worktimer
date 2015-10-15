Программа написана на [Scheme](http://www.schemers.org/) (реализация -
[GNU Guile](http://www.gnu.org/software/guile/)) и предназначена для
учета рабочего времени. Применяется следующим образом:

    Usage: timer [command]
    Commands:
        start [TASK]                    Start new task. If no task, use last runned task
        stop                            Stop task
        report                          Show report
        report day [DATE]               Show report for today or DATE
        report week [DATE]              Show report for current week or week of DATE
        report month [DATE]             Show report for current month or month of DATE
        report TASK                     Show report for project
        deadline set [TASK] DATE|TIME   Add deadline for project (or for last task)
        deadline clear [TASK]           Remove deadline for project (or for last task)
        deadline [TASK]                 Show deadline for project
        deadline all                    Show all deadlines
        timesheet                       Show all raw events
        timesheet day [DATE]            Show raw events for today or DATE
        timesheet week [DATE]           Show raw events for current week or week of DATE
        timesheet month [DATE]          Show raw events for current month or month of DATE
        timesheet TASK                  Show raw events
        archive                         Show archive tasks
        archive TASK                    Add task to archive
        unarch TASK                     Remove task from archive
        refresh                         Refresh worksheet file after manual edit
        (no command)                    Show running task and timer

Начало работы над задачей:

    $ worktimer.scm start [НАЗВАНИЕ_ЗАДАЧИ]

Название задачи может состоять из нескольких частей, разделенных символом
прямого слэша. Части названия - это элементы иерархии задачи, например, название
проекта, название подпроекта, задача, подзадача и т.д. Программа учитывает
иерархическую структуру задач и вычисляет время задач, как сумму времен
подзадач.

Пример:

    $ worktimer.scm start uberproject/website/design

Стартует отсчет времени на подзадачу "design" задачи "website" проекта
"uberproject". Уровни вложенности не ограничены.  Если команде `start` не
передать агрумент, то будет запущена последняя задача.

Остановить отсчет времени можно командой `stop`:

    $ worktimer.scm stop

или запустив новую задачу:

    $ worktimer.scm start uberproject/website/programming

По команде `report` выводится отчет по всем проектам и задачам:

    $ worktimer.scm report
    --- REPORT
    Overall: 00:03:45
      uberproject: 00:03:45
        website: 00:03:45
          programming: 00:02:34
          design: 00:01:11

    --- LAST STOPPED TASK
    uberproject/website/programming: [2014-04-18 09:47:04] - [2014-04-18 09:49:38] - 00:02:34

Как видно, общее время работы над проектом составило 3 минуты и 45 секунд, из
которых на дизайн сайта ушло 1 минута 11 секунд, а на программирование 2 минуты
34 секунды.

Команда `report` может иметь параметры:

-   `day [DATE]` - отчет по текущему дню или по дате DATE;
-   `week [DATE]` - отчет по текущей неделе или по неделе, в которую входит день
    DATE;
-   `month [DATE]` - отчет по текущему месяцу или по месяцу, в который входит день
    DATE;

Кроме того, параметром команды `report` может быть название проекта, например:

    $ worktimer.scm report uberproject/website
    --- REPORT
    Overall: 00:03:45
      uberproject: 00:03:45
        website: 00:03:45

    --- LAST STOPPED TASK
    uberproject/website/programming: [2014-04-18 09:47:04] - [2014-04-18 09:49:38] - 00:02:34

При запуске программы без аргументов выводится имя текущей задачи и текущий
таймер:

    $ worktimer.scm
    uberproject/website/programming: 00:02:51

Команда `deadline` предназначена для добавления и удаления дедлайнов. Дедлайны
могут быть датой, до которой нужно закончить проект, или временем, которое можно
потратить на выполнение задачи.

Установить дедлайт для проекта можно командой `deadline set`. В качестве
аргумента может выступать имя задачи и дата/время или просто дата/время (в этом
случае дедлайн установится для последней запущеной задачи).

Удаляется дедлайн командой `deadline clear` с именем проекта в качестве
параметра.

Посмотреть установленные дедлайны можно командой `deadline all`. Команда `deadline`
без параметров выведет дедлайн для текущей задачи.

Дедлайны отображаются в отчете. Для дедлайна по времени признак истечения
времени зависит от периода, за который сформирован отчет. Например, если дедлайн
по проекту 1 час, а всего на проект потрачено 2 часа, из которых сегодня только
20 минут, то при формировании отчета за весь период (или по проекту) признак
превышения времени будет установлен. А при отчете за сегодня, до дедлайна
останется еще 40 минут. Это может быть удобно при планировании времени на
день/неделю/месяц.

Для того, чтобы посмотреть историю работы, можно воспользоваться командой
`timesheet`. Эта команда показывает выполнявшиеся задачи в таком виде, в котором
они сохраняются на диск. Параметры команды такие-же, как у команды `report`.

Задачи (или подзадачи) можно отправлять в архив командой `archive [TASK]`. После этого
они перестают отображаться в отчете и в списке задач. Посмотреть список
архивированных задач можно командой `archive` без параметров. Деархивировать
задачу можно командой `unarch [TASK]`.

# Автодополнение для zsh

Для включения автодополнения в zsh скопируйте файл `zsh-completion/_timer` в
`~/.zsh-completion/`, и добавте в файл `~/.zshrc` строки

    fpath=(~/.zsh-completions $fpath)
    autoload -Uz compinit
    compinit

И не забудте сделать симлинк (например) `/usr/local/bin/timer` на `worktimer.scm`.

# Автодополнение для bash

Автодополнение в bash включается так:

    $ . bash-completion/timer
