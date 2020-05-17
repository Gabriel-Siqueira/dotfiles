#!/usr/bin/env bash

if [ -e /tmp/my_task_info ]
then
  info=$(cat /tmp/my_task_info)
else
  info=""
fi

if [ $(timew get dom.active) -eq 0 ]
then
  t=""
  wrapl=''
  wrapr=''
else
  t=$(timew get dom.active.duration | sed -E 's/H|M/:/' | sed -E 's/[A-Z]//g')
  wrapl='%{F#fcba03}'
  wrapr='%{F-}'
fi

echo $wrapl $info $t $wrapr
