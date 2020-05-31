#!/usr/bin/env bash

if [ $(timew get dom.active) -ne 0 ]
then
  $HOME/bin/clock.sh
fi

if [ $# -ge 1 ]
then
  id=$1 
else
  col='substr($0,prev["ID"],len["ID"]),substr($0,prev["Description"],500)'
  tasks=$(task daily | awk "BEGIN{OFS=\"\\t\"} NR==2 {for(i=1;i<=NF;i++){name[i]=\$i}} NR==3{acc=0;for(i=1;i<=NF;i++){prev[name[i]]=acc;len[name[i]]=length(\$i);acc+=1+length(\$i)}} NR>3{print $col}" - | head -n -2)
  tasks+=$'\n'
  tasks+=$(cat $MY_OTHER_ACT | sed -E "s/^/-\t/")
  selected=$(echo "$tasks" | rofi -dmenu -i -p 'task')
  info=$(echo "$selected" | cut -f2)
  id=$(echo "$selected" | cut -f1)
fi

if [ $id = '-' ] || [ $id -ne 0 ]
then
  echo $info > /tmp/my_task_info
  echo $id > /tmp/my_taskid
else
  rm -f /tmp/my_task_info
  rm -f /tmp/my_taskid
fi
