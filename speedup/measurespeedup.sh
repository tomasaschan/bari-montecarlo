#!/bin/bash

cd ~/plugg/bari/development

for nproc in {1,2,3,4,5,6,7,8}
do
    export NPROC=$nproc
    echo -n $nproc
    echo -n -e '\t'
    make runvp > /dev/null
    cat histogram.out | egrep -e "Every" | sed s/\#\ // | sed s/.*\:\ //g | sed s/.*took\ \ //g  | sed s/s//g #\(\d{1,2}\.\d{3}E[\+\-]\d{2}\)\ s/\1/g
    #-e "\#\ Runs"
done
