#!/bin/bash
cd src
swipl --quiet -f ./load.pl -g 'server(3030)'
