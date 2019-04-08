#!/bin/bash
git ls-tree -r master --name-only | egrep "\.(erl|js)" | egrep -v "(three\.js$|state)" | xargs wc -l
