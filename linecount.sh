#!/bin/bash
find apps | ( grep "\.erl$" ; echo "static\/game.js" ) | xargs wc -l
