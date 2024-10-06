#!/bin/bash

MALLOC_CHECK_=2 fpm test --flag -g --flag -lmcheck --c-flag -g --c-flag -lmcheck

location=$(find ./build/ -type f -name "example")

if [ "$location" == "" ]; then
  echo "Script error, blank!"
  exit 1
fi
	

valgrind --leak-check=full \
         --fair-sched=yes \
         $location


exit 0