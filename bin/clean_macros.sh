#!/usr/bin/env bash

echo "Delay 100"
echo "Delay 100"
echo "KeyStrPress Shift_L"
echo "Delay 10"
echo "KeyStrRelease Shift_L"
echo "Delay 10"
cat $1 | grep "KeyStrPress\|KeyStrRelease" | sed -E "s/(KeyStrRelease.*)/\1\nDelay 10/"

