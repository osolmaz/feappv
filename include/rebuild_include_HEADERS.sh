#!/bin/bash

sources=`find libfeappv libfeappv_integer8 -name "*.h" -o -type f | LC_COLLATE=POSIX sort`

echo "# Do not edit - automatically generated from $0" > include_HEADERS
echo -n "include_HEADERS = " >> include_HEADERS
for source_with_path in $sources ; do

    echo " \\" >> include_HEADERS
    echo -n "        "include/$source_with_path >> include_HEADERS

done


echo " " >> include_HEADERS

