#!/bin/bash

sources=`find elements  plot  program unix  user -name "*.f" -o -name "*.C" -o -name "*.c" -o -name "*.data" -type f | LC_COLLATE=POSIX sort`

echo "# Do not edit - automatically generated from $0" > libfeappv_la_SOURCES
echo -n "libfeappv_la_SOURCES = " >> libfeappv_la_SOURCES
for source_with_path in $sources ; do

    echo " \\" >> libfeappv_la_SOURCES
    echo -n "        "src/$source_with_path >> libfeappv_la_SOURCES

done


echo " " >> libfeappv_la_SOURCES

