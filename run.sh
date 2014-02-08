#!/bin/sh

EXAMPLE_NAME=$1
JAVA_CP="java -Dfile.encoding=UTF-8 -cp build/classes/main:build/resources/main:src/main/resources/cafebabe-1.2.jar:/home/signez/.gradle/caches/modules-2/files-2.1/org.scala-lang/scala-library/2.10.3/21d99cee3d3e478255ef9fcc90b571fb2ab074fc/scala-library-2.10.3.jar:/home/signez/.gradle/caches/modules-2/files-2.1/org.scala-lang/scala-compiler/2.10.3/fc9759d060ae131a73c020d477e25a14534cbedd/scala-compiler-2.10.3.jar:/home/signez/.gradle/caches/modules-2/files-2.1/org.scala-lang/scala-reflect/2.10.3/16dc45094c2d8919d21ee16a46a7ff7fa2aa3c88/scala-reflect-2.10.3.jar"
CLASS_DIR=$(mktemp -d /tmp/otterXXXX)

$JAVA_CP otterc.Main -d $CLASS_DIR testcases/${EXAMPLE_NAME}.otr

if [ $? -eq 0 ]
then
  java -cp $CLASS_DIR ${EXAMPLE_NAME#*_}
fi
