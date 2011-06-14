#!/bin/bash

if [ "x$HADOOP_HOME" == "x" ]; then
  echo
  echo "Please set HADOOP_HOME to your hadoop installation directory"
  echo
  exit 1
fi

CP="."
for f in ${HADOOP_HOME}/*.jar;     do CP="$CP:$f"; done
for f in ${HADOOP_HOME}/lib/*.jar; do CP="$CP:$f"; done

export CLASSPATH="$CP:$CLASSPATH"

runhaskell test.hs
