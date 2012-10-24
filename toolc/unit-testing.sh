
# Where is your reference toolc?
TOOLC=$1

for i in testcases/*.tool
do 
  scala -classpath target/scala-2.9.2/classes/ toolc.Main $i 2>&1 > /tmp/compiled.tool
  scala -classpath target/scala-2.9.2/classes/ toolc.Main /tmp/compiled.tool 2>&1 > /tmp/recompiled.tool
  diff /tmp/compiled.tool /tmp/recompiled.tool > /tmp/compiled.diff
  if [ "$?" -gt 0 ]
  then
    echo "[ FAIL ] $i ; diff : "
    cat /tmp/compiled.diff
  else
    echo "[  OK  ] Diff $i"

PLOP=`java -jar $TOOLC --lint /tmp/recompiled.tool 2>&1 > /tmp/recompiled.lint`
    if [ "$?" -gt 0 ]
    then
      echo "[ FAIL ] $i ; lint : "
      cat /tmp/recompiled.lint
    else
      echo "[  OK  ] Lint $i"
      COMP1=`java -jar $TOOLC $i 2>&1`
      COMP2=`java -jar $TOOLC /tmp/recompiled.tool 2>&1`

      if [ "$COMP1" == "$COMP2" ]
      then
        echo "[  OK  ] Same $i"
      else
        echo "[ FAIL ] $i, not the same :("
        echo "COMP1> $COMP1"
        echo "COMP2> $COMP2"
      fi
    fi
  fi
  
done

cd ~
