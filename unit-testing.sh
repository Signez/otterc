
# Where is your reference toolc?
TOOLC=$1

for i in testcases/*.tool
do 
  SIMPLENAME="${i#testcases/}"
  SIMPLENAME="${SIMPLENAME%.tool}"
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
    if [ "`cat /tmp/recompiled.lint | grep -v Picked | wc -l`" -gt 0 ]
    then
      echo "[ FAIL ] $i ; lint : "
      cat /tmp/recompiled.lint
    else
      echo "[  OK  ] Lint $i"
      COMP1="`java -jar $TOOLC $i && java $SIMPLENAME | grep -v Picked > /tmp/original.run`"
      COMP2="`java -jar $TOOLC /tmp/recompiled.tool 2>&1 && java $SIMPLENAME | grep -v Picked > /tmp/recompiled.run`"

      if [ "`diff /tmp/original.run /tmp/recompiled.run | grep -v Picked | wc -l`" -gt 0 ]
      then
        echo "[ FAIL ] $i, not the same :("
        echo "`diff /tmp/original.run /tmp/recompiled.run | grep -v Picked`"
      else
        echo "[  OK  ] Same $i"
      fi
    fi
  fi
  
done
