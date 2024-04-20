#! /bin/sh


echo "now runnig 1k";
(time raco  cover  test/cover-test.rkt  tysolver.rkt) &> dataout/logout-1k.txt
if [ -d coverage ]
  then 
   mv coverage dataout/coverarage-1k
fi

echo "now runnig 10k";

(time raco  cover  test/cover-test-10k.rkt  tysolver.rkt) &> dataout/logout-10k.txt
if [ -d coverage ]
  then
   mv coverage dataout/coverarage-10k
fi

echo "now runnig 100k";

(time raco  cover  test/cover-test-100k.rkt  tysolver.rkt) &> dataout/logout-100k.txt
if [ -d coverage ]
  then
   mv coverage dataout/coverarage-100k
fi

echo "Finished";
