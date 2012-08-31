(let loop ((rst (seed->rands (time-ms))) (n 0))
   (if (= n 10)
      (print "ok")
      (lets
         ((rst len (rand rst 1000))
          (rst alpha (rand rst (* len 2)))
          (rst nums (random-numbers rst alpha len))
          (rst perm (random-permutation rst nums)))
         (if (equal? (sort < perm) (sort < nums))
            (loop rst (+ n 1))
            (print (list 'bad 'nums nums 'perm perm))))))

