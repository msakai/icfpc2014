(define (fact n)
  (letrec ((fact-iter
	    (lambda (n ret)
	      (if (= n 0)
		  ret
		  (fact-iter (- n 1) (* ret n))))))
    (fact-iter n 1)))

(define (main) (fact 10))
