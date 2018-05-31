
(define path "tmp/rw.txt")

(define (output thing)
  (let ((file (open-output-file path)))
    (or (write-to file thing)
        (print "write failed"))
    (close-port file)))

(define (input)
  (lets
    ((file (open-input-file path))
     (val (read file)))
    (close-port file)
    val))

(define (test val)
  (output val)
  (let ((x (input)))
    (if (equal? val x)
      (print "read/write " val " OK")
      (print "ERROR: " val " → " x))))

(test 0)
(test 10+4/3i)
(test #true)
(test #false)
(test #empty)
(test (put #empty 1 2))
(test (list->ff '((a . 1) (b . 2) (c . 3))))

