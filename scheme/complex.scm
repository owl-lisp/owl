
(define-library (scheme complex)

   (import
      (scheme base)
      (only (owl defmac) lets)
      (only (owl math-extra) sqrt)
      (only (owl syscall) error))

  (export angle imag-part magnitude make-polar make-rectangular real-part)

  (begin

    (define (angle z)
      (error "angle" "Not supported by the implementation."))

    (define (imag-part z)
      (lets ((real imag z)) imag))

    (define (real-part z)
      (lets ((real imag z)) real))

    (define (magnitude z)
      (sqrt (+ (square (imag-part z))
               (square (real-part z)))))

    (define (make-polar mag ang)
      (error "make-polar" "Not supported by the implementation."))

    (define (make-rectangular rl im)
      (+ rl (* im 0+i)))))
