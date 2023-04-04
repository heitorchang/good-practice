(display "Running R5RS Scheme\n")

(define (my-add a b)
  (+ a b))

;; For The Little Schemer
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
