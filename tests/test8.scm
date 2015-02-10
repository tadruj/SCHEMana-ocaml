(set! number 2)
(define (square x) (* x x))
(square number)
(define (cube x) (* x (square x)))
(cube number)
(define (quad x) (* (square x) (square x)))
(quad number)
((lambda (x) x) 10)
((lambda (x y) (+ y x)) 1 2)
((lambda (x y z) (cons x (cons y (cons z ())))) 1 2 3)
(define (count n) (if (>= n 10) n (count (+ n 1))))
(count 0)