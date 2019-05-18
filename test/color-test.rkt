#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../typed/2htdp/image.rkt")

(define a (color 10 20 30))
(define b (make-color 40 50 60 70))

(check-expect (color? a) true)
(check-expect (color? b) true)

(check-expect (color-red a) 10)
(check-expect (color-green b) 50)
(check-expect (color-blue a) 30)
(check-expect (color-alpha b) 70)

;(check-expect (color=? a a) true)
;(check-expect (color=? a b) false)

(define (sum-of-rgb [c : Color]) : Integer
  (+ (color-red c)
     (color-green c)
     (color-blue c)))

(check-expect (sum-of-rgb a) 60)

(test)

