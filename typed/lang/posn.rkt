#lang typed/racket/base

; make Posn parametric
(define-type Posn (All (A) htdp:posn))

(provide Posn)

(require/typed
 lang/posn
 [#:opaque htdp:posn posn?])

(require/typed/provide
 lang/posn
 [make-posn (All (A) A A -> (Posn A))]
 [posn-x (All (A) (Posn A) -> A)]
 [posn-y (All (A) (Posn A) -> A)])
