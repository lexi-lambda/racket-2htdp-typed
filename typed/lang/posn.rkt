#lang typed/racket/base

(require/typed/provide
 lang/posn
 [#:opaque Posn posn?]
 [make-posn (Real Real -> Posn)]
 [posn-x (Posn -> Real)]
 [posn-y (Posn -> Real)])
