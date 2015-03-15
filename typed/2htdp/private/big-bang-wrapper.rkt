#lang racket/base

(provide big-bang*)

(require (for-syntax racket/base
                     racket/list
                     racket/function
                     unstable/list
                     racket/syntax
                     syntax/parse)
         2htdp/universe)

;; big-bang changes behavior based on whether or not clauses are present, which is inconvenient when
;; attempting to create a function wrapper. This automatically generates a cond form to handle all
;; possible cases.
(define-syntax (big-bang-cross-product stx)
  (syntax-parse stx
    [(_ world
        (constant-clause ...)
        (clause-condition clause-true (~optional clause-false #:defaults ([clause-false #f])))
        ...)
     ; wrap each condition with its true case and false case
     (define clause-dict
       (for/list ([condition (in-list (attribute clause-condition))]
                  [clause-t (in-list (attribute clause-true))]
                  [clause-f (in-list (attribute clause-false))])
         (list condition (list clause-t clause-f))))
     ; all the possible cases (each clause-condition can be #t or #f)
     (define case-pairs (map (curryr list #f) (attribute clause-condition)))
     (define all-cases (apply cartesian-product case-pairs))
     ; convert the set of cases into cond clauses
     (define/with-syntax (cond-case ...)
       (for/list ([case (in-list all-cases)])
         ; all the non-#f conditions need to be considered for the cond condition
         (define conditions (filter identity case))
         (define/with-syntax cond-condition
           (if (empty? conditions)
               #'else
               (with-syntax ([(c ...) conditions])
                 #'(and c ...))))
         (define big-bang-clauses
           (for/list ([condition (in-list case)]
                      [clause-t (in-list (attribute clause-true))]
                      [clause-f (in-list (attribute clause-false))])
             (if condition clause-t clause-f)))
         (define/with-syntax (case-clause ...) (filter identity big-bang-clauses))
         #'[cond-condition
            (big-bang
             world
             constant-clause ...
             case-clause ...)]))
     #'(cond cond-case ...)]))

(define (big-bang* world
                   #:to-draw renderer
                   #:on-tick [tick (λ (w) w)]
                   #:tick-rate [tick-rate 1/30]
                   #:tick-limit [tick-limit #f]
                   #:on-key [key (λ (w k) w)]
                   #:on-release [release (λ (w k) w)]
                   #:on-pad [pad #f]
                   #:on-mouse [mouse (λ (w x y e) w)]
                   #:stop-when [stop (λ (w) #f)]
                   #:last-picture [last-picture #f]
                   #:record? [record #f]
                   #:name [n "World"])
  (big-bang-cross-product
   world
   ([to-draw renderer]
    [on-key key]
    [on-release release]
    [on-mouse mouse]
    [record? record]
    [name n])
   (pad
    [on-pad pad])
   (tick-limit
    [on-tick tick tick-rate tick-limit]
    [on-tick tick tick-rate])
   (last-picture
    [stop-when stop last-picture]
    [stop-when stop])))
