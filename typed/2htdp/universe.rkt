#lang typed/racket/base

;; TODO:
;; Very much a WIP. Going to need more than a little spit and polish to make this work in typed code.

(require typed/2htdp/image
         (only-in 2htdp/universe
                  big-bang
                  on-tick
                  to-draw))

(provide big-bang
         on-tick
         to-draw)

;; opaque types
;; ---------------------------------------------------------------------------------------------------

;; simple exports
;; ---------------------------------------------------------------------------------------------------

(require/typed/provide
 2htdp/universe
 ; 2.4.2 Simple Simulations
 [animate ((Natural -> Scene) -> Natural)]
 [run-simulation ((Natural -> Scene) -> #t)]
 [run-movie (Positive-Real (Listof Image) -> #t)]
 ; 2.4.3 Interactions
 [key-event? (Any -> Boolean)]
 [key=? (Key-Event Key-Event -> Boolean)]
 [pad-event? (Any -> Boolean)]
 [pad=? (Pad-Event Pad-Event -> Boolean)]
 [mouse-event? (Any -> Boolean)]
 [mouse=? (Mouse-Event Mouse-Event -> Boolean)])

;; derived/reimplemented exports
;; ---------------------------------------------------------------------------------------------------

(provide
 Scene
 Mouse-Event Mouse-Event?)

;; derived types
;; ---------------------------------------------------------------------------------------------------

(define-type Scene (U Image))
(define-type Key-Event String)
(define-type Pad-Event Key-Event)
(define-type Mouse-Event (U "button-down" "button-up" "drag" "move" "enter" "leave"))

(define-predicate Mouse-Event? Mouse-Event)
