#lang typed/racket/base

(require (for-syntax racket/base
                     racket/function
                     racket/syntax
                     syntax/parse)
         typed/2htdp/image
         (only-in 2htdp/universe
                  to-draw
                  on-tick
                  on-key
                  on-release
                  on-pad
                  on-mouse
                  stop-when
                  record?
                  name))

;; simple exports
;; ---------------------------------------------------------------------------------------------------

(require/typed/provide
 2htdp/universe
 ; 2.4.2 Simple Simulations
 [animate ((Natural -> Image) -> Natural)]
 [run-simulation ((Natural -> Image) -> #t)]
 [run-movie (Positive-Real (Listof Image) -> #t)]
 ; 2.4.3 Interactions
 [key-event? (Any -> Boolean)]
 [key=? (String String -> Boolean)]
 [pad-event? (Any -> Boolean)]
 [pad=? (String String -> Boolean)]
 [mouse-event? (Any -> Boolean)]
 [mouse=? (Mouse-Event Mouse-Event -> Boolean)])

;; derived/reimplemented exports
;; ---------------------------------------------------------------------------------------------------

(provide
 Mouse-Event Mouse-Event?)

;; derived types
;; ---------------------------------------------------------------------------------------------------

; these would be nice, but due to how TR prints type aliases, they need to be ignored
; (define-type Scene Image)
; (define-type Key-Event String)
; (define-type Pad-Event Key-Event)

(define-type Mouse-Event (U "button-down" "button-up" "drag" "move" "enter" "leave"))

(define-predicate Mouse-Event? Mouse-Event)

;; typed big-bang
;; ---------------------------------------------------------------------------------------------------

(provide big-bang
         to-draw
         on-tick
         on-key
         on-release
         on-pad
         on-mouse
         stop-when
         record?
         name)

(require/typed
 "private/big-bang-wrapper.rkt"
 [big-bang*
  (All [World]
       (->* [World
             #:to-draw (World -> Image)]
            [#:on-tick (World -> World)
             #:tick-rate Positive-Real
             #:tick-limit (Option Positive-Integer)
             #:on-key (World String -> World)
             #:on-release (World String -> World)
             #:on-pad (Option (World String -> World))
             #:on-mouse (World Integer Integer Mouse-Event -> World)
             #:stop-when (World -> Boolean)
             #:last-picture (Option (World -> Image))
             #:record? Any
             #:name (U String Symbol)]
            World))])

(define-syntax (big-bang stx)
  (syntax-parse stx
    #:literals (: to-draw on-tick on-key on-release on-pad on-mouse stop-when record? name)
    [(_
      (~describe "initial-world-state : world-state-type"
                 (~seq initial-state:expr : world-type:expr))
      (~or (~once [to-draw draw:expr] #:name "to-draw clause")
           (~optional (~or [on-tick tick:expr]
                           [on-tick tick:expr tick-rate:expr]
                           [on-tick tick:expr tick-rate:expr tick-limit:expr])
                      #:name "on-tick clause"
                      #:defaults ([tick #f] [tick-rate #f] [tick-limit #f]))
           (~optional [on-key key:expr] #:name "on-key clause" #:defaults ([key #f]))
           (~optional [on-release release:expr] #:name "on-release clause" #:defaults ([release #f]))
           (~optional [on-pad pad:expr] #:name "on-pad clause" #:defaults ([pad #f]))
           (~optional [on-mouse mouse:expr] #:name "on-mouse clause" #:defaults ([mouse #f]))
           (~optional (~or [stop-when stop:expr]
                           [stop-when stop:expr last-picture:expr])
                      #:name "stop-when clause"
                      #:defaults ([stop #f] [last-picture #f]))
           (~optional [record? record:expr] #:name "record? clause" #:defaults ([record #f]))
           (~optional [name nm:expr] #:name "name clause" #:defaults ([nm #f])))
      ...)
     (define clauses
       (list #'[#:to-draw draw]
             (and (attribute tick) #'[#:on-tick tick])
             (and (attribute tick-rate) #'[#:tick-rate tick-rate])
             (and (attribute tick-limit) #'[#:tick-limit tick-limit])
             (and (attribute key) #'[#:on-key key])
             (and (attribute release) #'[#:on-release release])
             (and (attribute pad) #'[#:on-pad pad])
             (and (attribute mouse) #'[#:on-mouse mouse])
             (and (attribute stop) #'[#:stop-when stop])
             (and (attribute last-picture) #'[#:last-picture last-picture])
             (and (attribute record) #'[#:record? record])
             (and (attribute nm) #'[#:name nm])))
     (define/with-syntax (clause ...)
       (apply append (map syntax->list (filter identity clauses))))
     (quasisyntax/loc stx
       ((inst big-bang* world-type)
        #,(syntax/loc stx (ann initial-state world-type))
        clause ...))]))
