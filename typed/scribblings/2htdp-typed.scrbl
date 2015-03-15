#lang scribble/manual

@(require (for-label typed/racket/base
                     typed/2htdp/universe))

@title{HtDP/2e Teachpacks for Typed Racket}

This package provides typed wrappers for the @italic{How to Design Programs: 2nd Edition} teachpacks.
Unless otherwise documented, they all function identically to their untyped counterparts.

@section{Typed @racket[big-bang]}

@defmodule[typed/2htdp/universe]

@defform[#:literals (: on-tick on-key on-pad on-release on-mouse to-draw stop-when record? name)
         (big-bang state-expr : state-type clause ...)
         #:grammar
         ([clause (on-tick tick-expr)
                  (on-tick tick-expr rate-expr)
                  (on-tick tick-expr rate-expr limit-expr)
                  (on-key key-expr)
                  (on-pad pad-expr)
                  (on-release release-expr)
                  (on-mouse mouse-expr)
                  (to-draw draw-expr)
                  (stop-when stop-expr)
                  (stop-when stop-expr last-scene-expr)
                  (record? r-expr)
                  (name name-expr)])]{

Identical to the untyped equivalent, but requires an explicit type annotation to determine what type
should be used for the world's state. Also does not support networked universes, only local worlds.}
