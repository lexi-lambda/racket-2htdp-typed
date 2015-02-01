#lang typed/racket/base

(require (for-syntax racket/base
                     syntax/parse
                     racket/class
                     racket/draw)
         (only-in typed/racket/gui
                  Bitmap%
                  Font-Family Font-Style Font-Weight)
         typed/lang/posn)

; exposes the color names from racket/draw as a type
(define-syntax (define-color-database-types stx)
  (syntax-parse stx
    [(_ type-name:id)
     (define color-names (send the-color-database get-names))
     (with-syntax ([(color-name ...) (datum->syntax stx color-names)]
                   [(color-symbol ...) (datum->syntax stx (map string->symbol color-names))])
       #'(define-type type-name (U color-name ... 'color-symbol ...)))]))
(define-color-database-types ColorDatabase-Color)

;; opaque types
;; ---------------------------------------------------------------------------------------------------

(require/typed/provide
 2htdp/image
 ; 2.3.8 Image Predicates
 [#:opaque htdp:image image?]
 [#:opaque Color color?]
 [#:opaque Pen pen?])

; provide capitalized predicates as well
(provide
 (rename-out [color? Color?]
             [pen? Pen?]))

;; simple exports
;; ---------------------------------------------------------------------------------------------------

(require/typed/provide
 2htdp/image
 ; 2.3.1 Basic Images
 [circle (Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
 [ellipse (Nonnegative-Real Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
 [line (Real Real (U Pen Image-Color) -> Image)]
 [add-line (Image Real Real Real Real (U Pen Image-Color) -> Image)]
 [add-curve (Image Real Real Angle Real Real Real Angle Real (U Pen Image-Color) -> Image)]
 [text (String Positive-Byte Image-Color -> Image)]
 [text/font (String Positive-Byte Image-Color (Option String) Font-Family Font-Style Font-Weight Any -> Image)]
 [empty-image Image]
 ; 2.3.2 Polygons
 [triangle (Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
 [right-triangle (Nonnegative-Real Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
 [isosceles-triangle (Nonnegative-Real Angle Mode (U Pen Image-Color) -> Image)]
 [triangle/sss (Nonnegative-Real Nonnegative-Real Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
 [triangle/ass (Angle Nonnegative-Real Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
 [triangle/sas (Nonnegative-Real Angle Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
 [triangle/ssa (Nonnegative-Real Nonnegative-Real Angle Mode (U Pen Image-Color) -> Image)]
 [triangle/aas (Angle Angle Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
 [triangle/asa (Angle Nonnegative-Real Angle Mode (U Pen Image-Color) -> Image)]
 [triangle/saa (Nonnegative-Real Angle Angle Mode (U Pen Image-Color) -> Image)]
 [square (Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
 [rectangle (Nonnegative-Real Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
 [rhombus (Nonnegative-Real Angle Mode (U Pen Image-Color) -> Image)]
 [star (Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
 [star-polygon (Nonnegative-Real Positive-Integer Positive-Integer Mode (U Pen Image-Color) -> Image)]
 [radial-star (Positive-Integer Nonnegative-Real Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
 [regular-polygon (Nonnegative-Real Positive-Integer Mode (U Pen Image-Color) -> Image)]
 [polygon ((Listof Posn) Mode (U Pen Image-Color) -> Image)]
 [add-polygon (Image (Listof Posn) Mode (U Pen Image-Color) -> Image)]
 [scene+polygon (Image (Listof Posn) Mode (U Pen Image-Color) -> Image)]
 ; 2.3.3 Overlaying Images
 [overlay (Image Image Image * -> Image)]
 [overlay/align (X-Place Y-Place Image Image Image * -> Image)]
 [overlay/offset (Image Real Real Image -> Image)]
 [overlay/align/offset (X-Place Y-Place Image Real Real Image -> Image)]
 [overlay/xy (Image Real Real Image -> Image)]
 [underlay (Image Image Image * -> Image)]
 [underlay/align (X-Place Y-Place Image Image Image * -> Image)]
 [underlay/offset (Image Real Real Image -> Image)]
 [underlay/align/offset (X-Place Y-Place Image Real Real Image -> Image)]
 [underlay/xy (Image Real Real Image -> Image)]
 [beside (Image Image Image * -> Image)]
 [beside/align (Y-Place Image Image Image * -> Image)]
 [above (Image Image Image * -> Image)]
 [above/align (X-Place Image Image Image * -> Image)]
 ; 2.3.4 Placing Images & Scenes
 [empty-scene ((Nonnegative-Real Nonnegative-Real) (Image-Color) . ->* . Image)]
 [place-image (Image Real Real Image -> Image)]
 [place-image/align (Image Real Real X-Place Y-Place Image -> Image)]
 [place-images ((Listof Image) (Listof Posn) Image -> Image)]
 [place-images/align ((Listof Image) (Listof Posn) X-Place Y-Place Image -> Image)]
 [scene+line (Image Real Real Real Real (U Pen Image-Color) -> Image)]
 [scene+curve (Image Real Real Angle Real Real Real Angle Real (U Pen Image-Color) -> Image)]
 ; 2.3.5 Rotating, Scaling, Flipping, Cropping, and Framing Images
 [rotate (Angle Image -> Image)]
 [scale (Positive-Real Image -> Image)]
 [scale/xy (Positive-Real Positive-Real Image -> Image)]
 [flip-horizontal (Image -> Image)]
 [flip-vertical (Image -> Image)]
 [crop (Real Real Nonnegative-Real Nonnegative-Real Image -> Image)]
 [crop/align (X-Place Y-Place Nonnegative-Real Nonnegative-Real Image -> Image)]
 [frame (Image -> Image)]
 [color-frame ((U Pen Image-Color) Image -> Image)]
 ; 2.3.6 Bitmaps
 ; TODO: missing `bitmap` macro
 [bitmap/url (String -> Image)]
 [bitmap/file (Path-String -> Image)]
 [image->color-list (Image -> (Listof Color))]
 [color-list->bitmap ((Listof Color) Nonnegative-Real Nonnegative-Real -> Image)]
 [freeze (case->
          (Image -> Image)
          (Nonnegative-Real Nonnegative-Real Image -> Image)
          (Real Real Nonnegative-Real Nonnegative-Real Image -> Image))]
 ; 2.3.7 Image Properties
 [image-width (Image -> Nonnegative-Integer)]
 [image-height (Image -> Nonnegative-Integer)]
 [image-baseline (Image -> Nonnegative-Integer)]
 ; 2.3.8 Image Predicates
 [mode? (Any -> Boolean)]
 [image-color? (Any -> Boolean)]
 [color ((Byte Byte Byte) (Byte) . ->* . Color)]
 [make-color ((Byte Byte Byte) (Byte) . ->* . Color)]
 [y-place? (Any -> Boolean)]
 [x-place? (Any -> Boolean)]
 [angle? (Any -> Boolean)]
 [side-count? (Any -> Boolean)]
 [step-count? (Any -> Boolean)]
 [real-valued-posn? (Any -> Boolean)]
 [pen (Image-Color Nonnegative-Real Pen-Style Pen-Cap Pen-Join -> Pen)]
 [make-pen (Image-Color Nonnegative-Real Pen-Style Pen-Cap Pen-Join -> Pen)]
 ; 2.3.10 Pinholes
 [center-pinhole (Image -> Image)]
 [put-pinhole (Integer Integer -> Image)]
 [pinhole-x (Image -> (Option Integer))]
 [pinhole-y (Image -> (Option Integer))]
 [clear-pinhole (Image -> Image)]
 [overlay/pinhole (Image Image Image * -> Image)]
 [underlay/pinhole (Image Image Image * -> Image)]
 ; 2.3.11 Exporting Images to Disk
 [save-image ((Image Path-String) (Nonnegative-Real Nonnegative-Real) . ->* . Boolean)]
 [save-svg-image ((Image Path-String) (Nonnegative-Real Nonnegative-Real) . ->* . Boolean)])

;; derived / reimplemented exports
;; ---------------------------------------------------------------------------------------------------

(provide
 Image image?
 Image-Color Image-Color? image-color?
 Mode Mode? mode?
 Angle Angle?
 X-Place Y-Place X-Place? Y-Place?
 Pen-Style Pen-Cap Pen-Join Pen-Style? Pen-Cap? Pen-Join?
 Font-Family Font-Style Font-Weight Font-Family? Font-Style? Font-Weight?)

;; derived type declarations
;; ---------------------------------------------------------------------------------------------------

(define-type Image htdp:image) ; TODO: missing Image-Snip% instance possibility
(define-type Image-Color (U Color ColorDatabase-Color))
(define-type Mode (U 'solid "solid" 'outline "outline" Byte))
(define-type Angle Real) ; more specifically, Finite-NonNaN-Real, which isn't a type
(define-type X-Place (U 'left 'right 'middle 'center 'pinhole
                        "left" "right" "middle" "center" "pinhole"))
(define-type Y-Place (U 'top 'bottom 'middle 'center 'baseline 'pinhole
                        "top" "bottom" "middle" "center" "baseline" "pinhole"))
(define-type Pen-Style (U 'solid 'dot 'long-dash 'short-dash 'dot-dash
                          "solid" "dot" "long-dash" "short-dash" "dot-dash"))
(define-type Pen-Cap (U 'round 'projecting 'butt "round" "projecting" "butt"))
(define-type Pen-Join (U 'round 'bevel 'miter "round" "bevel" "miter"))

(define-predicate Image-Color? Image-Color)
(define-predicate Mode? Mode)
(define-predicate Angle? Angle)
(define-predicate X-Place? X-Place)
(define-predicate Y-Place? Y-Place)
(define-predicate Pen-Style? Pen-Style)
(define-predicate Pen-Cap? Pen-Cap)
(define-predicate Pen-Join? Pen-Join)

(define-predicate Font-Family? Font-Family)
(define-predicate Font-Style? Font-Style)
(define-predicate Font-Weight? Font-Weight)
