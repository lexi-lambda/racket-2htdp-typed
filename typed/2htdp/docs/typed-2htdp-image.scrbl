#lang scribble/manual

@(require (for-label typed/2htdp/image
                     (only-meta-in 0 typed/racket)))
@(define-syntax-rule @deftypeform[stuff ...]
   @defform[#:kind "type" stuff ...])
@(define-syntax-rule @deftypeidform[stuff ...]
   @defidform[#:kind "type" stuff ...])

@title{typed/2htdp/image}

@defmodule[typed/2htdp/image]{
A typed version of @racketmodname[2htdp/image].
}

@section{Basic Images}

@defproc[(circle [radius Nonnegative-Real] [mode Mode] [pen-or-color (U Pen Image-Color)]) Image]{
Constructs a circle with the given radius, mode, and color.
}

@defproc[(ellipse [width Nonnegative-Real] [height Nonnegative-Real]
                  [mode Mode] [pen-or-color (U Pen Image-Color)]) Image]{
Constructs an ellipse with the given width, height, mode, and color.
}

@defproc[(line [x1 Real] [y1 Real] [pen-or-color (U Pen Image-Color)]) Image]{
Constructs an image representing a line segment that connects the points (0,0) to (x1,y1).
}

@defproc[(add-line [image Image] [x1 Real] [y1 Real] [x2 Real] [y2 Real]
                   [pen-or-color (U Pen Image-Color)]) Image]{
Adds a line to the image @racket[image], starting from the point (x1],y1) and
going to the point (x2,y2). 
}

@defproc[(add-curve [image Image] [x1 Real] [y1 Real] [angle1 Angle] [pull1 Real] [x2 Real] [y2 Real]
                    [angle2 Angle] [pull2 Real] [pen-or-color (U Pen Image-Color)]) Image]{
Adds a curve to @racket[image], starting at the point (x1,y1), and ending at the point (x2,y2).
}

@defproc[(text [string String] [font-size Positive-Byte] [color Image-Color]) Image]{
Constructs an image that draws the given string, using the font size and color.
}

@defproc[(text/font [string String] [font-size Positive-Byte] [color Image-Color]
                    [face (Option String)] [family Font-Family] [style Font-Style]
                    [weight Font-Weight] [underline? Any]) Image]{
Constructs an image that draws the given string, using a complete font specification.
}

@defthing[empty-image Image]{The empty image.}

@section{Polygons}

@section{Overlaying Images}

@defproc[(overlay [i1 Image] [i2 Image] [is Image] ...) Image]{
Overlays all of its arguments building a single image. The first argument goes
on top of the second argument, which goes on top of the third argument, etc. The
images are all lined up on their centers.
}

@defproc[(overlay/align [x-place X-Place] [y-place Y-Place] [i1 Image] [i2 Image] [is Image] ...)
         Image]

@defproc[(overlay/offset [i1 Image] [x Real] [y Real] [i2 Image]) Image]

@defproc[(overlay/align/offset [x-place X-Place] [y-place Y-Place] [i1 Image] [x Real] [y Real]
                               [i2 Image]) Image]

@defproc[(overlay/xy [i1 Image] [x Real] [y Real] [i2 Image]) Image]

@defproc[(underlay [i1 Image] [i2 Image] [is Image] ...) Image]

@defproc[(underlay/align [x-place X-Place] [y-place Y-Place] [i1 Image] [i2 Image] [is Image] ...)
         Image]

@defproc[(underlay/offset [i1 Image] [x Real] [y Real] [i2 Image]) Image]

@defproc[(underlay/align/offset [x-place X-Place] [y-place Y-Place] [i1 Image] [x Real] [y Real]
                                [i2 Image]) Image]

@defproc[(underlay/xy [i1 Image] [x Real] [y Real] [i2 Image]) Image]

@defproc[(beside [i1 Image] [i2 Image] [is Image] ...) Image]{
Constructs an image by placing all of the argument images in a horizontal row,
aligned along their centers.
}

@defproc[(beside/align [y-place Y-Place] [i1 Image] [i2 Image] [is Image] ...) Image]

@defproc[(above [i1 Image] [i2 Image] [is Image] ...) Image]{
Constructs an image by placing all of the argument images in a vertical row,
aligned along their centers.
}

@defproc[(above/align [x-place X-Place] [i1 Image] [i2 Image] [is Image] ...) Image]

@section{Placing Images & Scenes}

@defproc[(empty-scene [width Nonnegative-Real] [height Nonnegative-Real]
                      [color Image-Color "white"]) Image]

@;{
 [place-image (Image Real Real Image -> Image)]
 [place-image/align (Image Real Real X-Place Y-Place Image -> Image)]
 [place-images ((Listof Image) (Listof Posn) Image -> Image)]
 [place-images/align ((Listof Image) (Listof Posn) X-Place Y-Place Image -> Image)]
 [scene+line (Image Real Real Real Real (U Pen Image-Color) -> Image)]
 [scene+curve (Image Real Real Angle Real Real Real Angle Real (U Pen Image-Color) -> Image)]
 }

@section{Rotating, Scaling, Flipping, Cropping, and Framing Images}

@section{Bitmaps}

@section{Image Properties}

@defproc[(image-width [i Image]) Nonnegative-Integer]{
Returns the width of @racket[i].
}

@defproc[(image-height [i Image]) Nonnegative-Integer]{
Returns the height of @racket[i].
}

@defproc[(image-baseline [i Image]) Nonnegative-Integer]{
Returns the distance from the top of the image to its baseline. The baseline of
an image is the place where the bottoms any letters line up, but without
counting the descenders, e.g. the tail on "y" or "g" or "j".

Unless the image was constructed with @racket[text], @racket[text/font] or, in
some cases, @racket[crop], this will be the same as its height.
}

@section{Image Types and Predicates}

@deftogether[[
  @deftypeidform[Image]
  @defproc[(image? [x Any]) Boolean]]]{
A type and a predicate for images.}

@deftogether[[
  @deftypeidform[Mode]
  @defproc[(mode? [x Any]) Boolean]]]

@deftogether[[
  @deftypeidform[Image-Color]
  @defproc[(image-color? [x Any]) Boolean]]]{
A type and a predicate for colors, either @racket[Color]s or strings representing colors.
}

@deftogether[[
  @deftypeidform[Color]
  @defstruct[color ([red Byte] [green Byte] [blue Byte] [alpha Byte])]]]{
A struct for colors.
}

@deftogether[[
  @deftypeidform[Y-Place]
  @defproc[(y-place? [x Any]) Boolean]]]

@deftogether[[
  @deftypeidform[X-Place]
  @defproc[(x-place? [x Any]) Boolean]]]

@deftogether[[
  @deftypeidform[Angle]
  @defproc[(angle? [x Any]) Boolean]]]

@;{
 [side-count? (Any -> Boolean)]
 [step-count? (Any -> Boolean)]
 [real-valued-posn? (Any -> Boolean)]
 }

@deftogether[[
  @deftypeidform[Pen]
  @defstruct[pen ([color Image-Color] [width Nonnegative-Real] [style Pen-Style] [cap Pen-Cap]
                                      [join Pen-Join])]]]

@deftogether[[
  @deftypeidform[Pen-Style]
  @defproc[(Pen-Style? [x Any]) Boolean]]]

@deftogether[[
  @deftypeidform[Pen-Cap]
  @defproc[(Pen-Cap? [x Any]) Boolean]]]

@deftogether[[
  @deftypeidform[Pen-Join]
  @defproc[(Pen-Join? [x Any]) Boolean]]]

@deftogether[[
  @deftypeidform[Font-Family]
  @defproc[(Font-Family? [x Any]) Boolean]]]

@deftogether[[
  @deftypeidform[Font-Style]
  @defproc[(Font-Style? [x Any]) Boolean]]]

@deftogether[[
  @deftypeidform[Font-Weight]
  @defproc[(Font-Weight? [x Any]) Boolean]]]

@section{Equality Testing of Images}

@section{Pinholes}

@section{Exporting Images to Disk}


