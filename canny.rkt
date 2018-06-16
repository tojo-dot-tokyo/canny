#lang racket
(require "flomap-canny.rkt")
(require pict)
(require racket/draw)
(require images/flomap)

(define (canny img #:sigma sigma #:heigh heigh #:low low)
  (bitmap
   (flomap->bitmap
    (flomap-canny (bitmap->flomap (pict->bitmap img))
                  #:sigma sigma
                  #:heigh heigh
                  #:low low))))
(provide canny)
