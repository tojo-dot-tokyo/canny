#lang typed/racket
(require images/flomap)
(require racket/flonum)
(require typed/racket/draw)
(require math/array)

(: flomap-grayscale (-> flomap flomap))
(define (flomap-grayscale img)
  (inline-build-flomap
   1 (flomap-width img) (flomap-height img)
   (λ (k x y i)
     (+ (* (flomap-ref img 1 x y) 0.299)
        (* (flomap-ref img 2 x y) 0.587)
        (* (flomap-ref img 3 x y) 0.114)))))
(provide flomap-grayscale)

(: flomap-canny (-> flomap [#:heigh Flonum] [#:low Flonum] flomap))
(define (flomap-canny img #:heigh [heigh 1.0] #:low [low 0.2])
  (let ([img
         (case (flomap-components img)
           [(4) (flomap-grayscale img)]
           [(1) img]
           [else (error "flomap-canny: invalid components")])])
    (canny img #:heigh heigh #:low low)))
(provide flomap-canny)

(: fmatan (-> flomap flomap flomap))
(define (fmatan fm1 fm2)
  (inline-build-flomap
   (flomap-components fm1) (flomap-width fm1) (flomap-height fm1)
   (λ (k x y i)
     (atan (flomap-ref fm1 k x y)
           (flomap-ref fm2 k x y)))))

(define-type Orient (Array (U 'horizontal 'positive-diagonal 'vertical 'negative-diagonal)))
(: tanθ->orient (-> flomap Orient))
(define (tanθ->orient fm)
  (build-array
   `#[,(flomap-height fm) ,(flomap-width fm)]
   (lambda ([v : Indexes])
     (let ([y : Index (vector-ref v 0)]
           [x : Index (vector-ref v 1)])
       (let ([t : Flonum (flomap-ref fm 0 x y)])
         (cond
           [(<= -0.4142 t 0.4142) 'horizontal]
           [(<= -0.4142 t 2.4142) 'positive-diagonal]
           [(>= (abs t) 2.4142)   'vertical]
           [else 'negative-diagonal]))))))

(define-syntax-rule (idx w x y) (+ (* w y) x))

(: trace! (-> FlVector flomap Orient Integer Integer Integer Integer Flonum Void))
(define (trace! vec mag orient w h x y low)
  (let loop ([x x] [y y])
    (when (and (<= 0 x (- w 1)) (<= 0 y (- h 1))
               (< (flvector-ref vec (idx w x y)) 0))
      (define s (flomap-ref mag 0 x y))
      (when (<= low s)
        (flvector-set! vec (idx w x y) s)
        (case (array-ref orient `#(,y ,x))
          [(horizontal)
           (loop (+ x 1) y)
           (loop (- x 1) y)]
          [(positive-diagonal)
           (loop (+ x 1) (+ y 1))
           (loop (- x 1) (- y 1))]
          [(vertical)
           (loop x (+ y 1))
           (loop x (- y 1))]
          [(negative-diagonal)
           (loop (+ x 1) (- y 1))
           (loop (- x 1) (+ y 1))]
          [else
           (error "error")])))))

(: canny (-> flomap [#:heigh Flonum] [#:low Flonum] flomap))
(define canny
  (lambda (fm #:heigh [heigh 1.0] #:low [low 0.2])
    (let* ([w (flomap-width fm)]
           [h (flomap-height fm)]
           [sx (flomap-gradient-x fm)]
           [sy (flomap-gradient-y fm)]
           [mag (fmsqrt (fm+ (fmsqr sx) (fmsqr sy)))]
           [ori (tanθ->orient (fm/ sx sy))]
           [vec (make-flvector (* w h) -1.0)])
      (for* ([y (in-range h)]
             [x (in-range w)])
        (when (<= heigh (flomap-ref mag 0 x y))
          (trace! vec mag ori w h x y low)))
      (flomap vec 1 w h))))
