#lang typed/racket
(require images/flomap)
(require racket/flonum)
(require math/array)

(: flomap-grayscale (-> flomap flomap))
(define (flomap-grayscale img)
  (inline-build-flomap
   1 (flomap-width img) (flomap-height img)
   (λ (k x y i)
     (+ (* (flomap-ref img 1 x y) 0.299)
        (* (flomap-ref img 2 x y) 0.587)
        (* (flomap-ref img 3 x y) 0.114)))))

(: flomap-canny (-> flomap #:sigma Flonum #:heigh Flonum #:low Flonum flomap))
(define flomap-canny
  (λ (img #:sigma sigma #:heigh heigh #:low low)
    (let ([img
           (case (flomap-components img)
             [(4) (flomap-grayscale img)]
             [(1) img]
             [else (error "flomap-canny: invalid components")])])
      (canny img sigma heigh low))))
(provide flomap-canny)

(define-type Orient (Array (U 'horizontal 'positive-diagonal 'vertical 'negative-diagonal)))
(: tanθ->orient (-> flomap Orient))
(define (tanθ->orient fm)
  (build-array
   `#[,(flomap-height fm) ,(flomap-width fm)]
   (λ ([v : Indexes])
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

(: canny (-> flomap Flonum Flonum Flonum flomap))
(define canny
  (lambda (fm sigma heigh low)
    (let* ([w (flomap-width fm)]
           [h (flomap-height fm)]
           [b (flomap-gaussian-blur fm sigma)]
           [sx (flomap-gradient-x b)]
           [sy (flomap-gradient-y b)]
           [mag (fmsqrt (fm+ (fmsqr sx) (fmsqr sy)))]
           [ori (tanθ->orient (fm/ sx sy))]
           [vec (make-flvector (* w h) -1.0)])
      (for* ([y (in-range h)]
             [x (in-range w)])
        (when (<= heigh (flomap-ref mag 0 x y))
          (trace! vec mag ori w h x y low)))
      (flomap vec 1 w h))))
