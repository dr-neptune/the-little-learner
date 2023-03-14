#lang racket
(require racket plot)
(plot-new-window? #t)
#| The Lines Sleep Tonight |#


(plot3d (surface3d (λ (x y) (* (cos x) (sin y)))
                     (- pi) pi (- pi) pi)
          #:title "An R × R → R function"
          #:x-label "x" #:y-label "y" #:z-label "cos(x) sin(y)")


(define (line x)
  (λ (w b)
    (+ (* w x) b)))

;; plot the line y = 0.5x + 10
(plot (function (λ (w) ((line w) 0.5 10)))
      #:x-min 0 #:x-max 100
      #:y-min 0 #:y-max 100
      #:title "y = mx+b")

(plot (list
       (function (λ (w) ((line w) 0.5 0)) #:color "blue")
       (function (λ (w) ((line w) 1 0))  #:color "green" #:style 'dot)
       (function (λ (w) ((line w) 1.5 0))  #:color "red"))
      #:x-min 0 #:x-max 100
      #:y-min 0 #:y-max 100
      #:title "y = mx+b for different m (0.5, 1, 1.5)")


(define ((dist cx cy cz) x y z)
  (sqrt (+ (sqr (- x cx)) (sqr (- y cy)) (sqr (- z cz)))))

(plot3d (list (isosurface3d (dist  1/4 -1/4 -1/4) 0.995
                            #:color 4 #:alpha 0.8 #:samples 21)
              (isosurface3d (dist -1/4  1/4  1/4) 0.995
                            #:color 6 #:alpha 0.8 #:samples 21))
        #:x-min -1 #:x-max 1
        #:y-min -1 #:y-max 1
        #:z-min -1 #:z-max 1
        #:altitude 25)
