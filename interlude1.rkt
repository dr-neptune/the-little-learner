#lang racket
(require racket malt)

#|
This interlude is about how it is possible to extend functions that
operate on fixed ranks of tensors into functions that also accept
tensors of different ranks. We use the concept of descending into the
higher-ranked tensors to accomplish this.
|#


(+ (tensor 2)
   (tensor 7))

(+ (tensor 5 6 7)
   (tensor 2 0 1))

(+ (tensor
    (tensor 4 6 7)
    (tensor 2 0 1))
   (tensor
    (tensor 1 2 2)
    (tensor 6 3 1)))

(+ 4 (tensor 3 6 5))

;; ((10 12 9) (13 13 8))
(+ (tensor 6 9 1)
   (tensor
    (tensor 4 3 8)
    (tensor 7 4 7)))

(* (tensor
    (tensor 4 6 5)
    (tensor 6 9 7))
   3)

(sqrt 9)
(sqrt (tensor 9 16 25))

(sum (tensor 10.0 12.0 14.0))

(define (sum1 t)
  (summed t (sub1 (tlen t)) 0.0))

(define (summed t i a)
  (cond [(zero? i) (+ (tref t 0) a)]
        [else
         (summed t (sub1 i) (+ (tref t i) a))]))

(sum1 (tensor 1 2 3 4 5))

(rank
 (sum (tensor
       (tensor
        (tensor 1 2)
        (tensor 3 4))
       (tensor
        (tensor 5 6)
        (tensor 7 8)))))

((line (tensor 2 7 5 11)) (list 4 6))
