#lang racket
(require racket malt)

(tensor 5.0 7.18 3.14)

(tlen (tensor
 (tensor 7 6 5 2)
 (tensor 3 8 6 9)
 (tensor 9 4 8 5)))

(tlen (tensor 17 12 91 67))

(tlen (tensor (tensor 3 2 8) (tensor 7 1 9)))

(define (repeat n f)
  (match n
    [1 f]
    [_ (compose f (repeat (sub1 n) f))]))

((repeat 5 add1) 6)

((repeat 4 tensor) 8)

(tensor
 (map tensor
      (map tensor '(5 6 7))
      (map tensor '(8 9 0))))

(define (rank t)
  (cond [(scalar? t) 0]
        [else
         (add1 (rank (tref t 0)))]))

(rank ((repeat 5 tensor) 8))

(rank
 (tensor
  (list->tensor (map tensor '(8 9)))
  (list->tensor (map tensor '(4 7)))))


(define xtens
  (tensor
   (list->tensor (map tensor '(5 6 8)))
   (list->tensor (map tensor '(7 9 5)))))

(define (tensor-shape t)
  (cond [(scalar? t) '()]
        [else
         (cons (tlen t)
               (tensor-shape (tref t 0)))]))

(tensor-shape xtens)

(tensor-shape (tensor 9 4 7 8 0 1))

(define (rank t)
  (ranked t 0))

(define (ranked t a)
  (cond [(scalar? t) a]
        [else
         (ranked (tref t 0) (add1 a))]))

(rank xtens)
