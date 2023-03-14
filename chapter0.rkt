#lang racket
(require racket malt)

(define (dense-block n m)
  (block relu
         (list (list m n)
               (list m))))

(shape (tensor (tensor 1.0 1.1 1.2 1.3)
               (tensor 1.0 1.1 1.2 1.3)))
