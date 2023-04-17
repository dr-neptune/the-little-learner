#lang racket
(require racket plot (rename-in malt (line malt-line)))
(plot-new-window? #t)

(define (tensor-cdr t)
  (cond
    [(or (eq? 1 (tlen t))
         (scalar? t)) '()]
    [else
     (trefs t (sequence->list (in-range 1 (tlen t))))]))

(define (tensor->list t)
  (cond [(empty? t) t]
        [(scalar? t) t]
        [else
         (cons (tref t 0)
               (tensor->list (tensor-cdr t)))]))

(define (t-scatter-plot x-tensor y-tensor)
  (let* ([xs (tensor->list x-tensor)]
         [ys (tensor->list y-tensor)]
         [max-overload (λ (ls) (add1 (last (sort ls <))))])
    (plot (points (map vector xs ys))
          ;; max seems to be overloaded by malt
          #:x-min (sub1 (apply min xs))
          #:x-max (max-overload xs)
          #:y-min (sub1 (apply min ys))
          #:y-max (max-overload ys))))


(define line-xs (tensor 2.0 1.0 4.0 3.0))
(define line-ys (tensor 1.8 1.2 4.2 3.3))

(t-scatter-plot line-xs line-ys)

((malt-line line-xs) (list 0.0 0.0))

(sum (sqr (- line-ys ((malt-line line-xs) (list 0.0 0.0)))))

(define (l2-loss xs ys)
  (λ (theta)
    (let ([pred-ys ((malt-line xs) theta)])
      (sum (sqr (- ys pred-ys))))))


((l2-loss line-xs line-ys) (list 0.0 0.0))


(define l2-loss
  (λ (target)
    (λ (xs ys)
      (λ (theta)
        (let ([pred-ys ((target xs) theta)])
          (sum
           (sqr
            (- ys pred-ys))))))))

(((l2-loss malt-line) line-xs line-ys) (list 0.2 2.54))

#| The Law of Revision (Initial Version) |#
#| new theta_0 = theta_0 - (alpha * rate of change of loss w.r.t theta_0) |#
