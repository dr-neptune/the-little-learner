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
(((l2-loss malt-line) line-xs line-ys) (list 1.0 0.0))
(((l2-loss malt-line) line-xs line-ys) (list 0.6623 0.0))

#| The Law of Revision (Initial Version) |#
#| new theta_0 = theta_0 - (alpha * rate of change of loss w.r.t theta_0) |#

#| Chapter 4 |#


(∇ (λ (theta) (sqr (first theta)) (list 27.0)) (list 54.0))

(define obj ((l2-loss malt-line) line-xs line-ys))

(∇ obj (list 0.0 0.0))

(define revise
  (λ (f revs theta)
    (cond [(zero? revs) theta]
          [else
           (revise f (sub1 revs) (f theta))])))


(revise (λ (theta)
          (map (λ (p) (- p 3)) theta))
        5
        (list 1 2 3))

(map (λ (x y)
       (+ x y))
     (list 12 17 32)
     (list 8 3 11))

(define nabla ∇)
(nabla obj (list 0.0 0.0))

(let ((alpha 0.01))
  (let ((f (λ (theta)
             (let ((gs (nabla obj theta)))
               (map (λ (p g) (- p (* alpha g))) theta gs)))))
    (revise f 1000 (list 0.0 0.0))))

(define alpha 0.01)
(define revs 1000)

(let ((f (λ (theta)
           (let ((gs (nabla obj theta)))
             (map (λ (p g) (- p (* alpha g))) theta gs)))))
  (revise f revs (list 0.0 0.0)))

(define gradient-descent
  (λ (obj theta)
    (let ((f (λ (big-theta)
               (map (λ (p g)
                      (- p (* alpha g)))
                    big-theta
                    (nabla obj big-theta)))))
      (revise f revs theta))))

#| Interlude II: Too Many Toys Make Us Hyperactive |#
(declare-hyper smaller)
(declare-hyper larger)

(with-hypers
  ((smaller 1)
   (larger 2000))
  (+ smaller larger))

smaller

#| Chapter 5: Target Practice |#
(require malt)
(require malt/set-impl)
(set-impl 'learner)

(declare-hyper revs)
(declare-hyper alpha)

(define gradient-descent
  (λ (obj theta)
    (let ((f (λ (big-theta)
               (map (λ (p g)
                      (- p (* alpha g)))
                    big-theta
                    (nabla obj big-theta)))))
      (revise f revs theta))))

(define gradient-descent
  (λ (obj theta)
    (let ((f (λ (big-theta)
               (map (λ (p g)
                      (- p (* alpha g)))
                    big-theta
                    (nabla obj big-theta)))))
      (revise f revs theta))))

(define l2-loss
  (λ (target)
    (λ (xs ys)
      (λ (theta)
        (let ((pred-ys ((target xs) theta)))
          (sum (sqr (- ys pred-ys))))))))

(with-hypers ((revs 1000)
              (alpha 0.01))
  (gradient-descent
   ((l2-loss malt-line) line-xs line-ys)
   (list 0.0 0.0)))

(define quad-xs (tensor -1.0 0.0 1.0 2.0 3.0))
(define quad-ys (tensor 2.55 2.1 4.35 10.2 18.25))

(define quad
  (λ (t)
    (λ (theta)
      (+ (* (first theta) (sqr t))
         (+ (* (second theta) t) (third theta))))))

((quad 3.0)
 (list 4.5 2.1 7.8))

(with-hypers ((revs 1000)
              (alpha 0.001))
  (gradient-descent
   ((l2-loss quad) quad-xs quad-ys)
   (list 0.0 0.0 0.0)))

(define plane-xs
  (tensor (tensor 1.0 2.05)
          (tensor 1.0 3.0)
          (tensor 2.0 2.0)
          (tensor 2.0 3.91)
          (tensor 3.0 6.13)
          (tensor 4.0 8.09)))

(define plane-ys
  (tensor
   13.99
   15.99
   18.0
   22.4
   30.2
   37.94))

(define plane
  (λ (t)
    (λ (theta)
      (+ (dot-prod (first theta) t) (second theta)))))

(define dot-prod
  (λ (w t)
    (sum (* w t))))

(dot-prod (tensor 2.0 1.0 7.0) (tensor 8.0 4.0 3.0))

(with-hypers
  ((revs 1000)
   (alpha 0.001))
  (gradient-descent
   ((l2-loss plane) plane-xs plane-ys)
   (list (tensor 0.0 0.0) 0.0)))

#| Interlude III: The Shape of Things to Come |#

(shape (tensor (tensor 2 4 5)
               (tensor 6 7 9)))

#| Chapter 6. An Apple A Day |#

(define samples
  (λ (n s)
    (sampled n s (list))))

(define sampled
  (λ (n i a)
    (cond [(zero? i) a]
          [else
           (sampled n (sub1 i)
                    (cons (random n) a))])))

(samples 20 3)

(declare-hyper batch-size)

(define sampling-obj
  (λ (expectant xs ys)
    (let ((n (tlen xs)))
      (λ (theta)
        (let ((b (samples n batch-size)))
          ((expectant (trefs xs b) (trefs ys b))
           theta))))))

(with-hypers ((revs 1000)
              (alpha 0.01)
              (batch-size 4))
  (gradient-descent
   (sampling-obj
    (l2-loss malt-line) line-xs line-ys)
   (list 0.0 0.0)))

(with-hypers ((revs 15000)
              (alpha 0.001)
              (batch-size 4))
  (gradient-descent
   (sampling-obj
    (l2-loss plane) plane-xs plane-ys)
   (list (tensor 0.0 0.0) 0.0)))

#| Chapter 7. The Crazy "ates" |#
