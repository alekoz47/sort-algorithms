#lang racket

;; Define test lists
;; ================================

(define L1 empty)
(define L2 (list 1 2 3))
(define L3 (list 1 3 2))
(define L4 (list 3 2 1))
(define L5 (list 3 1 2))

(define (big-list n)
  (define (big-list-acc rsf n)
    (cond ((zero? n) rsf)
          (else
           (big-list-acc (cons (random n) rsf)
                         (sub1 n)))))
  (big-list-acc empty n))

(define L6 (big-list 1000))
(define L7 (big-list 10000))

;; Define insertion-sort algorithm
;; ================================

;; (listof Number) -> (listof Number)
;; Return a sorted list of numbers
(define (sort lon)
  (cond ((empty? lon) empty)
        (else
         (insert (first lon) (sort (rest lon))))))

;; Number (listof Number) -> (listof Number)
;; Insert number in sorted place closest to beginning of list
(define (insert n lon)
  (cond ((empty? lon) (cons n empty))
        (else
         ;; If first item is greater than comparator,
         ;; place in beginning, otherwise continue searching
         (if (<= n (first lon))
             (cons n lon)
             (cons (first lon) (insert n (rest lon)))))))

;; Print test lists and results, as well as computing time
;; ================================

L1
(time (sort L1))
L2
(time (sort L2))
L3
(time (sort L3))
L4
(time (sort L4))
L5
(time (sort L5))
L6
(time (sort L6))
L7
(time (sort L7))
