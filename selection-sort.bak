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
(define L8 (big-list 100000))

;; Define selection-sort algorithm
;; ================================

;; (listof Number) -> (listof Number)
;; Return a sorted list of numbers
(define (selection-sort lon)
  (define (selection-sort-acc rsf lon)
    (cond ((empty? lon) rsf)
          (else
           (define minimum (smallest lon))
           (selection-sort-acc (cons minimum rsf)
                               (remove minimum lon)))))
  (selection-sort-acc empty lon))

;; (listof Number) -> Number
;; Return the smallest item from a list of numbers
(define (smallest lon)
  (define (smallest-acc rsf lon)
    (cond ((empty? lon) rsf)
          (else
           ;; If accumulator is greater than test, recurse test
           (if (> rsf (first lon))
               (smallest-acc rsf (rest lon))
               (smallest-acc (first lon) (rest lon))))))
  ;; Start with first item because comparator needed
  (smallest-acc (first lon) (rest lon)))

;; X (listof X) -> (listof X)
;; Remove a specified element from a list
(define (remove x lox)
  (define (remove-acc rsf x lox)
    (cond ((empty? lox) rsf)
          (else
           ;; If comparator is equal to first item, recurse without first item
           (if (equal? x (first lox))
               (remove-acc rsf x (rest lox))
               (remove-acc
                (cons (first lox) rsf) x (rest lox))))))
  (remove-acc empty x lox))

;; Print test lists and results, as well as computing time
;; ================================

L1
(time (selection-sort L1))
L2
(time (selection-sort L2))
L3
(time (selection-sort L3))
L4
(time (selection-sort L4))
L5
(time (selection-sort L5))
L6
(time (selection-sort L6))
L7
(time (selection-sort L7))
L8
(time (selection-sort L8))
