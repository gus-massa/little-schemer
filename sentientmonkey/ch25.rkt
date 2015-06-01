#lang racket/base

(require rackunit)
(require "../lib/mk.rkt")
(require "reasoned.rkt")
(require "ch22.rkt")

;; 2
(define (append l s)
  (cond
    [(null? l) s]
    [else (cons (car l)
                (append (cdr l) s))]))

(check-equal? (append '(a b c) '(d e)) '(a b c d e))

;; 3
(check-equal? (append '(a b c) '()) '(a b c))

;; 4
(check-equal? (append '() '(d e)) '(d e))

;; 5
; (check-equal? (append 'a '(d e))) ;; raises error

;; 6
(check-equal? (append '(d e) 'a) '(d e . a))

;; 9
(define (appendo1 l s out)
  (conde
    [(nullo l) (== s out)]
    [else
      (fresh (a d res)
        (caro l a)
        (cdro l d)
        (appendo1 d s res)
        (conso a res out))]))

;; 10
(check-run* (x)
            (appendo1 '(cake)
                      '(tastes yummy)
                      x)
            => '((cake tastes yummy)))

;; 11
(check-run* (x)
            (fresh (y)
              (appendo1
                `(cake with ice ,y)
                '(tastes yummy)
                x))
            => '((cake with ice _.0 tastes yummy)))

;; 12
(check-run* (x)
            (fresh (y)
              (appendo1
                '(cake with ice cream)
                y
                x))
            => '((cake with ice cream . _.0)))

;; 13
(check-run 1 (x)
           (fresh (y)
             (appendo1 `(cake with ice . ,y) '(d t) x))
           => '((cake with ice d t)))

;; 14
(check-run 1 (y)
           (fresh (x)
             (appendo1 `(cake with ice . ,y) '(d t) x))
           => '(()))

;; 15
(define (appendo2 l s out)
(conde
  [(nullo l) (== s out)]
  [else
    (fresh (a d res)
      (conso a d l)
      (appendo2 d s res)
      (conso a res out))]))

;; 16
(check-run 5 (x)
           (fresh (y)
             (appendo2 `(cake with ice . ,y) '(d t) x))
           => '((cake with ice d t)
                (cake with ice _.0 d t)
                (cake with ice _.0 _.1 d t)
                (cake with ice _.0 _.1 _.2 d t)
                (cake with ice _.0 _.1 _.2 _.3 d t)))

;; 17
(check-run 5 (y)
           (fresh (x)
             (appendo2 `(cake with ice . ,y) '(d t) x))
           => '(()
                (_.0)
                (_.0 _.1)
                (_.0 _.1 _.2)
                (_.0 _.1 _.2 _.3)))

;; 18
(check-equal? `(cake with ice . (_.0 _.1 _.2)) '(cake with ice _.0 _.1 _.2))

;; 19
(check-equal? (append '(cake with ice _.0 _.1 _.2) '(d t)) '(cake with ice _.0 _.1 _.2 d t))

;; 20
(check-run 5 (x)
           (fresh (y)
             (appendo2
               `(cake with ice . ,y)
               `(d t . ,y)
               x))
           => '((cake with ice d t)
                (cake with ice _.0 d t _.0)
                (cake with ice _.0 _.1 d t _.0 _.1)
                (cake with ice _.0 _.1 _.2 d t _.0 _.1 _.2)
                (cake with ice _.0 _.1 _.2 _.3 d t _.0 _.1 _.2 _.3)))

;; 21
(check-run* (x)
            (fresh (z)
              (appendo2
                '(cake with ice cream)
                `(d t . ,z)
                x))
            => '((cake with ice cream d t . _.0)))

;; 22
;; z stays fresh

;; 23
(check-run 6 (x)
           (fresh (y)
             (appendo2 x y '(cake with ice d t)))
           => '(()
                (cake)
                (cake with)
                (cake with ice)
                (cake with ice d)
                (cake with ice d t)))

;; 24
;; these are all of the beginning values of '(cake with ice d t)

;; 25
(check-run 6 (y)
           (fresh (x)
             (appendo2 x y '(cake with ice d t)))
           => '((cake with ice d t)
                (with ice d t)
                (ice d t)
                (d t)
                (t)
                ()))

;; 26
;; all of the suffixes of '(cake with ice d t)

;; 27
(check-run 6 (r)
           (fresh (x y)
             (appendo2 x y '(cake with ice d t))
             (== `(,x ,y) r))
           => '((() (cake with ice d t))
                ((cake) (with ice d t))
                ((cake with) (ice d t))
                ((cake with ice) (d t))
                ((cake with ice d) (t))
                ((cake with ice d t) ())))

;; 28
;; each answer is '(cake with ice d t) split into two lists

;; 29
;; (check-run 7 (r)
;;            (fresh (x y)
;;              (appendo2 x y '(cake with ice d t))
;;              (== `(,x ,y) r))
;;            => ??)
;; no answer

;; 30
;; that would be nice!

;; 31
;; make the recursive call last
(define (appendo3 l s out)
  (conde
    [(nullo l) (== s out)]
    [else
      (fresh (a d res)
        (conso a d l)
        (conso a res out)
        (appendo3 d s res))]))


;; 32
(check-run 7 (r)
           (fresh (x y)
             (appendo3 x y '(cake with ice d t))
             (== `(,x ,y) r))
           => '((() (cake with ice d t))
                ((cake) (with ice d t))
                ((cake with) (ice d t))
                ((cake with ice) (d t))
                ((cake with ice d) (t))
                ((cake with ice d t) ())))

;; 33
(check-run 7 (x)
           (fresh (y z)
             (appendo3 x y z))
           => '(()
                (_.0)
                (_.0 _.1)
                (_.0 _.1 _.2)
                (_.0 _.1 _.2 _.3)
                (_.0 _.1 _.2 _.3 _.4)
                (_.0 _.1 _.2 _.3 _.4 _.5)))

;; 34
(check-run 7 (y)
           (fresh (x z)
             (appendo3 x y z))
           => '(_.0 _.0 _.0 _.0 _.0 _.0 _.0))

;; 35
;; res is passed into appendo3 recursively. On each recursion res is either null, and we s as our result or it continues to recurse.

;; 36
(check-run 7 (z)
           (fresh (x y)
             (appendo3 x y z))
           => '(_.0
                (_.0 . _.1)
                (_.0 _.1 . _.2)
                (_.0 _.1 _.2 . _.3)
                (_.0 _.1 _.2 _.3 . _.4)
                (_.0 _.1 _.2 _.3 _.4 . _.5)
                (_.0 _.1 _.2 _.3 _.4 _.5 . _.6)))

;; 37
(check-run 7 (r)
           (fresh (x y z)
             (appendo3 x y z)
             (== `(,x ,y ,z) r))
           => '((() _.0 _.0)
                ((_.0) _.1 (_.0 . _.1))
                ((_.0 _.1) _.2 (_.0 _.1 . _.2))
                ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
                ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))
                ((_.0 _.1 _.2 _.3 _.4) _.5 (_.0 _.1 _.2 _.3 _.4 . _.5))
                ((_.0 _.1 _.2 _.3 _.4 _.5) _.6 (_.0 _.1 _.2 _.3 _.4 _.5 . _.6))))

;; 38
(define (swappendo l s out)
  (conde
    [s#
      (fresh (a d res)
        (conso a d l)
        (conso a res out)
        (swappendo d s res))]
    [else (nullo l) (== s out)]))

;; 39
;; (run 1 (z)
;;   (fresh (x y)
;;     (swappendo x y z)))
;; no answer

;; 40
;; tl;dr: we didn't terminate our recursion early
;; vlonger answer: this switches out search to be depth vs. breadth first search.

;; TODO try lambda-limited


;; 41
(define (unwrap x)
  (cond
    [(pair? x) (unwrap (car x))]
    [else x]))

(check-equal? (unwrap '((((pizza))))) 'pizza)

;; 42
(check-equal? (unwrap '((((pizza pie) with)) extra cheese)) 'pizza)

;; 43
;; will try to keep going for now...

;; 45
(define (unwrapo x out)
  (conde
    [(pairo x)
     (fresh (a)
       (caro x a)
       (unwrapo a out))]
    [else (== x out)]))

;; 46
(check-run* (x)
            (unwrapo '(((pizza))) x)
            => '(pizza
                 (pizza)
                 ((pizza))
                 (((pizza)))))

;; 47
;; those are all other valid options since we didn't eliminate them by unifying with failure

;; 48
;; (run 1 (x)
;;   (unwrapo x 'pizza))

;; no answer

;; 49
;; (run 1 (x)
;;   (unwrapo `(,x) 'pizza))

;; still no answer

;; 50
;; the recursive call happens before the else call, so it just keeps on going...

;; 51
;; let's fix it.

;; 52
(define (unwrapo2 x out)
  (conde
    [s# (== x out)]
    [else
      (fresh (a)
        (caro x a)
        (unwrapo2 a out))]))

;; 53
(check-run 5 (x)
           (unwrapo2 x 'pizza)
           => '(pizza
                 (pizza . _.0)
                 ((pizza . _.0) .  _.1)
                 (((pizza . _.0) . _.1) . _.2)
                 ((((pizza . _.0) . _.1) . _.2) . _.3)))

;; 54
(check-run 5 (x)
           (unwrapo2 x '((pizza)))
           => '(((pizza))
                 (((pizza)) . _.0)
                 ((((pizza)) . _.0) .  _.1)
                 (((((pizza)) . _.0) . _.1) . _.2)
                 ((((((pizza)) . _.0) . _.1) . _.2) . _.3)))

;; 55
(check-run 5 (x)
           (unwrapo2 `((,x))'pizza)
           => '(pizza
                 (pizza . _.0)
                 ((pizza . _.0) .  _.1)
                 (((pizza . _.0) . _.1) . _.2)
                 ((((pizza . _.0) . _.1) . _.2) . _.3)))


;; 56
;; Yeah, I do need a break after that. Time for some steaks.