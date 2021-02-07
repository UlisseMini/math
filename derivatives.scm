;; shamelessly stolen from https://youtu.be/bV87UzKMRtE (amazing lecture)
;; I love how he first writes deriv with nonexistant predicates, then writes the predicates.

(define (constant? exp var)
  (and (atom? exp)
       (not (eq? exp var))))

(define (same-var? exp var)
  (and (atom? exp)
       (eq? exp var)))

(define (sum? exp)
  (and (not (atom? exp))
       (eq? (car exp) '+)))

(define (product? exp)
  (and (not (atom? exp))
       (eq? (car exp) '*)))

(define (make-sum a1 a2)
  (cond
    ((and (number? a1) (number? a2)) (+ a1 a2))
    ((and (number? a1) (= a1 0)) a2)
    ((and (number? a2) (= a2 0)) a1)
    (else (list '+ a1 a2))))

(define (make-product a1 a2)
  (cond
    ((or (eq? a1 0) (eq? a2 0)) 0)
    ((and (number? a1) (number? a2) (* a1 a2)))
    ((eq? a1 1) a2)
    ((eq? a2 1) a1)
    (else (list '* a1 a2))))

(define a1 cadr)
(define a2 caddr)


;; critically, notice how the deriv procedure
;; does not depend on the representation! you can change the representation!
;; for example, you can change make-sum and make-product to simplify basic things

(define (deriv exp var)
  (cond ((constant? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp) (make-sum (deriv (a1 exp) var)
                              (deriv (a2 exp) var)))
        ((product? exp) (make-sum
                          (make-product (a1 exp) (deriv (a2 exp) var))
                          (make-product (a2 exp) (deriv (a1 exp) var))))))

