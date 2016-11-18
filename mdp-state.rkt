#lang racket

(require "define-argcheck.rkt"
         "composer/composer.rkt")


(struct state [list-of-harmonies harmonic-progression state-value]
    #:guard
      (lambda (loh hp sv name)
        (if (and
             (list? loh) ; list of harmonies
             (list? hp) ; list of symbols
             (number? sv)) ; numeric state value
             (values loh hp sv)
            (error "Invalid state parameters"))))

(struct part-range [lower-bound upper-bound]
  #:guard
    (lambda (lb ub name)
      (if (and
           (pair? lb)
           (pair? ub)
           (symbol? (car lb))
           (symbol? (car ub))
           (integer? (cdr lb))
           (integer? (cdr ub)))
          (values lb ub)
          (error "Invalid part-range parameters"))))
           

;; a mapping of current states to next states
;; implement via pattern matching
(struct policy [stuff])

(define (schenkerian-analysis current-state) 1)

;; pick a next state based on policy
;; update current state value based on analysis function reward/penalty
;; return the next state
(define (state-transition current-state
                          adjacent-states
                          policy
                          analysis-function) 1)

(define chord-progression
  (list 'I 'IV 'V 'I))

(define (enumerate-part-range pr)
  (

(define (part-range-valid-notes pr harmony)
    

;; comment
(define (populate-state-space progression
                              key
                              list-of-part-ranges)
  (define (populate-helper progression
                           key
                           list-of-part-ranges
                           list-of-states)
    (if (null? progression)
        list-of-states
        (let* ([current-harmony (car progression)]
               [



