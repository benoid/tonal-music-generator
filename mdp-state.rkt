#lang racket

(require "define-argcheck.rkt"
         "composer/composer.rkt")


(struct state [list-of-instrument-parts harmonic-progression state-value]
    #:guard
      (lambda (loip hp sv name)
        (if (and
             (list? loip) ; list of instrument parts
             (list? hp) ; list of symbols
             (number? sv)) ; numeric state value
             (values loip hp sv)
            (error "Invalid state parameters"))))

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

(define progression
  (list 'I 'IV 'V 'I))

;; comment
(define (populate-state-space progression
                              key
                              num-voices)
  