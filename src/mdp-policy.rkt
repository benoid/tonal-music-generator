;; NOTE: This file is no longer used in the project.  It will be removed.

#lang racket

(require "mdp-state.rkt"
         "racket-avl-modified.rkt")

;; a mapping of current states to next states
;; implement via pattern matching
(struct policy [state-mapping]
  #:guard
  (lambda (stm type-name)
    (if (avl? stm)
      stm
      (error "Invalid policy parameter"))))

(define (policy-add-mapping pol
                            current-state
                            next-state)
  (avl-add! (policy-state-mapping pol)
            (cons current-state next-state)))

(define (policy-contains? pol
                          st)
  (avl-contains? (policy-state-mapping pol) (cons st 0)))

(define (policy-match pol
                      st)
  (avl-find (policy-state-mapping pol) (cons st 0)))

(define (policy-request-action pol
                               st
                               voicing-enumeration)
  (if (not (policy-contains? pol st))
      (policy-add-mapping pol st 
                          (state
                            (append 
                              (state-voicing-prog st) 
                              (list-ref voicing-enumeration 
                                        (length (state-voicing-prog st))))
                                  (state-harmonic-progression st)
                                  0))
      (cdr (policy-match pol st))))

(define test-policy
  (policy (make-custom-avl (lambda (x y) (state<=? (car x) (car y))) 
                           (lambda (x y) (state=? (car x) (car y))))))

;; roman numerals represent functional harmony
(define chord-progression
  (list 'I 'IV 'V 'I))

(define empty-state
  (state '() chord-progression 0))

(define state-one
  (state (list
           (list
               (note 'C 4 null-beat)
               (note 'G 3 null-beat)
               (note 'C 3 null-beat)
               (note 'E 2 null-beat)))
         chord-progression
         0))

(define state-two
  (state (list 
           (list
               (note 'C 4 null-beat)
               (note 'G 3 null-beat)
               (note 'C 3 null-beat)
               (note 'E 2 null-beat))
           (list 
               (note 'A 4 null-beat)
               (note 'F 3 null-beat)
               (note 'C 3 null-beat)
               (note 'A 2 null-beat)))
         chord-progression
         0))

(policy-add-mapping test-policy empty-state state-one)
;(policy-contains? test-policy empty-state)



;; The following line should return true
(state=?
  (policy-request-action test-policy
                         empty-state
                         example-voicing-enumeration)
  state-one)
