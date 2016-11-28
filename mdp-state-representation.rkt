#lang racket

;; needed libraries
(require 
         "rsound-composer/define-argcheck.rkt"
         "rsound-composer/composer.rkt"
         "racket-avl-modified.rkt")

(provide (all-defined-out)
         (all-from-out "rsound-composer/composer.rkt"))


;; state representation that contains a chord voicing, harmonic progression
(struct state [voicing-prog harmonic-progression value]
    #:guard
      (lambda (cv hp sv name)
        (if (and
             (list? cv) ; list of harmonies
             (list? hp) ; list of symbols
             (number? sv)) ; numeric state value
             (values cv hp sv)
            (error "Invalid state parameters"))))

(define (voicing-compare v1 v2)
  (define 
    relationship-list
    (for/list ([n1 (reverse v1)]
               [n2 (reverse v2)])
      (cond ((= (note-midi-number n1) (note-midi-number n2)) 0)
            ((< (note-midi-number n1) (note-midi-number n2)) -1)
            (else 1))))
  (define (v-comp-helper rel-list)
    (cond ((null? rel-list) 0)         ;; voicings are equal
          ((eq? (car rel-list) -1) -1) ;; v1 < v2 
          ((eq? (car rel-list) 1) 1)   ;; v1 > v2
          (else                        ;; equal so far, check next
            (v-comp-helper (cdr rel-list)))))
  (v-comp-helper relationship-list))

(define (state<=? s1 s2)
  (define (s<=helper vprog1 vprog2)
    (cond ((null? vprog1) #t)
          ((null? vprog2) #f)
          ((eq? (voicing-compare (car vprog1) (car vprog2)) -1) #t)
          ((eq? (voicing-compare (car vprog1) (car vprog2)) 1) #f)

          ;; Equal so far, check next
          ((eq? (voicing-compare (car vprog1) (car vprog2)) 0)
           (s<=helper (cdr vprog1) (cdr vprog2)))
          (else "<#procedure:state<=?")))
  (s<=helper (state-voicing-prog s1) (state-voicing-prog s2)))

(define (state=? s1 s2)
  (define (s=helper vprog1 vprog2)
    (cond ((and (null? vprog1)  (null? vprog2) #t))
          ((null? vprog1) #f)
          ((null? vprog2) #f)
          ;; Equal so far, check next
          ((eq? (voicing-compare (car vprog1) (car vprog2)) 0)
           (s=helper (cdr vprog1) (cdr vprog2)))
          (else #f)))
  (s=helper (state-voicing-prog s1) (state-voicing-prog s2)))






;; create a note without duration (pitch)
(define (pitch letter octave)
  (note letter octave null-beat))

;; upper and lower bounds for acceptable notes in a voice
(struct part-range [lower-bound upper-bound]
  #:guard
    (lambda (lb ub name)
      (if (and
            (note? lb)
            (note? ub))
          (values lb ub)
          (error "Invalid part-range parameters"))))
           

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

(define (policy-match pol st)
  (avl-find (policy-state-mapping pol) (cons st 0)))

(define test-policy
  (policy (make-custom-avl (lambda (x y) (state<=? (car x) (car y))) (lambda (x y) (state=? (car x) (car y))))))

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

;(define (policy-get-action pol current-state)
;  (let ([

(policy-add-mapping test-policy empty-state state-one)
(policy-contains? test-policy empty-state)

;; dummy function
(define (schenkerian-analysis current-state) 1)

;; pick a next state based on policy
;; update current state value based on analysis function reward/penalty
;; return the next state
;; dummy function
(define (state-transition current-state
                          adjacent-states
                          policy
                          analysis-function) 1)

;; example of possible voice part ranges
(define example-part-range-list
  (list
    (part-range (pitch 'C 4) (pitch 'C 6)) ;; Soprano
    (part-range (pitch 'F 3) (pitch 'F 5)) ;; Alto
    (part-range (pitch 'C 3) (pitch 'C 5)) ;; Tenor
    (part-range (pitch 'E 2) (pitch 'E 4)))) ;; Bass 

;; turns part range into a list of appropriate notes
(define (enumerate-part-range pr)
  (for/list ([i (in-range 
                  (note-midi-number (part-range-lower-bound pr))
                  (+ (note-midi-number (part-range-upper-bound pr)) 1))])
    (make-note-from-midi-num i null-beat)))

;; example of C major harmony
(define  cmaj 
  (harmony
    (whole-note 'C 5)
    (whole-note 'E 5)
    (whole-note 'G 5)
    (whole-note 'C 5)))

;; takes a part range and returns a list of playable notes in harmony
(define (part-range-valid-pitches pr harmony)
  (let ([pr-all-pitches (enumerate-part-range pr)])
    (filter 
      (lambda (x) 
        (ormap (lambda (y)
                 (note-pitch-class-enharm-eq? x y))
               (harmony-notes harmony)))
      pr-all-pitches)))
    

;; populates the state space, which is a list of cartesian products of valid
;; pitches for each part range
;; takes a recursive approach in populating state space.  Takes first chord in state space and return all possible chord voicings.
(define (populate-state-space progression
                              key
                              list-of-part-ranges)
 (define (populate-helper sub-progression
                           key
                           list-of-part-ranges
                           list-of-voicing-groups)
    (if (null? sub-progression)
        list-of-voicing-groups
        (let* ([current-harmony
                (functional-harmony
                 key
                 (car sub-progression)
                 null-beat)]
               [parts-valid-note-list
                (map
                  (lambda (pr)
                    (part-range-valid-pitches pr current-harmony))
                  list-of-part-ranges)]
               [voicing-group

                 ;; To reduce the state space, we prune out obvious invalid 
                 ;; voicings
                 (filter
                   (lambda (voicing)
                     (let ([voicing-pitch-classes 
                             (map note-pitch-class voicing)]
                           [harmony-pitch-classes
                             (map note-pitch-class 
                               (harmony-notes current-harmony))])
                     (and
                       (andmap
                         (lambda (x) x)
                         (for/list ([n harmony-pitch-classes])
                           (if (member n voicing-pitch-classes) #t #f)))
                       (let ([voicing-midi-num (map note-midi-number voicing)])
                         (equal? voicing-midi-num (sort voicing-midi-num >=)))
                       )))


                       
                  (apply cartesian-product parts-valid-note-list))])
          (populate-helper (cdr sub-progression) key list-of-part-ranges (cons voicing-group list-of-voicing-groups)))))
  (populate-helper (reverse chord-progression) key list-of-part-ranges '()))

;; creates a sample state space over I, IV, V, I progression in C major
#;(define example-state-space
  (populate-state-space chord-progression
                        (pitch 'C 5)
                        example-part-range-list))


;; prints state space
(define (print-state-space stsp)
 (void 
   (map
     (lambda (voicing-group)
       (map
        (lambda (voicing)
          (map (lambda (n)
                 (display "    ")
                (display (note->list n)) (newline))
              voicing)
          (newline))
       voicing-group))
  stsp)
   (display "Total State Space Size: ")
   (display (length (flatten stsp)))
   (newline)))

;;(define test-avl
 ;; (make-avl state-<=?))

;(print-state-space example-state-space)
