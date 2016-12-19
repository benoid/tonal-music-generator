#lang racket

(require "mdp-state.rkt"
	 "mdp-reward.rkt"
	 "rsound-composer/composer.rkt")

(provide (all-defined-out)
	 (all-from-out 
	   "mdp-state.rkt"
	   "mdp-reward.rkt"))

(define (value-iteration state-table rec-depth)
  (if (<= rec-depth 0)
    state-table
    (let* ([min-length (apply min (map length state-table))]
	   [sub-state-space (map (lambda (state-group)
				   (sort 
				     (take state-group min-length)
				     (lambda (x y)
				       (< (state-value x) (state-value y))))
				   ) 
				 state-table)]
	   [iteration-table
	     (map* list sub-state-space)]

	   [updated-state-space
	     (map* list
		   (map reward-function iteration-table))])

      (value-iteration updated-state-space (- rec-depth 1)))))

;(print-state-space (value-iteration example-state-space))

(define cm (convolution-matrix (car (value-iteration example-state-space 1))
			       1))

(define (display-convolution-matrix m)
  (map (lambda (lst)
	 (map (lambda (n)
		(display (cons (note-letter n) (note-octave n)))
		(newline))
	      lst)
	 (newline))
       m))

(define (best-path state-space)
  (map first state-space))

(define (play-markov-path path-list)
  (play-instrument-part #:tempo 100
			(instrument-part 
			  [vgame-synth-instrument 4]
			  (apply measure
				 (map 
				   (lambda (st)
				     (apply harmony
					    (map (lambda (n)
						   (quarter-note (note-letter n) (note-octave n)))
						 (state-voicing st))))
				   path-list)))))

(define (export-markov-path path-list filename)
  (rs-write
    (instrument-part->rsound #:tempo 100
			     (instrument-part 
			       [vgame-synth-instrument 4]
			       (apply measure
				      (map 
					(lambda (st)
					  (apply harmony
						 (map (lambda (n)
							(quarter-note (note-letter n) (note-octave n)))
						      (state-voicing st))))
					path-list))))
    filename))

(define progression1 (list 'I 'IV 'V 'I))
(define progression2 (list 'i 'bVI 'iv 'ii-dim 'V 'i))
(define progression3 (list 'I 'V 'I))
(define progression4 (list 'I 'bVII 'bIII 'ii-dim 'V 'I))
(define progression5 (list 'I 'IV 'I 'V 'vi 'IV 'V 'I))

(define (progression->state-space progression)
	  (map 
	    shuffle
	    (populate-state-space progression
				  (eighth-note 'C 5)
				  example-part-range-list)))

(define state-space1
	  (progression->state-space progression1))
(define state-space2
	  (progression->state-space progression2))
(define state-space3
	  (progression->state-space progression1))
(define state-space4
	  (progression->state-space progression4))
(define state-space5
	  (progression->state-space progression5))



(define (run-markov-system-trial key progression)
  (let ([state-space 
	  (map 
	    shuffle
	    (populate-state-space progression
				  key
				  example-part-range-list))])
    (display "Running Markov System Trial\n\n")
    (display "Chord Progression: ")
    (display progression)
    (newline)
    (newline)
    (display "Best state path after 0 value iterations: \n")
    (map display-state (best-path (value-iteration state-space 0)))
    (newline)
    (newline)
    (display "Best state path after 10 value iterations: \n")
    (map display-state (best-path (value-iteration state-space 10)))
    (newline)
    (newline)
    (display "Best state path after 100 value iterations: \n")
    (map display-state (best-path (value-iteration state-space 100)))
    (newline)
    (newline)
    (display "Best state path after 1000 value iterations: \n")
    (map display-state (best-path (value-iteration state-space 1000)))
    (newline)
    (newline)))
;(run-markov-system-trial (eighth-note 'C 5) progression5)

