#lang racket

;; todo
;;  - melodic analysis
;;     - reward for stepwise motion
;;     - penalty for unresolved resolved skips
;;     - severe penalty unless soprano ends on tonic
;;     - severe penalty unless bass begins and ends on tonic
;;     - severe penalty unless soprano ends with stepwise motion
;;     - severe penalty unless bass ends with V I motion
;;     - moderate penalty for second inversion triads
;;  - contrapuntal analysis
;;     - severe penalty for parallel perfect intervals
;;     - severe penalty for similar perfect intervals between soprano and bass
;;     - reward reward for contrapuntal devices (eg.  tenths) 
(require "mdp-state.rkt")
(provide (all-defined-out)
         (all-from-out "mdp-state.rkt"))

(define (convolution-matrix state-prog root-index)
  (let ([first-voicing
          (cond [(eq? root-index 0) (state '() '() 0)]
                [(< root-index (length state-prog)) 
                 (list-ref state-prog (- root-index 1))]
                [else (error "convolution matrix: invalid first voicing")])]
        [second-voicing 
          (cond [(< root-index (length state-prog))
                 (list-ref state-prog root-index)]
                [else (error "convolution matrix: invalid second voicing")])]
        [third-voicing
          (cond [(>= root-index (- (length state-prog) 1)) 
                 (state '() '() 0)]
                [else 
                  (list-ref state-prog (+ root-index 1))])])
    (list 
      (state-voicing first-voicing)
      (state-voicing second-voicing)
      (state-voicing third-voicing))))

(define (map* lmbda list-of-lists)
  (call-with-values 
    (lambda ()
      (apply values
             (cons lmbda list-of-lists)))
    map))

(define (mostly-stepwise? conv-matrix)
  (<= 
    (foldl + 0
           (map*             
             (lambda (first-note second-note third-note)
               (let (
                     [first-interval 
                       (if (< (abs (- (note-midi-number first-note) 
                                      (note-midi-number second-note))) 5)
                         0
                         1)]
                     [second-interval 
                       (if (< (abs (- (note-midi-number first-note) 
                                      (note-midi-number second-note))) 5)
                         0
                         1)])
                 (+ first-interval second-interval)))
             conv-matrix))
    1))


(define (unresolved-skips? conv-matrix)
  (andmap (lambda (x) x)
          (map*             
            (lambda (first-note second-note third-note)
              (let (
                    [first-interval 
                      (- (note-midi-number first-note) 
                         (note-midi-number second-note))]
                    [second-interval 
                      (- (note-midi-number second-note) 
                         (note-midi-number third-note))])
                (cond 
                  ((or (> (abs first-interval) 6)
                       (> (abs second-interval) 6))
                   #f)
                  ((and (> first-interval 4)
                        (not
                          (and (< second-interval 0)
                               (> second-interval -3))))
                   #f)
                  ((and (< first-interval -4)
                        (not
                          (and (> second-interval 0)
                               (< second-interval 3))))
                   #f)
                  (else #t))

                ))
            conv-matrix)))

(define (second-inversion-triads-correct? conv-matrix) 0)
(define (parallel-perfect-intervals? conv-matrix) 0)
(define (similar-perfect-intervals? conv-matrix) 0)
(define (parallel-tenths? conv-matrix) 0)
(define (voice-exchange? conv-matrix?) 0)
(define (doubled-leading-tone? conv-matrix) 0)

;; end stuff
(define (soprano-proper-cadence? conv-matrix)
  (andmap (lambda (x) x)
          (map*             
            (lambda (first-note second-note)
              (let (
                    [first-interval 
                      (- (note-midi-number first-note) 
                         (note-midi-number second-note))])
                (cond 
                  ((and (> (abs first-interval) 0)
                        (< (abs first-interval) 3))
                   #t)
                  (else #t))
                ))
           (take conv-matrix 2))))

(define (bass-begins-on-tonic? conv-matrix)
          (let* ([first-chord (second conv-matrix)]
                 [relationships
                   (cartesian-product (list (last first-chord)) (take first-chord 3))]
                 [bass-intervals
                   (map (lambda (rel)
                          (- (note-midi-number (second rel))
                             (note-midi-number (first rel)))
                          )
                        relationships)])
              #|(newline)
              (newline)
              (display "chord:\n ")
              (map (lambda (n) (display (note->list n)) (newline)) first-chord)
              (display "relationships: \n")
              (map (lambda (rel)
                     (display (note->list (first rel)))
                     (newline)
                     (display (note->list (second rel)))
                     (newline)) relationships)
              (newline)
              (display "bass-intervals: ")
              (display bass-intervals)
              (newline)
              (display (ormap (lambda (x) (eq? (remainder x 12) 7)) bass-intervals))
              (newline)
              (display "===============") |#

              (ormap (lambda (x) (eq? (remainder x 12) 7)) bass-intervals)

            ))


(define (bass-proper-cadence? conv-matrix)
          (let* ([last-chord (second conv-matrix)]
                 [relationships
                   (cartesian-product (list (last last-chord)) (take last-chord 3))]
                 [bass-intervals
                   (map (lambda (rel)
                          (- (note-midi-number (second rel))
                             (note-midi-number (first rel)))
                          )
                        relationships)])
              (ormap (lambda (x) (eq? (remainder x 12) 7)) bass-intervals)
            ))

(define (reward-function state-prog)
  (for/list ([i (in-range 0 (length state-prog))]
             [current-state state-prog])
    (let* ([matrix (convolution-matrix state-prog i)])
      (cond ((= i 0)
             (let ([bass-begins-on-tonic 
                     (if (bass-begins-on-tonic? matrix) 0 1000)])
                 (state (state-voicing current-state)
                        (state-harmonic-progression current-state)
                        (+ (state-value current-state)
                           bass-begins-on-tonic))))
             ((= i (- (length state-prog) 1)) 
              (let ([soprano-cadence
                      (if (soprano-proper-cadence? matrix) 0 1000)]
                    [bass-cadence
                      (if (bass-proper-cadence? matrix) 0 1000)]
                    )
                 (state (state-voicing current-state)
                        (state-harmonic-progression current-state)
                        (+ (state-value current-state)
                           bass-cadence
                           soprano-cadence))))
             (else
               (let ([mostly-stepwise (if (mostly-stepwise? matrix) 0 5)]
                     [unresolved-skips (if (unresolved-skips? matrix) 10 0)])
                 (state (state-voicing current-state)
                        (state-harmonic-progression current-state)
                        (+ (state-value current-state)
                           mostly-stepwise
                           unresolved-skips))))))))


