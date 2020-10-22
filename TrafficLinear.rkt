#lang racket

; Traffic Light simulation with linear state sequence.

; Run through the sequence of states for a single traffic light;
; time delay between each state transition.
 
; Run the state sequence once: 
; (runIt stateSequence)

; Or run forever
; (runIt_continuous stateSequence)

; Linear sequence of states: List(Num)

(define stateSequence (list 1 2 3 4) )


; Output function: Num -> Void - produce output for a given state number
(define outputState
  (λ (s)
   (println 
    (cond
      ((equal? s 1) "Red")
      ((equal? s 2) "Red+Amber")
      ((equal? s 3) "Green")
      ((equal? s 4) "Amber")
      )
    )
    )
  )

; Run a state sequence. Recursively process the list, printing each state using outputState.
; Stop when no states left.
(define runIt (λ (seq)
                (cond
                  ((empty? seq) "Done")
                  (#t (outputState (first seq))
                      (sleep 1)
                      (runIt (rest seq)))
                  )))
                        

; Run a state sequence. Recursively process the list, printing each state using outputState.
; On each iteration, move the first state to the end of the sequence.
(define runIt_continuous (λ (seq)
                           (cond
                             ((empty? seq) "Done")
                             (#t (outputState (first seq))
                                 (sleep 1)
                                 ; Move the 'current' state to the end of the list:
                                 (runIt_continuous (append (rest seq) (list (first seq)))))
                             )))
                        



