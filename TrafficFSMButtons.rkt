#lang racket

; Traffic Light simulation. 
; Run through the sequence of states for a single traffic light; time delay between each state transition.
; 

(require "AsipSimulated.rkt" "AsipButtonsSimulated.rkt")


; sequence of light states

(define stateTable
  '(
    (1 (("n" 2) ("r" 1)))
    (2 (("n" 3) ("r" 1)))
    (3 (("n" 4) ("r" 1)))
    (4 (("n" 1) ("r" 1)))
    )
  )



(define nextState (λ (state event sTab)
                    (print "-- " )
                    (print  state )
                    (print " - ")
                    (println  event)
                    
                    (let ((trs (findf (λ (el) (equal? (first el) state)) sTab)))
                      (cond
                        ((not trs) #f)
                        (#t (let ((t (findf (λ (tr) (equal? (first tr) event)) (second  trs)))

                                  )
                              (cond
                                ((not t) #f)
                                (#t (second t))
                                )
                              )
                            )
                        )
                      )                       
                    ))


(define outputState
  (λ (s)
    (cond
      ((equal? s 1) "Red")
      ((equal? s 2) "Red+Amber")
      ((equal? s 3) "Green")
      ((equal? s 4) "Amber")
      )
    )
  )

(define run (λ (st inList sTab)
              (println (outputState st))
              (sleep 1)
              (cond
                ((empty? inList)  st)
                (#t (run (nextState st (first inList) sTab) (rest inList) sTab) )
                )
              )
  )


; (run startState eventList stateTransitionTable)
; e.g.:
; (run 1 (list "n" "r") stateTable)

(define run-interactive (λ (st  sTab)
                          (println (outputState st))
             
                          (sleep 1)
                          (let ((ev (read)))
                            (cond
                              ((equal? ev "x")  st)
                              (#t (run-interactive (nextState st (symbol->string ev) sTab) sTab) )
                              )
                            )
                          )
  )

