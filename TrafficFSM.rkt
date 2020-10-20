#lang racket

; Traffic Light simulation with state transition table. 
; 

;; Works in 'batch' mode by providing a list of states:
; (run startState eventList stateTransitionTable)
; e.g.:
; (run 1 (list "n" "r") stateTable)

; Or interactively where the user types an event name + enter:
; (run-interactive startState stateTable)
; (run-interactive 1 stateTable)

; State transition table
; List (StateNum x (List(EventName x StateNum)
; Actually the pairs (x products) are also lists.
; StateNum = integer, EventName = String.

(define stateTable
  '(
    (1 (("n" 2) ("r" 1)))
    (2 (("n" 3) ("r" 1)))
    (3 (("n" 4) ("r" 1)))
    (4 (("n" 1) ("r" 1)))
    )
  )



(define nextState (λ (st ev sTab)
  ; trs is the list of transitions from state st.
                    ; i.e. a StateNum x List(EventName x StateNum)
                    ; where the first element = st.
                    (let ((trs (findf (λ (el) (equal? (first el) st)) sTab)))
                      (cond
                        ((not trs) #f)
                        (#t (let ((t (findf (λ (tr) (equal? (first tr) ev)) (second  trs)))

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

