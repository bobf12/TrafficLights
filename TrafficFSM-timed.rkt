#lang racket

; Traffic Light simulation with state transition table. 
;
; adds timed transitions.
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

(define table-lookup (λ(table key)
                       (cond
                         ((empty? table) #f)
                         ((equal? (first (first table)) key) (second (first table)))
                         (#t (table-lookup (rest table) key))
                         )
                       )
  )


(define nextState (λ (st ev sTab)
                    ; trs is the list of transitions from state st.
                    ; i.e. a  List(EventName x StateNum)
                    ; where (st trs) is in sTab
                    (let ((trs (table-lookup sTab st)))
                      (cond
                        ((not trs) #f)
                        (#t (let ( (newSt (table-lookup trs ev) ) )
                              ; (ev newSt) is in trs
                              (cond
                                ((not newSt) #f)
                                (#t  newSt)
                                )
                              )
                            )
                        )
                      )                       
                    ))

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

(define run (λ (st inList sTab)
              (outputState st)
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

