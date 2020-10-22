# TrafficLights
Block 1 project sample files
----------------------------

**TrafficLinear.rkt**. Simple linear sequence, with a fixed time gap between transitions. To run, do either:
`(runIt stateSequence)` - run through the sequence once, then stop - or 
`(runIt-continuous stateSequence)` - keeps one repeating the sequence forever.

**TrafficFSM.rkt** FSM-based version with two events “n” and “r” (for next - move on through the sequence, and reset - go back to the beginning).
FSM transition relation is represented using lists.
To run:
`(run 1 (list "n" "r") stateTable)` - i.e. by providing a list of events as the second parameter.
Or
`(run-interactive 1 stateTable)` - interactive version type event names (followed by enter), one at a time.

**TrafficFSM-GUI.rkt** same as the other FSM one, but driven by a simple GUI (needs the two required ASIP files too).
To run:
`(startup)` starts the GUI. Button 1 generates an “n” event, Button 2, an “r”. 

**TrafficFSM-GUI-timed.rkt** - adds timed transitions - transitions that are taken from a state a specified time after entering that state.

State Transition Table
----------------------

The FSM variants use a representation of state transitions using lists - _List (StateNum x List (EventName x StateNum)_. E.g.

>(define stateTable
>  '(
>    (1 (("a" 1) ("b" 2)))
>    (2 (("a" 2) ("b" 1)))
>    ) )

For timed state transitions, numbers, representing time delay in seconds, are used as event names. E.g:
> (1 ((2 3) ("b" 2)))
is interpreted as a transition from state 1 to state 3 that happens 2 seconds after entering state 1, 
if the machine is still in state 1 at that time (an event "b" could have caused a transition to state 2 in the meantime).
