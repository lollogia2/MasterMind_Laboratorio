;  ---------------------------------------------
;  --- ALGORITMO DI KNUTH ---
;  ---------------------------------------------
(defmodule AGENT (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

(deftemplate combination (multislot code (allowed-values blue green red yellow orange white black purple) (cardinality 4 4)))

(defrule human-player
  (status (step ?s) (mode human))
  =>
  (printout t "Your guess at step " ?s crlf)
  (bind $?input (readline))
  (assert (guess (step ?s) (g  (explode$ $?input)) ))
  (pop-focus)
 )

 (defrule first-move
  (status (step 1) (mode computer))
  =>
  (assert (guess (step 1) (g blue green red yellow)))
  (pop-focus)
 )

