;  ---------------------------------------------
;              --- AGENTE UMANO ---
;  ---------------------------------------------
(defmodule AGENT (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

;  --------------
;  -- TEMPLATE --
;  --------------

; Agent phase
(deftemplate phase
  (slot name (allowed-values begin random-guessing three-to-four four-to-all-right)) 
)  

; Colors that can be the missing fourth color in phase 3
(deftemplate candidates 
  (multislot values (allowed-values blue green red yellow orange white black purple) (cardinality 0 4))
)

; State at phase 3 
(deftemplate state-ph3 
  (slot right-candidate) ;the missing color
  (slot potential-candidate) ;the slot contains potentialy the right color
  (slot origin-step) ;contains the step at witch the agent start checking new color for a position in the sequence
  (multislot origin-value) ;contains the colors at origin step
  (slot to-check) ;contains the cosidered position in the sequence
)

;Template for available moves for phase 4
(deftemplate moves-ph4
  (slot left (type INTEGER)) ;first element index of color switch
  (slot right (type INTEGER)) ;second element index of color switch
)

; State at phase 4
(deftemplate state-ph4
 (multislot origin) ;origin state
 (slot right-placed (type INTEGER));right placed in origin state
 (slot moved-before-left); the first element index of the switch applied to origin
 (slot moved-before-right); the second element index of the switch applied to origin
)


;  --------------
;  -- UMANO -----
;  --------------
(defrule human-player
  (status (step ?s) (mode human))
  =>
  (printout t "Your guess at step " ?s crlf)
  (bind $?input (readline))
  (assert (guess (step ?s) (g  (explode$ $?input)) ))
  (pop-focus)
 )



;  ----------------------------------
;  --- INIT FACTS and GENERAL RULES---
;  ----------------------------------
(deffacts init-facts 
  (phase (name begin))
)

(deffacts movements 
  (moves-ph4 (left 1) (right 2))
  (moves-ph4 (left 1) (right 3))
  (moves-ph4 (left 1) (right 4))
  (moves-ph4 (left 2) (right 3))
  (moves-ph4 (left 2) (right 4))
  (moves-ph4 (left 3) (right 4))
)

; Check if we have four feedbacks (rightplaced or misplaced)
(defrule check-game-response-four (declare (salience 100))
  (status (step ?s&:(> ?s 0)) (mode computer))
  ?ph <- (phase (name ?phase&:(or (eq ?phase random-guessing) (eq ?phase three-to-four))))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  (guess (step ?s1) (g ?first ?second ?third ?fourth))
  (test (eq (+ ?rp ?mp) 4))
 =>
  (printout t "Four color are right but only " ?rp " in corect position" crlf)
  (modify ?ph (name four-to-all-right))
  (assert (state-ph4 (origin nil nil nil nil) (moved-before-left nil) (moved-before-right nil)))
)


;  -----------------------
;  --- PHASE 1: BEGIN ----
;  -----------------------
;in our algorithm the 'opening' is always the same move (blue-red-black-yellow)
(defrule first-move
  (status (step 0) (mode computer))
  ?ph <- (phase (name begin))
  =>
  (assert (guess (step 0) (g blue red black yellow)))
  (modify ?ph (name random-guessing))
  (pop-focus)
)


;  --------------------------------------------
;  PHASE 2: RANDOM GUESS TILL 3+ CORRECT COLOR
;  --------------------------------------------

; The random color sequence have only two feedback, we need another random sequence
(defrule random-guess-2rp-np
  ?ph <- (phase (name random-guessing))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  (test (eq (+ ?rp ?mp) 2))
=>
  (bind ?lista-colori (create$ blue green red yellow orange white black purple))
  (bind ?value1  (nth$ (random 1 (length$ ?lista-colori)) ?lista-colori))
  (bind ?lista-colori (delete-member$ ?lista-colori ?value1))
  (bind ?value2 (nth$ (random 1 (length$ ?lista-colori)) ?lista-colori))
  (bind ?lista-colori (delete-member$ ?lista-colori ?value2))
  (bind ?value3 (nth$ (random 1 (length$ ?lista-colori)) ?lista-colori))
  (bind ?lista-colori (delete-member$ ?lista-colori ?value3))
  (bind ?value4  (nth$ (random 1 (length$ ?lista-colori)) ?lista-colori))
  (printout t "Running random-guess-2rp-np is trying with color " ?value1 ?value2 ?value3 ?value4 crlf)
  (assert (guess (step ?s) (g ?value1 ?value2 ?value3 ?value4)))
  (modify ?ph (name random-guessing))
  (pop-focus)
)

; The random color sequence have one or zero feedback, in this case we discard al the color and take the oposite
(defrule random-guess-1or0rp-np
  ?ph <- (phase (name random-guessing))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  (guess (step ?s1&:(eq (- ?s 1) ?s1)) (g ?first ?second ?third ?fourth))
  (test (or (eq (+ ?rp ?mp) 1) (eq (+ ?rp ?mp) 0)))
=>
  (bind ?next-guess  (delete-member$ (create$ blue green red yellow orange white black purple) ?first ?second ?third ?fourth))
  (printout t "Running random-guess-1or0rp-np is trying with color" ?next-guess crlf)
  (assert (guess (step ?s) (g ?next-guess)))
  (modify ?ph (name random-guessing))
  (pop-focus)
)

; The random color have three feedback. Now it's possible to pass to the next phase.
(defrule random-guess-3rp-np
  ?ph <- (phase (name random-guessing))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  (guess (step ?s1&:(eq (- ?s 1) ?s1)) (g ?first ?second ?third ?fourth))
  (test (eq (+ ?rp ?mp) 3))
=>
  (printout t "Running random-guess-3rp-np trying to guess "  crlf)
  (bind ?candidates_values  (delete-member$ (create$ blue green red yellow orange white black purple) ?first ?second ?third ?fourth))
  (assert (candidates (values ?candidates_values)))
  (assert (state-ph3 (right-candidate nil) (origin-step 0)  (potential-candidate nil)  (origin-value ?first ?second ?third ?fourth) (to-check 4)))
  (modify ?ph (name three-to-four))
)

; ----------------------------
;  FROM THREE TO FOUR MP + RP
; ----------------------------

; In the case where the guess have three feedbacks, it's necessary to change the color of position to-check till give 4 feedbacks 
; or potentialy 2 feedback (in that case the colors checked before are all right colors)
(defrule three-to-four-3rp-mp
  ?ph <- (phase (name three-to-four))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  (guess (step ?s2&:(eq (- ?s 1) ?s2)) (g $?guesses)) 
  ?state-ph3 <- (state-ph3 (right-candidate ?r-c&:(eq ?r-c nil)) (origin-step ?o-s) (origin-value ?first ?second ?third ?fourth) (to-check ?check-index))
  ?candidates <- (candidates (values $?candidates_values))
  (test (eq (+ ?rp ?mp) 3))
=>
  (bind ?random-candidate (nth$ (random 1 (length$ ?candidates_values)) ?candidates_values))
  (modify ?candidates (values (delete-member$ ?candidates_values ?random-candidate)))
  (modify ?ph (name three-to-four))
  (bind ?p-c  (nth$ ?check-index $?guesses))
  (modify ?state-ph3 (origin-step (+ ?o-s 1)) (potential-candidate ?p-c))
  (bind ?mygues (replace$ (create$ ?first ?second ?third ?fourth) ?check-index ?check-index ?random-candidate))
  (printout t "Running three-to-four-3rp-mp with " ?mygues crlf)
  (assert (guess (step ?s) (g ?mygues )))
  (pop-focus)
)

; If the previous guess give only 2 feedbacks instead of 3 AND the previous state is the origin: the changed color is wrong and orign color is right
(defrule three-to-four-2rp-mp-eliminate 
  ?ph <- (phase (name three-to-four))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  ?state-ph3-var <- (state-ph3 (right-candidate ?r-c&:(eq ?r-c nil)) (potential-candidate ?p-c) (origin-step ?o-s) (origin-value ?first ?second ?third ?fourth) (to-check ?check-index))
  ?candidates <- (candidates (values $?candidates_values))

  ; step -2 is origin?
  (test (eq ?o-s 1))
  (test (eq (+ ?rp ?mp) 2))
=>
  
  (modify ?state-ph3-var (origin-step 1) (to-check (- ?check-index 1)) (origin-value ?first ?second ?third ?fourth) (potential-candidate nil))
  (bind ?random-candidate (nth$ (random 1 (length$ ?candidates_values)) ?candidates_values))
  (modify ?candidates (values (delete-member$ ?candidates_values ?random-candidate)))
  (bind ?mygues (replace$ (create$ ?first ?second ?third ?fourth) (- ?check-index 1) (- ?check-index 1) ?random-candidate))
  (assert (guess (step ?s) (g ?mygues)))
  (modify ?ph (name three-to-four))
  (printout t "Running three-to-four-2rp-mp-eliminate " ?mygues crlf)
  (pop-focus)
)

; If the previous guess give only 2 feedbacks instead of 3 AND the previous state is not the origin: 
; there are two color both right, the origin color and the color before the two feedback (the right candidate)
(defrule three-to-four-2rp-mp-findimpostor
  ?ph <- (phase (name three-to-four))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  ?state-ph3-var <- (state-ph3 (right-candidate ?r-c&:(eq ?r-c nil)) (potential-candidate ?p-c) (origin-step ?o-s) (origin-value ?first ?second ?third ?fourth) (to-check ?check-index))
  ?candidates <- (candidates (values $?candidates_values))
 ; step -2 is origin?
  (test (> ?o-s 1))
  (test (eq (+ ?rp ?mp) 2))
=>
  (bind ?right-color ?p-c)
  (printout t "Running three-to-four-2rp-mp-findimpostor the missing color is " ?right-color crlf)

  (modify ?state-ph3-var (right-candidate ?right-color) (to-check (- ?check-index 1)) (origin-value ?first ?second ?third ?fourth))
  (modify ?ph (name three-to-four))
)

; Once the right candidate is well known, it's time to find which color of the remaining position to-check is the impostor
(defrule three-to-four-find-impostor
  ?ph <- (phase (name three-to-four))
  (status (step ?s&:(> ?s 0)) (mode computer))
  ?state-ph3-var <- (state-ph3 (right-candidate ?right-color&~nil) (origin-step ?o-s) (origin-value ?first ?second ?third ?fourth) (to-check ?check-index))

=>
  (bind ?mygues (replace$ (create$ ?first ?second ?third ?fourth) ?check-index ?check-index ?right-color))
  (printout t "Running three-to-four-find-impostor trying with " ?mygues crlf)
  (assert (guess (step ?s) (g ?mygues)))
  (modify ?state-ph3-var  (to-check (- ?check-index 1)) (origin-value ?first ?second ?third ?fourth))
  (modify ?ph (name three-to-four))
  (pop-focus)
)


;  -------------------------------
;  --- DA 4 A 4 GIUSTE ----
;  -------------------------------



; The algorithm start to check a new sequence chosing a possibile switching move that can either improve or get worse the number of right placed.
(defrule four-to-all-right-init (declare (salience 20))

  ?ph <- (phase (name four-to-all-right))
  (status (step ?s&:(> ?s 0)) (mode computer))
  ?s-ph4 <- (state-ph4 (origin nil nil nil nil) (moved-before-left nil) (moved-before-right nil))
  ?mv <- (moves-ph4 (left ?left-position) (right ?right-position))

  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  (guess (step ?s1) (g $?lista-before))

=>

  (bind ?value-left-position (nth$ ?left-position $?lista-before))
  (bind ?value-right-position (nth$ ?right-position $?lista-before))
  (bind $?changed-colors (replace$ $?lista-before ?right-position ?right-position ?value-left-position))
  (bind $?changed-colors (replace$ $?changed-colors ?left-position ?left-position ?value-right-position))

  (printout t "Running four-to-al-right-init trying : (color " $?changed-colors ") (move " ?left-position  " " ?right-position ") (before_rp: " ?rp ")" crlf)


  (modify ?s-ph4 (origin $?lista-before) (right-placed ?rp) (moved-before-left ?left-position) (moved-before-right ?right-position))
  (assert (guess (step ?s) (g $?changed-colors)))
  (modify ?ph (name four-to-all-right))
  (retract (fact-index ?mv))

  (pop-focus)

)

; ALGORITHM
; Checking if we have a worst result, with the new guess, than the 'best' one
; if so, then we take the 'best' one and we try the next move
(defrule four-to-all-right-wrong (declare (salience 10))
  ?ph <- (phase (name four-to-all-right))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp_before) (miss-placed ?mp))
  ?s-ph4 <- (state-ph4 (origin $?lista-origin) (right-placed ?rp_origin))
  ?mv <- (moves-ph4 (left ?left-position) (right ?right-position))
  (test (< ?rp_before ?rp_origin))
=>
  (bind ?value-left-position (nth$ ?left-position $?lista-origin))
  (bind ?value-right-position (nth$ ?right-position $?lista-origin))
  (bind $?changed-colors (replace$ $?lista-origin ?right-position ?right-position ?value-left-position))
  (bind $?changed-colors (replace$ $?changed-colors ?left-position ?left-position ?value-right-position))

  (printout t "Running four-to-al-right-wrong trying: (color " $?changed-colors ") (move " ?left-position  " " ?right-position ")  (before-rp " ?rp_before ") (origin-rp " ?rp_origin")" crlf)

  (modify ?s-ph4 (origin $?lista-origin) (right-placed ?rp_origin) (moved-before-left ?left-position) (moved-before-right ?right-position))
  (assert (guess (step ?s) (g $?changed-colors)))
  (modify ?ph (name four-to-all-right))
  (retract (fact-index ?mv))

  (pop-focus)

)

; checking if the 'right-placed' colors on the new guess are more than the 'best' combination(origin)
; if so, then we take the new one and we try the next move
(defrule four-to-all-right-correct
  ?ph <- (phase (name four-to-all-right))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp_before) (miss-placed ?mp))
  (guess (step ?s1) (g $?lista-before))

  ?s-ph4 <- (state-ph4 (origin $?lista-origin) (right-placed ?rp_origin))
  ?mv <- (moves-ph4 (left ?left-position) (right ?right-position))
  (test (> ?rp_before ?rp_origin))
=>

  (bind ?value-left-position (nth$ ?left-position $?lista-before))
  (bind ?value-right-position (nth$ ?right-position $?lista-before))

  (bind $?changed-colors (replace$ $?lista-before ?right-position ?right-position ?value-left-position))
  (bind $?changed-colors (replace$ $?changed-colors ?left-position ?left-position ?value-right-position))

  (printout t "Running four-to-al-right-correct trying: (color " $?changed-colors ") (move " ?left-position  " " ?right-position ")  (before-rp " ?rp_before ") (origin-rp " ?rp_origin")" crlf)

  (modify ?s-ph4 (origin $?lista-before) (right-placed ?rp_before) (moved-before-left ?left-position) (moved-before-right ?right-position))

  (assert (guess (step ?s) (g $?changed-colors)))
  (modify ?ph (name four-to-all-right))

  (retract (fact-index ?mv))

  (pop-focus)

)

; SPECIAL CASE

; CASE 0->0
; checking if the 'right-place' from the origin state and current state didn't change
(defrule four-to-all-right-zero-to-zero (declare (salience 15))
  ?ph <- (phase (name four-to-all-right))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp_before) (miss-placed ?mp))
  (guess (step ?s1) (g $?lista-guess))

  ?s-ph4 <- (state-ph4(origin $?lista-origin) (right-placed ?rp_origin) (moved-before-left ?left-position&:(neq ?left-position nil)) (moved-before-right ?right-position&:(neq ?right-position nil)))
  (test (eq ?rp_before ?rp_origin))
=>
  (bind $?not-moved-before (delete-member$ (create$ 1 2 3 4) ?left-position ?right-position))
  (bind ?left-position-next (nth$ 1 $?not-moved-before))
  (bind ?right-position-next (nth$ 2 $?not-moved-before))

  (bind ?value-left-position (nth$ ?left-position $?lista-origin))
  (bind ?value-right-position (nth$ ?right-position $?lista-origin))
  (bind ?value-left-position-next (nth$ ?left-position-next $?lista-origin))
  (bind ?value-right-position-next (nth$ ?right-position-next $?lista-origin))

  (bind $?changed-colors (replace$ $?lista-origin ?right-position-next ?right-position-next ?value-right-position))
  (bind $?changed-colors (replace$ $?changed-colors ?right-position ?right-position ?value-right-position-next))
  (bind $?changed-colors (replace$ $?changed-colors ?left-position-next ?left-position-next ?value-left-position))
  (bind $?changed-colors (replace$ $?changed-colors ?left-position ?left-position ?value-left-position-next))

  (do-for-all-facts ((?f moves-ph4)) TRUE (retract (fact-index ?f)))
  (assert (moves-ph4 (left ?left-position) (right ?right-position)))
  (assert (moves-ph4 (left ?left-position-next) (right ?right-position-next)))
  (assert (guess (step ?s) (g $?changed-colors)))

  (modify ?s-ph4 (origin nil nil nil nil) (moved-before-left nil) (moved-before-right nil))
  
  (printout t "Running four-to-al-right-zero-to-zero trying: (color " $?changed-colors ") (before-rp " ?rp_before ") (origin-rp " ?rp_origin")" crlf)
  (modify ?ph (name four-to-all-right))
  (pop-focus)
)

; Checking if the 'right-place' in the origin state are equal to 0, and if the 'right-place' in the last state are equal to 2
(defrule four-to-all-right-zero-two (declare (salience 15))

  ?ph <- (phase (name four-to-all-right))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp_before) (miss-placed ?mp))
  (guess (step ?s1) (g $?lista-guess))

  ?s-ph4 <- (state-ph4(origin $?lista-origin) (right-placed ?rp_origin) (moved-before-left ?left-position&:(neq ?left-position nil)) (moved-before-right ?right-position&:(neq ?right-position nil)))

  (test (eq ?rp_before 2))
  (test (eq ?rp_origin 0))

=>

  (bind $?not-moved-before (delete-member$ (create$ 1 2 3 4) ?left-position ?right-position))
  (bind ?to-switch-left (nth$ 1 $?not-moved-before))
  (bind ?to-switch-right (nth$ 2 $?not-moved-before))

  (bind ?value-to-switch-left(nth$ ?to-switch-left $?lista-guess))
  (bind ?value-to-switch-right (nth$ ?to-switch-right $?lista-guess))

  (bind $?changed-colors (replace$ $?lista-guess ?to-switch-left  ?to-switch-left  ?value-to-switch-right))
  (bind $?changed-colors (replace$ $?changed-colors ?to-switch-right ?to-switch-right ?value-to-switch-left ))

  (printout t "Running four-to-al-right-zero-to-two trying (applied to before): (color " $?changed-colors ") (move " ?to-switch-left " " ?to-switch-right ") (before-rp " ?rp_before ") (origin-rp " ?rp_origin")" crlf)

  (assert (guess (step ?s) (g $?changed-colors)))
  (modify ?ph (name four-to-all-right))
  (pop-focus)

)

; CASE 2->0
; Checking if the 'right-place' in the origin state are equal to 2, and if the 'right-place' in the last state are equal to 0
(defrule four-to-all-right-two-zero (declare (salience 15))

  ?ph <- (phase (name four-to-all-right))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp_before) (miss-placed ?mp))
  (guess (step ?s1) (g $?lista-guess))

  ?s-ph4 <- (state-ph4(origin $?lista-origin) (right-placed ?rp_origin) (moved-before-left ?left-position&:(neq ?left-position nil)) (moved-before-right ?right-position&:(neq ?right-position nil)))

  (test (eq ?rp_before 0))
  (test (eq ?rp_origin 2))

=>

  (bind $?not-moved-before (delete-member$ (create$ 1 2 3 4) ?left-position ?right-position))
  (bind ?to-switch-left (nth$ 1 $?not-moved-before))
  (bind ?to-switch-right (nth$ 2 $?not-moved-before))

  (bind ?value-to-switch-left(nth$ ?to-switch-left $?lista-origin))
  (bind ?value-to-switch-right (nth$ ?to-switch-right $?lista-origin))

  (bind $?changed-colors (replace$ $?lista-origin ?to-switch-left  ?to-switch-left  ?value-to-switch-right))
  (bind $?changed-colors (replace$ $?changed-colors ?to-switch-right ?to-switch-right ?value-to-switch-left ))

  (printout t "Running four-to-al-right-two-to-zero trying (applied to origin): (color " $?changed-colors ") (move " ?to-switch-left " " ?to-switch-right ") (before-rp " ?rp_before ") (origin-rp " ?rp_origin")" crlf)

  (assert (guess (step ?s) (g $?changed-colors)))
  (modify ?ph (name four-to-all-right))
  (pop-focus)
)




