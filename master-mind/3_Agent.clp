;  ---------------------------------------------
;              --- AGENTE UMANO ---
;  ---------------------------------------------
(defmodule AGENT (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

;  --------------
;  -- TEMPLATE --
;  --------------
(deftemplate phase
  (slot name (allowed-values begin random-guessing three-to-four four-to-all-right))
)  


(deftemplate candidates 
  (multislot values (allowed-values blue green red yellow orange white black purple) (cardinality 0 4))
)

(deftemplate random-comb 
  (multislot posizione)
)

(deftemplate state 
  (slot right-candidate) 
  (slot potential-candidate) 
  (slot origin-step)
  (multislot origin-value)
  (slot to-check)
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



;controlliamo se abbiamo 4 feedback, giusti o semigiusti
(defrule check-game-response-four-case1 (declare (salience 100))
 (status (step ?s&:(> ?s 0)) (mode computer))
 ?ph <- (phase (name ?phase&:(or (eq ?phase random-guessing) (eq ?phase three-to-four))))
 (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
 (test (eq (+ ?rp ?mp) 4))
 =>
 (modify ?ph (name four-to-all-right))
)



;  -------------------
;  --- FASE BEGIN ---
;  -------------------
(defrule first-move
  (status (step 0) (mode computer))
  ?ph <- (phase (name begin))
  =>
  (assert (guess (step 0) (g blue red black yellow)))
  (modify ?ph (name random-guessing))
  (pop-focus)
)

;  -----------------------------------
;   RANDOM GUESS TILL 3+ CORRECT COLOR
;  -----------------------------------

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

  (printout t "random-guess-2rp-np is trying with color" ?value1 ?value2 ?value3 ?value4 crlf)
  (assert (guess (step ?s) (g ?value1 ?value2 ?value3 ?value4)))
  (modify ?ph (name random-guessing))
  (pop-focus)
)

(defrule random-guess-1or0rp-np
  ?ph <- (phase (name random-guessing))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  (guess (step ?s1&:(eq (- ?s 1) ?s1)) (g ?first ?second ?third ?fourth))
  (test (or (eq (+ ?rp ?mp) 1) (eq (+ ?rp ?mp) 0)))
=>
  (bind ?next-guess  (delete-member$ (create$ blue green red yellow orange white black purple) ?first ?second ?third ?fourth))
  (printout t "random-guess-1or0rp-np is trying with color" ?next-guess crlf)
  (assert (guess (step ?s) (g ?next-guess)))
  (modify ?ph (name random-guessing))
  (pop-focus)
)
;qua è possibile accorciare l'agente tramite random-guess-1or0rp-np per far partire direttamente from three to four ma sarebbe un po' confusso

(defrule random-guess-3rp-np
  ?ph <- (phase (name random-guessing))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  (guess (step ?s1&:(eq (- ?s 1) ?s1)) (g ?first ?second ?third ?fourth))
  (test (eq (+ ?rp ?mp) 3))
=>

  ;(bind ?candidates_values  (delete-member$ (create$ blue green red yellow orange white black purple) ?first ?second ?third ?fourth))
  ;(bind ?random-candidate (nth$ (random 1 (length$ ?candidates_values)) ?candidates_values))
  ;(bind ?candidates_values  (delete-member$ ?candidates_values ?random-candidate))

  ;(bind ?mygues (create$ ?first ?second ?third ?random-candidate))
  ;(assert (guess (step ?s) (g ?mygues)))

  ;(assert (candidates (values ?candidates_values)))
  ;(assert (state (right-candidate nil) (potential-candidate nil) (origin-step 1) (origin-value ?first ?second ?third ?fourth) (to-check 4)))
  ;(modify ?ph (name three-to-four))
  (printout t "random-guess-3rp-np trying to guess "  crlf)
  (bind ?candidates_values  (delete-member$ (create$ blue green red yellow orange white black purple) ?first ?second ?third ?fourth))
  (assert (candidates (values ?candidates_values)))
  (assert (state (right-candidate nil) (origin-step 0)  (potential-candidate nil)  (origin-value ?first ?second ?third ?fourth) (to-check 4)))
  (modify ?ph (name three-to-four))
  ;(pop-focus)
)

; ----------------------------
;  FROM THREE TO FOUR MP + RP
; ----------------------------
; right-candidate: serve per individuare quale dei quattro è sbagliato
; regola nel caso in cui passiamo a 2 mp + rp E la fase precedente è l'origine
(defrule three-to-four-2rp-mp-eliminate 
  ?ph <- (phase (name three-to-four))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  ?state-var <- (state (right-candidate ?r-c&:(eq ?r-c nil)) (potential-candidate ?p-c) (origin-step ?o-s) (origin-value ?first ?second ?third ?fourth) (to-check ?check-index))
  ?candidates <- (candidates (values $?candidates_values))

  ; step -2 is origin?
  (test (eq ?o-s 1))
  (test (eq (+ ?rp ?mp) 2))
=>
  
  (modify ?state-var (origin-step 1) (to-check (- ?check-index 1)) (origin-value ?first ?second ?third ?fourth) (potential-candidate nil))
  (bind ?random-candidate (nth$ (random 1 (length$ ?candidates_values)) ?candidates_values))
  (modify ?candidates (values (delete-member$ ?candidates_values ?random-candidate)))
  (bind ?mygues (replace$ (create$ ?first ?second ?third ?fourth) (- ?check-index 1) (- ?check-index 1) ?random-candidate))
  (assert (guess (step ?s) (g ?mygues)))
  (modify ?ph (name three-to-four))
  (printout t " three-to-four-2rp-mp-eliminate " ?mygues crlf)
  (pop-focus)
)

; regola nel caso in caso passiamo a 2 mp + rp E la fase precedente non è l'origine
(defrule three-to-four-2rp-mp-findimpostor
  ?ph <- (phase (name three-to-four))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  ?state-var <- (state (right-candidate ?r-c&:(eq ?r-c nil)) (potential-candidate ?p-c) (origin-step ?o-s) (origin-value ?first ?second ?third ?fourth) (to-check ?check-index))
  ?candidates <- (candidates (values $?candidates_values))

  ;(guess (step ?s2&:(eq (- ?s 2) ?s2)) (g ?first ?second ?third ?fourth))

  ; step -2 is origin?
  (test (> ?o-s 1))
  (test (eq (+ ?rp ?mp) 2))
=>

  (bind ?right-color ?p-c)
  ;(bind ?righ-color (nth$ ?check-index (create$ ?first ?second ?third ?fourth)))

  (modify ?state-var (right-candidate ?right-color) (to-check (- ?check-index 1)) (origin-value ?first ?second ?third ?fourth))

  (printout t "three-to-four-2rp-mp-findimpostor the missing color is " ?right-color crlf)


  (modify ?ph (name three-to-four))
)


(defrule three-to-four-find-impostor
  ?ph <- (phase (name three-to-four))
  (status (step ?s&:(> ?s 0)) (mode computer))
  ?state-var <- (state (right-candidate ?right-color&~nil) (origin-step ?o-s) (origin-value ?first ?second ?third ?fourth) (to-check ?check-index))

=>
  (bind ?mygues (replace$ (create$ ?first ?second ?third ?fourth) ?check-index ?check-index ?right-color))
  (printout t "three-to-four-find-impostor trying with" ?mygues crlf)
  (assert (guess (step ?s) (g ?mygues)))
  (modify ?state-var  (to-check (- ?check-index 1)) (origin-value ?first ?second ?third ?fourth))
  (modify ?ph (name three-to-four))
  (pop-focus)
)



; regola in cui restiamo 3 mp + rp 
(defrule three-to-four-3rp-mp
  ?ph <- (phase (name three-to-four)) ;ok
  (status (step ?s&:(> ?s 0)) (mode computer)) ;ok
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp)) ;ok
  (guess (step ?s2&:(eq (- ?s 1) ?s2)) (g $?guesses)) 
  ?state <- (state (right-candidate ?r-c&:(eq ?r-c nil)) (origin-step ?o-s) (origin-value ?first ?second ?third ?fourth) (to-check ?check-index))
  ?candidates <- (candidates (values $?candidates_values))
  (test (eq (+ ?rp ?mp) 3))
=>
  (bind ?random-candidate (nth$ (random 1 (length$ ?candidates_values)) ?candidates_values))
  (modify ?candidates (values (delete-member$ ?candidates_values ?random-candidate)))
  (modify ?ph (name three-to-four))
  (bind ?p-c  (nth$ ?check-index $?guesses))
  (modify ?state (origin-step (+ ?o-s 1)) (potential-candidate ?p-c))
  (bind ?mygues (replace$ (create$ ?first ?second ?third ?fourth) ?check-index ?check-index ?random-candidate))
  (printout t "three-to-four-3rp-mp" ?mygues crlf)
  (assert (guess (step ?s) (g ?mygues )))
  (pop-focus)
)



;  -------------------------------
;  --- DA 4 A CASO A 4 GIUSTE ----
;  -------------------------------



(defrule four-to-all-right-permute
  ?ph <- (phase (name four-to-all-right))
  (status (step ?s&:(> ?s 0)) (mode computer))
  ;(answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  (guess (step ?s1&:(eq (- ?s 1) ?s1)) (g ?first ?second ?third ?fourth))
=>
  (bind ?lista-colori (create$ ?first ?second ?third ?fourth))
  (bind ?value1  (nth$ (random 1 4) ?lista-colori))
  (bind ?lista-colori (delete-member$ ?lista-colori ?value1))
  (bind ?value2 (nth$ (random 1 3) ?lista-colori))
  (bind ?lista-colori (delete-member$ ?lista-colori ?value2))
  (bind ?value3 (nth$ (random 1 2) ?lista-colori))
  (bind ?lista-colori (delete-member$ ?lista-colori ?value3))
  (bind ?value4  (nth$ 1 ?lista-colori))
  (bind ?mygues (create$ ?value1 ?value2 ?value3 ?value4))
  (printout t "four-to-all-right-permute" ?mygues crlf)
  (assert (guess (step ?s) (g ?mygues )))
  (modify ?ph (name four-to-all-right))
  (pop-focus)
)



;  ------------------
;  --- INIT FACTS and GENERAL RULES---
;  ------------------
(deffacts init-facts (phase (name begin)))