;  ---------------------------------------------
;              --- AGENTE UMANO ---
;  ---------------------------------------------
(defmodule AGENT (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

;  --------------
;  -- TEMPLATE --
;  --------------
(deftemplate phase
  (slot name (allowed-values submit random-guessing three-to-four four-to-all-right))
)  

(deftemplate colors-for-solution
  (multislot present (allowed-values blue green red yellow orange white black purple) (cardinality 0 4))
  (multislot absent (allowed-values blue green red yellow orange white black purple) (cardinality 0 4))
)

(deftemplate color-list
  (multislot color (allowed-values blue green red yellow orange white black purple) (cardinality 0 8))
)

(deftemplate candidates 
  (multislot values (allowed-values blue green red yellow orange white black purple) (cardinality 0 4))
)

(deftemplate random-comb 
  (multislot posizione)
)

(deftemplate state 
  (slot impostor) 
  (slot origin-value)
  (slot origin-step)
  (slot right-candidate) 
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


;  -------------------
;  --- FASE SUBMIT ---
;  -------------------

;allo step 0, userà sempre come tentativo la combinazione 'blue green red yellow'
(defrule first-move
  (status (step 0) (mode computer))
  ?ph <- (phase (name submit))
  =>
  (assert (guess (step 0) (g blue green red yellow)))
  (modify ?ph (name random-guessing))
  (pop-focus)
)



;--------------------
; RANDOM GUESS TILL 3+ CORRECT COLOR
;--------------------



(defrule random-guess-2rp-np

  ?ph <- (phase (name random-guessing))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  (test (eq (+ ?rp ?mp) 2))

=>

  (bind ?lista-colori (create$ blue green red yellow orange white black purple))
  ;(random 1 (lenght$ ?lista-colori)) ?
  (bind ?value1  (nth$ (random 1 8) ?lista-colori))
  (bind ?lista-colori (delete-member$ ?lista-colori ?value1))
  (bind ?value2 (nth$ (random 1 7) ?lista-colori))
  (bind ?lista-colori (delete-member$ ?lista-colori ?value2))
  (bind ?value3 (nth$ (random 1 6) ?lista-colori))
  (bind ?lista-colori (delete-member$ ?lista-colori ?value3))
  (bind ?value4  (nth$ (random 1 5) ?lista-colori))

  (assert (guess (step ?s) (g ?value1 ?value2 ?value3 ?value4)))
  (modify ?ph (name random-guessing))
  (pop-focus)
)

(defrule random-guess-1rp-np
  ?ph <- (phase (name random-guessing))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  (guess (step ?s1&:(eq (- ?s 1) ?s1)) (g ?first ?second ?third ?fourth))
  (test (eq (+ ?rp ?mp) 1) or (eq (+ ?rp ?mp) 0))
=>
  (bind ?next-guess  (delete-member$ (create$ blue green red yellow orange white black purple) (create$ ?first ?second ?third ?fourth)))
  (assert (guess (step ?s) (g ?next-guess)))
  (modify ?ph (name random-guessing))
  (pop-focus)
)


(defrule random-guess-3rp-np
  ?ph <- (phase (name random-guessing))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  (guess (step ?s1&:(eq (- ?s 1) ?s1)) (g ?first ?second ?third ?fourth))
  (test (eq (+ ?rp ?mp) 3))
=>
  (candidates_values  (delete-member$ (create$ blue green red yellow orange white black purple) (create$ ?first ?second ?third ?fourth)))
  (assert (candidates (values ?candidates_values)))
  (assert (state (right-candidate nil) (origin-step 0) (origin-value ?first ?second ?third ?fourth) (to-check 4)))
  (modify ?ph (name three-to-four))
)

; ----------------------------
;  FROM THREE TO FOUR MP + RP
; ----------------------------
; right-candidate: serve per individuare quale dei quattro è sbagliato
; regola nel caso in cui passiamo a 2 mp + rp E la fase precedente è l'origine
(defrule three-to-four-2rp-mp-eliminate
  ?ph <- (phase (name three-to-four))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  ?state-var <- (state (right-candidate nil) (origin-step ?o-s) (origin-value ?first ?second ?third ?fourth) (to-check ?check-index))
  ?candidates <- (candidates (values ?candidates_values))

  ; step -2 is origin?
  (test (eq ?o-s 1))
  (test (eq (+ ?rp ?mp) 2))
=>

  (modify ?state-var (origin-step 1) (to-check (- ?check-index 1) (origin-value ?first ?second ?third ?fourth)))
  (bind ?random-candidate (nth$ (random 1 (length$ ?candidates_values))) ?candidates_values)
  (modify ?candidates (values (delete-member$ ?candidates_values ?random-candidate)))
  (bind ?mygues (remove? (create? ?first ?second ?third ?fourth) (- ?check-index 1) (- ?check-index 1) ?random-candidate))
  (assert (guess (step ?s) (g ?mygues)))
  (modify ?ph (name three-to-four))
  (pop-focus)
)

; regola nel caso in caso passiamo a 2 mp + rp E la fase precedente non è l'origine
(defrule three-to-four-2rp-mp-findimpostor
  ?ph <- (phase (name three-to-four))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  ?state-var <- (state (right-candidate nil) (origin-step ?o-s) (origin-value ?first ?second ?third ?fourth) (to-check ?check-index))
  ?candidates <- (candidates (values ?candidates_values))

  (guess (step ?s1&:(eq (- ?s 2) ?s1)) (g ?first ?second ?third ?fourth))

  ; step -2 is origin?
  (test (?o-s &:(> ?o-s 1)))
  (test (eq (+ ?rp ?mp) 2))
=>
  
  (bind ?righ-color (nth$ ?check-index (create$ ?first ?second ?third ?fourth)))

  (modify ?state-var (right-candidate ?righ-color) (to-check (- ?check-index 1)) (origin-value ?first ?second ?third ?fourth))

  (modify ?ph (name three-to-four))
)


(defrule three-to-four-find-impostor
  ?ph <- (phase (name three-to-four))
  ?state-var <- (state (right-candidate ?righ-color&~nil) (origin-step ?o-s) (origin-value ?first ?second ?third ?fourth) (to-check ?check-index))

=>
  (bind ?mygues (remove? (create? ?first ?second ?third ?fourth) ?check-index ?check-index ?right-color))
  (assert (guess (step ?s) (g ?mygues)))
  (modify ?state-var  (to-check (- ?check-index 1) (origin-value ?first ?second ?third ?fourth)))
  (modify ?ph (name three-to-four))
  (pop-focus)
)



; regola in cui restiamo 3 mp + rp 
(defrule three-to-four-3rp-mp
  ?ph <- (phase (name three-to-four))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  ?state <- (state (right-candidate nil) (origin-step ?o-s) (origin-value ?first ?second ?third ?fourth) (to-check ?check-index))
  ?candidates <- (candidates (values ?candidates_values))
  (test (eq (+ ?rp ?mp) 3))
=>
  (bind ?random-candidate (nth$ (random 1 (length$ ?candidates_values))) ?candidates_values)
  (modify ?candidates (values (delete-member$ ?candidates_values ?random-candidate)))
  (modify ?ph (name three-to-four))
  (modify ?state (origin-step (+ ?o-s 1)))
  (bind ?mygues (remove? (create? ?first ?second ?third ?fourth) ?check-index ?check-index ?random-candidate))
  (assert (guess (step ?s) (g ?mygues )))
  (pop-focus)
)



;  -------------------------------
;  --- DA 4 A CASO A 4 GIUSTE ----
;  -------------------------------

;controlliamo se abbiamo 4 feedback, giusti o semigiusti
(defrule check-game-response-four (declare (salience 100))
 (status (step ?s&:(> ?s 0)) (mode computer))
 ?ph <- (or (phase (name three-to-four) (phase (name random-guessing))))
 (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
 (test (eq (+ ?rp ?mp) 4))
 =>
 (modify ?ph (name four-to-all-right))
)

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
  (assert (guess (step ?s) (g ?mygues )))
  (modify ?ph (name four-to-all-right))
  (pop-focus)
)



;come fatto iniziale parto dalla fase submit
(deffacts initial-facts
  (phase (name submit))
  (colors-for-solution)
  (color-list(color blue green red yellow orange white black purple))
)