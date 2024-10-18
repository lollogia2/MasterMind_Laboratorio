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

(deftemplate origin-state 
 (multislot origin (allowed-values blue green red yellow orange white black purple) (cardinality 4 4))
 (slot right-placed (type INTEGER))
 (slot moved-before-primo (type INTEGER))
 (slot moved-before-secondo (type INTEGER))

)

(deftemplate solution 
  (multislot sol (allowed-values blue green red yellow orange white black purple) (cardinality 4 4))
)

(deftemplate moves-ph4
  (slot primo (type INTEGER))
  (slot secondo (type INTEGER))
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
 (guess (step ?s1) (g ?first ?second ?third ?fourth))

 (test (eq (+ ?rp ?mp) 4))
 =>
 (modify ?ph (name four-to-all-right))
 (assert origin-state (origin ?first ?second ?third ?fourth) (right-placed ?rp))
 (assert trynext)
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
;  --- DA 4 A 4 GIUSTE ----
;  -------------------------------

;; Strutture dati necessarie: 
;; 1.stato origine (da riutilizzare anche sopra per l'impostor): OCCHIO non è il stato 'prima', è l'ultimo stato in cui è avuto un successo
;; 2.lista azioni disponibili espresse ciascuna come coppia
;; 3.multislot fisso di 4 per colori coretti


;; Regole con salience alta che catturano i casi semplici da risolvere (DOPO)

;; Algoritmo di risoluzione 

;; 1) Regola che applica una nuova permutazione tra quelle disponibili
;; PRIMA: A CASO
;; DOPO: lo scambio da effettuare è compatibile con il multislot dei colori certi (effettuabile nelle precondizioni); da implementare anche regola
;;       generica per catturare i colori sicuri

(deftemplate origin-state 
 (multislot origin (allowed-values blue green red yellow orange white black purple) (cardinality 4 4)
 (slot right-placed (type INTEGER)))
)


(defrule four-to-al-right-move 
  (trynext)
  ?ph <- (phase (name four-to-all-right))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (origin-state (origin $?lista-origin) (right-placed ?rp))
  (move-ph4 (primo ?primo-scambio) (secondo ?secondo-scambio))
=>
  (bind ?value-primo-scambio (nth$ ?primo-scambio ?lista-origin))
  (bind ?value-secondo-scambio (nth$ ?secondo-scambio ?lista-origin))
  (bind ?changed-colors (replace$ $?lista-origin ?secondo-scambio ?secondo-scambio ?value-primo-scambio))
  (bind ?changed-colors (replace$ $?changed-colors ?primo-scambio ?primo-scambio ?value-secondo-scambio))
  
  (assert (guess (step ?s) (g ?changed-colors)))
  (assert (origin-state (origin $?lista-origin) (right-placed ?rp) (moved-before-primo ?primo-scambio) (moved-before-secondo ?secondo-scambio)))
  (modify ?ph (name four-to-all-right))
  (printout t " four-to-al-right-move trying " ?changed-colors crlf)
  (pop-focus)
)

;; 2) Regola che cattura un fallimento
(defrule four-to-al-right-move-is-wrong 
  ?ph <- (phase (name four-to-all-right))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1) (right-placed ?rp_before) (miss-placed ?mp))
  (origin-state (origin $?lista-origin) (right-placed ?rp_origin))
  (test (< ?rp_before ?rp_origin))
=>
  (assert trynext)
  (assert (origin-state (origin $?lista-origin) (right-placed ?rp_origin)))
)
;; PRECOND: rosse prima maggiori di adesso
;; CONSEGUENZE: non cambia origine, passa a 1)
;; 3) Regola che cattura un successo
;; PRECOND: rosse prima minori di adesso
;; CONSEGUENZE: cambia origine, passa a 1)
;; Regola che cattura il caso di uguaglianza (0-->0)

(defrule four-to-al-right-move-is-correct 
  ?ph <- (phase (name four-to-all-right))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1) (right-placed ?rp_before) (miss-placed ?mp))
  (origin-state (origin $?lista-origin) (right-placed ?rp_origin))
  (test (> ?rp_before ?rp_origin))
  (guess (step ?s1) (g $?lista-guess))
=>
  (assert trynext)
  (assert (origin-state (origin $?lista-guess) (right-placed ?rp_before)))
)

(defrule four-to-all-right-zero-to-zero
  ?ph <- (phase (name four-to-all-right))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1) (right-placed ?rp_before) (miss-placed ?mp))
  (origin-state (origin $?lista-origin) (right-placed ?rp_origin) (moved-before-primo ?primo-scambio) (moved-before-secondo ?secondo-scambio))
  (test (eq ?rp_before ?rp_origin))
  (guess (step ?s1) (g $?lista-guess))
=>
  (bind $?not-moved-before (delete-member$ (create$ 1 2 3 4) ?primo-scambio ?secondo-scambio))
  (bind ?primo-scambio-next (nth$ 1 $?not-moved-before))
  (bind ?secondo-scambio-next (nth$ 2 $?not-moved-before))

  (bind ?value-primo-scambio (nth$ ?primo-scambio ?lista-origin))
  (bind ?value-secondo-scambio (nth$ ?secondo-scambio ?lista-origin))
  (bind ?value-primo-scambio-next (nth$ ?primo-scambio-next ?lista-origin))
  (bind ?value-secondo-scambio-next (nth$ ?secondo-scambio-next ?lista-origin))

  (bind ?changed-colors (replace$ $?lista-origin ?secondo-scambio-next ?secondo-scambio-next ?value-secondo-scambio))
  (bind ?changed-colors (replace$ $?changed-colors ?secondo-scambio ?secondo-scambio ?value-secondo-scambio-next))
  (bind ?changed-colors (replace$ $?lista-origin ?primo-scambio-next ?primo-scambio-next ?value-primo-scambio))
  (bind ?changed-colors (replace$ $?changed-colors ?primo-scambio ?primo-scambio ?value-primo-scambio-next))

  (do-for-all-facts ((?f move-ph4)) TRUE (retract (fact-index ?f)))
  (assert (moves-ph4 (primo ?primo-scambio) (secondo ?secondo-scambio)))
  (assert (moves-ph4 (primo ?primo-scambio-next) (secondo ?secondo-scambio-next)))
  (assert (guess (step ?s) (g ?changed-colors)))

  (assert (origin-state (origin $?lista-guess) (right-placed ?rp_before)))
  (modify ?ph (name four-to-all-right))
  (printout t " four-to-al-right-zero-to-zero trying " ?changed-colors crlf)

  (assert trynext)
  (pop-focus)
)






;  ------------------
;  --- INIT FACTS and GENERAL RULES---
;  ------------------
(deffacts init-facts 
  (moves-ph4 (primo 1) (secondo 2))
  (moves-ph4 (primo 1) (secondo 3))
  (moves-ph4 (primo 1) (secondo 4))
  (moves-ph4 (primo 2) (secondo 3))
  (moves-ph4 (primo 2) (secondo 4))
  (moves-ph4 (primo 3) (secondo 4))
  (phase (name begin))
  )