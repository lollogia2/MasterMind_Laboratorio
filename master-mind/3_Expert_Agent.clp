;  ---------------------------------------------
;  --- ALGORITMO DI KNUTH ---
;  ---------------------------------------------
(defmodule AGENT (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

;template che contiene tutte le combinazioni possibili di colori
(deftemplate combination (multislot code (allowed-values blue green red yellow orange white black purple) (cardinality 4 4)))

;template che tiene conto della fase corrente nell'algoritmo
(deftemplate phase
(slot number
      (type INTEGER)
      (range 0 4) 
   )
)

;template di supporto che tiene conto dei rp e mp delle soluzioni rimanenti, così da poter decidere se scartarle o meno
(deftemplate checkcombos 
  (slot combination)
  (slot right-placed (type INTEGER))
  (slot miss-placed (type INTEGER))
)


(defrule human-player
  (status (step ?s) (mode human))
  =>
  (printout t "Your guess at step " ?s crlf)
  (bind $?input (readline))
  (assert (guess (step ?s) (g  (explode$ $?input)) ))
  (pop-focus)
 )

;  --------------
;  --- FASE 1 ---
;  --------------


;solo allo step 1, inizializzo tutti i campi di checkcombos
(defrule initconbos
  (status (step 1) (mode computer))
  ?ph<- (phase (number ?n&:(= ?n 1)))
  ?combo <- (combination (code $?))
  =>
  (assert (checkcombos(combination ?combo) (right-placed 0)(miss-placed 0)))
)

;quando inizio con uno step diverso dall'1, devo resettare tutti i campi rimanenti di checkcombos
(defrule resetallcombos
  (status (step ?s&:(> ?s 1)) (mode computer))
  ?ph<- (phase (number ?n&:(= ?n 1)))
  ?chcombos <- (checkcombos)
  =>
  (modify ?chcombos (right-placed 0)(miss-placed 0))
)

;finsco la fase 1 e passo alla 2
(defrule endph1 (declare (salience -10))
  ?ph<- (phase (number ?n&:(= ?n 1)))
=>
  (modify ?ph (number (+ ?n 1)))
)

;  --------------
;  --- FASE 2 ---
;  --------------

;controllo quali colori nelle combinazioni in checkcombos differiscono dai colori dalla mia possibile soluzione al turno precedente,
;creo un missplaced per ogni colore fuori posto
(defrule check-miss-placed
  (status (step ?s&:(> ?s 0)) (mode computer))
  ?ph<- (phase (number ?n&:(= ?n 2)))
  ?combo <- (combination(code $?prima ?k $?dopo))
  (guess (step ?s1&:(eq (- ?s 1) ?s1)) (g $?prima2 ?k $?dopo2))
  (test (neq (length$ $?prima2) (length$ $?prima)))
  (test (neq (length$ $?dopo2) (length$ $?dopo)))
=>
  (bind ?new (gensym*))
  (assert (missplaced ?new ?combo))
)

;conto quanti missplaced ho creato associati ad una sequenza di colore di checkcombos
(defrule count-missplaced
  (status (step ?s&:(> ?s 0)) (mode computer))
  ?ph<- (phase (number ?n&:(= ?n 2)))
  ?m <- (missplaced ? ?sol)
  ?checkcombos <- (checkcombos (combination ?sol) (miss-placed ?mp))
=>
  (retract ?m)
  (bind ?new-mp (+ ?mp 1))
  (modify ?checkcombos (miss-placed ?new-mp)) 
)

;controllo quali colori nelle combinazioni in checkcombos fanno match con i colori soluzione al turno precedente, 
;creo un rightplaced per ogni colore a posto
(defrule check-right-placed
  (status (step ?s&:(> ?s 0)) (mode computer))
  ?ph<- (phase (number ?n&:(= ?n 2)))
  ?combo <- (combination(code $?prima ?k $?dopo))
  (guess (step ?s1&:(eq (- ?s 1) ?s1)) (g $?prima2 ?k $?dopo2))
  (test (eq (length$ $?prima2) (length$ $?prima)))
  (test (eq (length$ $?dopo2) (length$ $?dopo)))   
=>
  (bind ?new (gensym*))
  (assert (rightplaced ?new ?combo))
)

;conto quanti rightplaced ho creato associati ad una sequenza di colore di checkcombos
(defrule count-rightplaced
  (status (step ?s&:(> ?s 0)) (mode computer))
  ?ph<- (phase (number ?n&:(= ?n 2)))
  ?checkcombos <- (checkcombos (combination ?sol) (right-placed ?rp))
  ?r <- (rightplaced ? ?sol)
=>
  (retract ?r)
  (bind ?new-rp (+ ?rp 1))
  (modify ?checkcombos (right-placed ?new-rp))
)
   
(defrule endph2
  (status (step ?s&:(> ?s 0)) (mode computer))
  ?ph<- (phase (number ?n&:(= ?n 2)))
=>
  (modify ?ph (number (+ ?n 1)))
)

;  --------------
;  --- FASE 3 ---
;  --------------

;rimuovo le combinazioni che non darebbero la stessa soluzione della guess
(defrule no-equal-feedback
  (status (step ?s&:(> ?s 0)) (mode computer))
  (phase (number ?n&:(= ?n 3)))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?right-answer) (miss-placed ?miss-answer))
  ?checkcombos <- (checkcombos (combination ?comb)(right-placed ?right-comb)(miss-placed ?miss-comb))
  (test (or (neq ?right-answer ?right-comb)(neq ?miss-answer ?miss-comb)))
=>
  (retract ?comb)
  (retract ?checkcombos)
)

(defrule endph3
  (status (step ?s&:(> ?s 0)) (mode computer))
  ?ph<- (phase (number ?n&:(= ?n 3)))
=>
  (modify ?ph (number (* ?n 0)))
)


;  --------------
;  --- FASE 0 ---
;  --------------
 (defrule first-move
  (status (step 0) (mode computer))
  ?ph<- (phase (number ?n&:(= ?n 0)))
  =>
  (assert (guess (step 1) (g blue green red yellow)))
  (modify ?ph (number (+ ?n 1)))
  (pop-focus)
 )


  (defrule another-move
  (status (step ?s&:(> ?s 0)) (mode computer))
  ?ph<- (phase (number ?n&:(= ?n 0)))
  =>
  ****************************
  (pop-focus)

 )

(deffacts initial-facts
  (phase (number 0))
)
