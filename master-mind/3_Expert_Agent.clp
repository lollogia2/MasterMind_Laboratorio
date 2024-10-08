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

;solo allo step 1, inizializzo tutti i campi di checkcombos
(defrule initcobos
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

(defrule check-miss-placed
  (status (step ?s&:(> ?s 1)) (mode computer))
  ?ph<- (phase (number ?n&:(= ?n 2)))
  (status (step ?s))
  ?combo<-(checkcombos (combination $?prima ?k $?dopo) )
  (guess (step ?s&:(eq (- ?s 1) ?s1)) (g $?prima2 ?k $?dopo2))
  (test (neq (length$ $?prima2) (length$ $?prima)))
  (test (neq (length$ $?dopo2) (length$ $?dopo)))
=>
  (bind ?new (gensym*))
  (assert (missplaced ?new ?combo))
)

(defrule count-missplaced
  (status (step ?s&:(> ?s 1)) (mode computer))
  ?ph<- (phase (number ?n&:(= ?n 2)))
  (status (step ?s))
  ?m <- (missplaced ? ?sol)
  ?checkcombos <- (checkcombos (combination ?sol) (miss-placed ?mp))
=>
  (retract ?m)
  (bind ?new-mp (+ ?mp 1))
  (modify ?a (miss-placed ?new-mp))  
)

(defrule check-right-placed
  (status (step ?s))
  (secret-code (code $?prima ?k $?dopo) )
  (guess (step ?s) (g $?prima2  ?k $?dopo2))
  (test (eq (length$ $?prima2) (length$ $?prima)))
  (test (eq (length$ $?dopo2) (length$ $?dopo)))   
=>
  (bind ?new (gensym*))
  (assert (rightplaced ?new))
)

(defrule count-rightplaced
  (status (step ?s))
  ?a <- (answer (step ?s) (right-placed ?rp) (miss-placed ?mp))
  ?r <- (rightplaced ?)
=>
  (retract ?r)
  (bind ?new-rp (+ ?rp 1))
  (modify ?a (right-placed ?new-rp))
)





 (defrule first-move
  (status (step 0) (mode computer))
  ?ph<- (phase (number ?n&:(= ?n 0)))
  =>
  (assert (guess (step 1) (g blue green red yellow)))
  (modify ?ph (number (+ ?n 1)))
  (pop-focus)
 )

(deffacts initial-facts
  (phase (number 0))
)
