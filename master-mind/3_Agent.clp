;  ---------------------------------------------
;              --- AGENTE UMANO ---
;  ---------------------------------------------
(defmodule AGENT (import MAIN ?ALL) (import GAME ?ALL) (export ?ALL))

;  --------------
;  -- TEMPLATE --
;  --------------
(deftemplate phase
  (slot name (allowed-values submit check modifyseq))
)  

(deftemplate colors-for-solution
  (multislot present (allowed-values blue green red yellow orange white black purple) (cardinality 0 4))
  (multislot absent (allowed-values blue green red yellow orange white black purple) (cardinality 0 4))
)

(deftemplate color-list
  (multislot color (allowed-values blue green red yellow orange white black purple) (cardinality 0 8))
)

(deftemplate hystory-combination
  (multislot combination (allowed-values blue green red yellow orange white black purple) (cardinality 4 4))
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
;  --- FASE CHECK ----
;  -------------------

;controlliamo se abbiamo 4 feedback, giusti o semigiusti
(defrule check-game-response-four
 (status (step ?s&:(> ?s 0)) (mode computer))
 ?ph <- (phase (name check))
 (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
 (test (eq (+ ?rp ?mp) 4))
 =>
 (modify ?ph (name modifyseq))
)

(defrule modifysequence
  (status (step ?s&:(> ?s 0)) (mode computer))
  ?ph <- (phase (name modifyseq))
  (colors-for-solution (present $?present-colors))  ;; Estraiamo i colori presenti
  (hystory-combination $?combinations)  ;; Estraiamo tutte le combinazioni precedenti
  =>
  ;; Genera una nuova combinazione non ripetuta
  (bind ?new-combination (gen-new-combination $?present-colors $?combinations))
  ;; Se abbiamo una nuova combinazione valida
  (if ?new-combination then
    ;; Asserisci la nuova combinazione come tentativo
    (assert (guess (step ?s) (g (expand$ ?new-combination))))
    ;; Aggiungi questa combinazione all'history
    (assert (hystory-combination (combination (expand$ ?new-combination))))
    ;; Cambia la fase per verificare la nuova combinazione
    (modify ?ph (name check))
  )
)

;; Funzione per generare una nuova combinazione non ripetuta
(deffunction gen-new-combination (?present-colors ?combinations)
  (bind ?new-combination nil)
  (while (not ?new-combination)
    ;; Estrai una combinazione casuale dai colori presenti
    (bind ?candidate (permute$ (subset$ ?present-colors 4)))
    ;; Verifica se la combinazione non è già stata usata
    (if (not (member$ ?candidate ?combinations)) then
      (bind ?new-combination ?candidate)
    )
  )
  ;; Ritorna la nuova combinazione
  (return ?new-combination)
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
  (modify ?ph (name check))
  (pop-focus)
)

;come fatto iniziale parto dalla fase submit
(deffacts initial-facts
  (phase (name submit))
  (colors-for-solution)
  (color-list(color blue green red yellow orange white black purple))
)
