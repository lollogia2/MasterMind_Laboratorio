

(deftemplate phase
  (slot name (allowed-values submit next-hf))
)  

(defrule random1
?ph<- (phase (name submit))
=>
(bind ?lista-colori (create$ blue green red yellow orange white black purple))
(bind ?value1  (nth$ (random 1 8) ?lista-colori))
(bind ?lista-colori (delete-member$ ?lista-colori ?value1))
(bind ?value2 (nth$ (random 1 7) ?lista-colori))
(bind ?lista-colori (delete-member$ ?lista-colori ?value2))
(bind ?value3 (nth$ (random 1 6) ?lista-colori))
(bind ?lista-colori (delete-member$ ?lista-colori ?value3))
(bind ?value4  (nth$ (random 1 5) ?lista-colori))
(printout t "Your guess at step " ?value1 ?value2 ?value3 ?value4 crlf)
)