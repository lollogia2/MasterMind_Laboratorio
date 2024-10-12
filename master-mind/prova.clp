(deftemplate colore 
   (multislot nomi))

(deftemplate lista-colori
   (multislot nomi))

(deffacts lista-colors 
   (lista-colori (colors blu giallo verde rosso nero bianco marrone arancio))
)

(defrule genera-colore-casuale
   (lista-colori (colors ?lista-colori))
   =>
   (printout t "Lista dei colori: " ?lista-colori crlf)

   (bind ?colori-casuali (create$))

   (bind ?colori-vecchi (create$))
   
   (bind ?indice-casuale (random 0 (- (length$ ?lista-colori) 1)))

   (printout t "Indice casuale: " ?indice-casuale crlf)

   (bind ?colore-casuale (nth$ ?indice-casuale ?lista-colori))

   (printout t "colore casuale selezionato: " ?colore-casuale crlf)
   (bind ?colori-vecchi ?colori-casuali)

   (if (not (member$ ?colore-casuale ?colori-casuali)) then
      (bind ?colori-casuali ?colori-vecchi ?colore-casuale)

      (printout t "colori scelti finora: " ?colori-casuali crlf)
   )

   (if (< (length$ ?colori-casuali) then (break)))

   (printout t "Lista finale di colori casuali: " ?colori-casuali crlf)

)

