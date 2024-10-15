
(defrule random-guess-generate (declare salience 100)

  (not (random-comb (posizione _)))
  ?ph <- (phase (name random-guessing))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  (test (eq (+ ?rp ?mp) 2))
  
=>
  (bind ?value1 (nth$ (random 1 8) ?lista-colori))
  (bind ?value2 (nth$ (random 1 8) ?lista-colori))
  (bind ?value3 (nth$ (random 1 8) ?lista-colori))
  (bind ?value4 (nth$ (random 1 8) ?lista-colori))
  (assert random-comb (posizione ?value1 ?value2 ?value3 ?value4))
)

(defrule random-guess-check-comb (declare salience 90)
  ?ph <- (phase (name random-guessing))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  ?randomed <- (random-comb (posizione ?value1 ?value2 ?value3 ?value4))
  (not guess (step _) (g ?value1 ?value2 ?value3 ?value4))
  (test (neq ?value1 ?value2 ?value3 ?value4))
  (test (neq ?value2 ?value1 ?value3 ?value4))
  (test (neq ?value3 ?value2 ?value1 ?value4))
  (test (neq ?value4 ?value2 ?value3 ?value1 ))
  (test (eq (+ ?rp ?mp) 2))
=>
  (assert (guess (step ?s) (g ?value1 ?value2 ?value3 ?value4)))
  ??????
)

(defrule random-guess-check-comb (declare salience 90)
  ?ph <- (phase (name random-guessing))
  (status (step ?s&:(> ?s 0)) (mode computer))
  (answer (step ?s1&:(eq (- ?s 1) ?s1)) (right-placed ?rp) (miss-placed ?mp))
  ?randomed <- (random-comb (posizione ?value1 ?value2 ?value3 ?value4))
  (not guess (step _) (g ?value1 ?value2 ?value3 ?value4))
  (test (neq ?value1 ?value2 ?value3 ?value4))
  (test (neq ?value2 ?value1 ?value3 ?value4))
  (test (neq ?value3 ?value2 ?value1 ?value4))
  (test (neq ?value4 ?value2 ?value3 ?value1 ))
  (test (eq (+ ?rp ?mp) 2))
=>
  (assert (guess (step ?s) (g ?value1 ?value2 ?value3 ?value4)))
  (modify ?ph (name random-guessing))
  (pop-focus)
)
