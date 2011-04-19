(deftemplate question
    (slot factor (default none))
    (slot text (default none))
    (slot has-pre-condition (type SYMBOL) (default no))
    (multislot choices (default yes no))
)

(deftemplate answer
    (slot known-factor (default none))
    (slot value (default none))
)

(deffunction ask-question
    (?question ?choices)
    (printout t ?question "[" ?choices "]:")
    (bind ?answer (read))
    (if (not (member$ ?answer ?choices))
        then (printout t "Invalid option!" crlf)
        return (ask-question ?question ?choices)
        else (return ?answer))
)

(defrule ask-questions
    ?q <- (question 
	        (text ?question)
	    	(factor ?factor)
	    	(choices $?choices)
          )
    	  (not (answer (known-factor ?factor)))
    =>
    (assert (answer 
            	(known-factor ?factor)
    			(value (ask-question ?question $?choices)))
    )
    (retract ?q)
)

;(defrule tally-answers
;	?a <- (answer
;        	(known-factor ?factor)
;        	(value ?value)
;          )
;)

(defglobal
    ?*score* = 0
)

(deffacts questions "Initial Facts"
    (question (factor limit-access) (text "Does the system limit access to authorized users?"))
    (question (factor awesome) (text "Is this system awesome?"))
)