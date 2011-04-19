(deftemplate question
    (slot factor (default none))
    (slot text (default none))
    (slot pre-condition (default none))
    (multislot choices (default yes no))
    (slot category)
)

(deftemplate answer
    (slot known-factor (default none))
    (slot value (default none))
)

(deftemplate system-status
    (slot status)
    (slot current-category)
)

(deftemplate answer-value
	(slot question)
    (slot answer)
    (slot value)
)

(deffunction ask-question
    (?question ?choices)
    (printout t ?question " " ?choices ": ")
    (bind ?answer (read))
    (if (not (member$ ?answer ?choices))
        then (printout t "Invalid option!" crlf)
        return (ask-question ?question ?choices)
        else (return ?answer))
)

(defrule ask-questions
    (system-status (status elicitation) (current-category ?category))
    ?q <- (question 
	        (text ?question)
	    	(factor ?factor)
	    	(choices $?choices)
        	(pre-condition none)
        	(category ?category)
          )
    	  (not (answer (known-factor ?factor)))
    =>
    (assert (answer 
            	(known-factor ?factor)
    			(value (ask-question ?question $?choices)))
    )
    (retract ?q)
)

(defrule ask-questions-with-preconditions
    (system-status (status elicitation) (current-category ?category))
    ?q <- (question 
	        (text ?question)
	    	(factor ?factor)
	    	(choices $?choices)
        	(pre-condition ?precondition)
        	(category ?category)
          )
    	  (not (answer (known-factor ?factor)))
    	  (answer (known-factor ?precondition))
    =>
    (assert (answer 
            	(known-factor ?factor)
    			(value (ask-question ?question $?choices)))
    )
    (retract ?q)
)

(defrule choose-category
	?status <- (system-status (status elicitation) (current-category ?current-category))
    (not (question (category ?current-category)))
    (question (category ?next-category))
	=>
	(assert (system-status (status elicitation) (current-category ?next-category)))
    (retract ?status)
)

(defrule end-elicitation
    ?s <- (system-status (status elicitation))
    (not (question (factor ?)))
    =>
    (printout t "Done with elicitation :D")
    (retract ?s)
    (assert (system-status (status tally)))
)

(defglobal
    ?*score* = 0
)

(deffacts status
    (system-status
        (status elicitation)
        (current-category access-control)
    )
)

(deffacts questions
    ;Access Control
    (question (factor limit-access) (text "Does the system limit access to authorized users?") (category access-control))
    
    ;Awareness and Training
    (question (factor manager-awareness) (text "Are managers aware of system security risks?") (category awareness-training))
    (question (factor user-awareness) (text "Are users aware of system security risks?") (pre-condition manager-awareness) (category awareness-training))
    (question (factor training) (text "Are personnel adequately trained to carry out security-related duties?") (category awareness-training))
)

(deffacts answers
    ; Access Control
    (answer-value (question limit-access) (answer yes) (value 1))
    
    ;Awareness and Training
    (answer-value (question manager-awareness) (answer yes) (value 1))
    (answer-value (question user-awareness) (answer yes) (value 1))
    (answer-value (question training) (answer yes) (value 1))
)

(facts)