(deftemplate question
    (slot factor (default none))
    (slot text (default none))
    (slot pre-condition (default none))
    (multislot choices (default yes no))
    (slot category)
    (slot asked (default FALSE))
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

(deftemplate category-results
	(slot category)
    (slot score (default 0))
    (slot reported (default FALSE))
    (slot title)
)

(deftemplate result
    (slot category)
    (slot min)
    (slot max)
    (slot reason)
    (slot fuzzy-score)
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
        	(asked FALSE)
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
    (assert (question
            	(text ?question)
            	(factor ?factor)
            	(choices $?choices)
            	(pre-condition none)
            	(category ?category)
            	(asked TRUE)
            )
     )
)

(defrule ask-questions-with-preconditions
    (system-status (status elicitation) (current-category ?category))
    ?q <- (question
        	(asked FALSE)
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
    (assert (question
        	(text ?question)
        	(factor ?factor)
        	(choices $?choices)
        	(pre-condition ?precondition)
        	(category ?category)
        	(asked TRUE)
        )
     )
)

(defrule choose-category
	?status <- (system-status (status elicitation) (current-category ?current-category))
    (not (question (category ?current-category) (asked FALSE)))
    (question (category ?next-category) (asked FALSE))
	=>
	(assert (system-status (status elicitation) (current-category ?next-category)))
    (retract ?status)
)

(defrule end-elicitation
    ?s <- (system-status (status elicitation))
    (not (question (asked FALSE)))
    =>
    (retract ?s)
    (assert (system-status (status tally)))
)

(defglobal
    ?*score* = 0
)

(defrule tally-results-value
    (system-status (status tally))
    ?answer <- (answer (known-factor ?factor) (value ?value))
    (answer-value (question ?factor) (answer ?value) (value ?answer-value))
    (question (factor ?factor) (category ?category))
    ?category-results <- (category-results (category ?category) (score ?score) (title ?title))
    =>
    (retract ?answer)
    (bind ?*score* (+ ?*score* ?answer-value))
    (retract ?category-results)
    (assert (category-results
            	(category ?category)
            	(score (+ ?score ?answer-value))
            	(title ?title)
            )
    )
)

(defrule tally-results-no-value
    (system-status (status tally))
    ?answer <- (answer (known-factor ?factor) (value ?value))
    (not (answer-value (question ?factor) (answer ?value) (value ?answer-value)))
    =>
    (retract ?answer)
)

(defrule end-tally
    ?s <- (system-status (status tally))
    (not (answer))
    =>
    (retract ?s)
    (assert (system-status (status report)))
)

(defrule report
    (system-status (status report))
    ?category-result <- (category-results (reported FALSE) (category ?category) (title ?title) (score ?score))
    (result (category ?category) (min ?min) (max ?max) (reason ?reason) (fuzzy-score ?fuzzy-score))
    (test (and (>= ?score ?min) (<= ?score ?max)))
    =>
    (retract ?category-result)
    (printout t crlf "Results for " ?title " category: " crlf "Rating: " ?fuzzy-score crlf)
    (printout t "Raw Score: " ?score crlf "Reasoning:" crlf ?reason crlf)
)

; Facts ;

(deffacts status
    (system-status
        (status elicitation)
        (current-category access-control)
    )
)

(deffacts categories
    (category-results (category access-control) (title "Access Control"))
    (category-results (category awareness-training) (title "Awareness and Training"))
    (category-results (category audit-accountability) (title "Audit and Accountability"))
)

(deffacts questions
    ;Access Control
    (question (factor limit-access) (text "Does the system limit access to authorized users?") (category access-control))
    
    ;Awareness and Training
    (question (factor manager-awareness) (text "Are managers aware of system security risks?") (category awareness-training))
    (question (factor user-awareness) (text "Are users aware of system security risks?") (pre-condition manager-awareness) (category awareness-training))
    (question (factor training) (text "Are personnel adequately trained to carry out security-related duties?") (category awareness-training))
    
    ;Audit and Accountability
    (question (factor create-audit) (text "Are audit records created?") (pre-condition audit) (category audit-accountability))
    (question (factor protect-audit) (text "Are audit records protected?") (pre-condition create-audit) (category audit-accountability))
    (question (factor retain-audit) (text "Are audit records retained") (pre-condition protect-audit) (category audit-accountability))
    (question (factor audit) (text "Can actions of users be uniquely traced back?") (category audit-accountability))
)

(deffacts answers
    ; Access Control
    (answer-value (question limit-access) (answer yes) (value 1))
    
    ;Awareness and Training
    (answer-value (question manager-awareness) (answer yes) (value 1))
    (answer-value (question user-awareness) (answer yes) (value 1))
    (answer-value (question training) (answer yes) (value 1))
    
    ;Audit and Accountability
    (answer-value (question create-audit) (answer yes) (value 1))
    (answer-value (question protect-audit) (answer yes) (value 1))
    (answer-value (question retain-audit) (answer yes) (value 1))
    (answer-value (question audit) (answer yes) (value 1))
)

(deffacts results
    ; Access Control
    (result (category access-control) (fuzzy-score "Poor") (min 0) (max 0) (reason "Should limit access to authorized users"))
    (result (category access-control) (fuzzy-score "Excellent") (min 1) (max 1) (reason "Meets guidelines"))
    
    ;Awareness and Training
    (result (category awareness-training) (fuzzy-score "Poor") (min 0) (max 0) (reason "Should ensure all users are aware of security risks and personnel are adequately trained."))
    (result (category awareness-training) (fuzzy-score "Fair") (min 1) (max 2) (reason "Meets some of the guidelines. Should ensure all users are aware of security risks and personnel are adequately trained."))
    (result (category awareness-training) (fuzzy-score "Excellent") (min 3) (max 3) (reason "Meets guidelines"))
    
    ; Audit and Accountability
    (result (category audit-accountability) (fuzzy-score "Poor") (min 0) (max 0) (reason "Audit information should be created, protected, and retained in order to uniquely track user actions."))
    (result (category audit-accountability) (fuzzy-score "Fair") (min 1) (max 3) (reason "Meets some of the guidelines. Audit information should be created, protected, and retained in order to uniquely track user actions."))
    (result (category audit-accountability) (fuzzy-score "Excellent") (min 4) (max 4) (reason "Meets guidelines"))
)
