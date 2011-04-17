(defrule are-lights-working
    (not (lights-working ?))
    =>
    (printout t "Are the car's lights working (yes or no)?")
    (assert (lights-working (read))))