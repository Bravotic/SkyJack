#lang racket

(require skyjack "./nav-definitions.rkt" "./plan-definitions.rkt")
(provide (all-defined-out))

(define basic-report
   (make-text-report-generator
    [airport name ":\n"
     elevation " - " coordinates "\n"
     (radio "Clearance") "\n"
     "----------------------------------------------"]
    [vor name ":\n"
     country " - " power "\n"
     "----------------------------------------------"]))
