#lang racket

(require "runtime-data.rkt")

(define airport-text-generator/c (-> airport? string?))
(define vor-text-generator/c (-> vor? string?))

; Generates text for a single navigable object using the two provided generates
; for each type.
(define (navigable-generate-text-report airport/g vor/g navigable)
  (match navigable
    [(? airport? a) (airport/g a)]
    [(? vor? v) (vor/g v)]))

; Generates text for a whole plan, joining the text of each navigable with the newline character.
(define/contract (plan-generate-text-report airport/g vor/g plan)
  (-> airport-text-generator/c vor-text-generator/c plan? string?)
  (string-join (map (curry navigable-generate-text-report airport/g vor/g) (plan-sequence plan)) "\n"))

(provide plan-generate-text-report)

(module+ test
  (require rackunit)
  (require "./spec.rkt")

  (define (airport-name a) (symbol->string (navigable-name a)))

  (define-airport KBOS
    [coordinates 42.36197 -71.0079]
    [elevation 20]
    [country US]
    [size large]
    [radio
     ("D-ATIS" 135.0)
     ("Tower East" 132.225)
     ("Tower West" 128.8)
     ("Ground" 121.75)
     ("Clearance" 121.65)
     ("Ramp" 134.05)])

    (define-vor PVD
      [coordinates 41.5564002990723 -71.999397277832]
      [elevation 310]
      [country US]
      [frequency 110.0]
      [power high])

  (define-plan boston-pvd-boston KBOS D-> PVD D-> KBOS)
    
  ; generate-text-for-navigable is able to generate a string given an airport
  (check-equal?
   (navigable-generate-text-report airport-name airport-name KBOS)
   "KBOS")

  ; generate-text-for-navigable is able to generate a string given a VOR
  (check-equal?
   (navigable-generate-text-report airport-name airport-name PVD)
   "PVD")

  ; plan-text-generate-report is able to generate a report for multiple navigables on a plan.
  (check-equal?
   (plan-generate-text-report airport-name airport-name boston-pvd-boston)
   "KBOS\nPVD\nKBOS"))
