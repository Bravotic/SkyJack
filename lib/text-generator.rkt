#lang racket

(require "runtime-data.rkt"
         racket/stxparam
         (for-syntax syntax/parse))

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
  (string-join (map (curry navigable-generate-text-report airport/g vor/g) (plan-sequence plan))
               "\n"))

(define/contract (arbitrary-generate-report plan airport-gen vor-gen combiner)
  (-> plan?
      (-> airport? any/c #;X)
      (-> vor? any/c #;X)
      (-> (listof any/c #;X) any/c #;Y)
      any/c #;Y)
  (combiner
   (map
    (λ (nv)
      (match nv
        [(? airport? a) (airport-gen a)]
        [(? vor? a) (vor-gen a)]))
    (plan-sequence plan))))

(begin-for-syntax
  (define-syntax-class navigable-params
    (pattern (~or (~datum name)
                  (~datum coordinates)
                  (~datum latitude)
                  (~datum longitude)
                  (~datum elevation)
                  (~datum country))))
  (define-syntax-class airport-params
    (pattern (~or navigable-params
                  (~datum size)
                  (~datum radio)
                  ((~datum radio) name:string)
                  s:string)))
  (define-syntax-class vor-params
    (pattern (~or navigable-params
                  (~datum freq)
                  (~datum power)
                  s:string))))

(define-syntax (make-text-report-generator stx)
  (syntax-parse stx
    [(_ (~alt
         (~once [(~datum airport) e-air:airport-params ...])
         (~once [(~datum vor) e-vor:vor-params ...])) ...)
     #'(λ (plan)
         (plan-generate-text-report
          (λ (air)
            #;(define-syntax-parameter ...)
            (string-append e-air ...))
          (λ (vr)
            #;(define-syntax-parameter ...)
            (string-append e-vor ...))
          plan))]))

(provide plan-generate-text-report
         arbitrary-generate-report)

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
