#lang racket

(require "runtime-data.rkt"
         "runtime-functions.rkt"
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
    (pattern (~or x:navigable-params
                  (~datum size)
                  (~datum radio)
                  ((~datum radio) name:string)
                  s:string)))
  (define-syntax-class vor-params
    (pattern (~or x:navigable-params
                  (~datum freq)
                  (~datum power)
                  s:string))))

(define-syntax (make-text-report-generator stx)
  (syntax-parse stx
    [(_ (~alt
         (~once [(~datum airport) e-air:airport-params ...])
         (~once [(~datum vor) e-vor:vor-params ...])) ...)
     (define/syntax-parse (v-air ...)
       (map (λ (p) (transform-params #'air p)) (attribute e-air)))
     (define/syntax-parse (v-vor ...)
       (map (λ (p) (transform-params #'vr p)) (attribute e-vor)))
     #'(λ (plan)
         (plan-generate-text-report
          (λ (air)
            (string-append v-air ...))
          (λ (vr)
            (string-append v-vor ...))
          plan))]))

(define-for-syntax (transform-params navigable param)
  (syntax-parse param
    [(~datum name) #`(symbol->string (navigable-name #,navigable))]
    [(~datum coordinates) #`(coord->string (navigable-coordinates #,navigable))]
    [(~datum latitude) #`(number->string (coord-lat (navigable-coordinates #,navigable)))]
    [(~datum longitude) #`(number->string (coord-lon (navigable-coordinates #,navigable)))]
    [(~datum elevation) #`(format "~a ft" (navigable-elevation #,navigable))]
    [(~datum country) #`(symbol->string (navigable-country #,navigable))]
    [(~datum size) #`(symbol->string (airport-size #,navigable))]
    [(~datum radio) #`(radios->string (airport-radio #,navigable))]
    [((~datum radio) name:string)
     #`(format
        "~a MHz"
        (khz->mghz
         (hash-ref (airport-radio #,navigable) name
                   (λ () (raise-syntax-error
                          'make-text-report-generator
                          "Unknown radio"
                          #'name)))))]
    [(~datum freq) #`(format "~a MHz" (khz->mghz (waypoint-freq #,navigable)))]
    [(~datum power) #`(symbol->string (waypoint-power #,navigable))]
    [s:string #'s]))

(provide plan-generate-text-report
         arbitrary-generate-report
         make-text-report-generator)

(module+ test
  (require rackunit)
  (require syntax/macro-testing)
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
   "KBOS\nPVD\nKBOS")

  ; Test make text generator calls correct generator lambda for type
  (check-equal?
   ((make-text-report-generator
     [airport "airport"]
     [vor "vor"])
    boston-pvd-boston)
   "airport\nvor\nairport")
  
  ; Test transforming params happens correctly.
  
  (check-equal?
   (phase1-eval (transform-params #'KBOS #'name))
   '(symbol->string (navigable-name KBOS)))

  (check-equal?
   (phase1-eval (transform-params #'KBOS #'coordinates))
   '(coord->string (navigable-coordinates KBOS)))

  (check-equal?
   (phase1-eval (transform-params #'KBOS #'latitude))
   '(number->string (coord-lat (navigable-coordinates KBOS))))

  (check-equal?
   (phase1-eval (transform-params #'KBOS #'longitude))
   '(number->string (coord-lon (navigable-coordinates KBOS))))

  (check-equal?
   (phase1-eval (transform-params #'KBOS #'elevation))
   '(format "~a ft" (navigable-elevation KBOS)))

  (check-equal?
   (phase1-eval (transform-params #'KBOS #'country))
   '(symbol->string (navigable-country KBOS)))

  (check-equal?
   (phase1-eval (transform-params #'KBOS #'size))
   '(symbol->string (airport-size KBOS)))

  (check-equal?
   (phase1-eval (transform-params #'KBOS #'country))
   '(symbol->string (navigable-country KBOS)))

  (check-equal?
   (phase1-eval (transform-params #'KBOS #'radio))
   '(radios->string (airport-radio KBOS)))

  (check-equal?
   (phase1-eval (transform-params #'PVD #'freq))
   '(format "~a MHz" (khz->mghz (waypoint-freq PVD))))

  (check-equal?
   (phase1-eval (transform-params #'PVD #'power))
   '(symbol->string (waypoint-power PVD)))
  
  ; Test make-text-generator calls transformers

  (check-equal?
   ((make-text-report-generator
    [airport name]
    [vor power])
    boston-pvd-boston)
   "KBOS\nhigh\nKBOS")
  )