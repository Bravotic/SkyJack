#lang racket

(require syntax-spec-v3
         "runtime-data.rkt"
         (for-syntax syntax/parse
                     "runtime-data.rkt"
                     "country-codes.rkt"))

(provide (all-defined-out))

(begin-for-syntax
  ;; (symbol-table nav-id/c (ListOf NavIdSyntax))
  (define-persistent-symbol-table navigables)
  
  (define-syntax-class nav-id
    (pattern x:id
      #:fail-unless
      (nav-id/c (syntax->datum #'x))
      "Navid must be one of uppercase, digit, -, or :"))

  (define-syntax-class latitude
    (pattern x:number
      #:fail-unless
      (latitude/c (syntax->datum #'x))
      "Latitude must be a valid latitude"))
  (define-syntax-class longitude
    (pattern x:number
      #:fail-unless
      (longitude/c (syntax->datum #'x))
      "Longitude must be a valid longitude"))

  (define-syntax-class size
    (pattern x:id
      #:fail-unless
      (size/c (syntax->datum #'x))
      "Size must be one of small, medium, or large"))
  (define-syntax-class vor-freq
    (pattern x:number
      #:fail-unless
      (vor-freq/c (mghz->hertz (syntax->datum #'x)))
      "Vor frequency must be in the range of 108-118MHz in increments of 0.05MHz"))
  (define-syntax-class airport-freq
    (pattern x:number
      #:fail-unless
      (airport-freq/c (mghz->hertz (syntax->datum #'x)))
      "Airport frequency must be in the air band"))
  (define-syntax-class elevation
    (pattern x:number
      #:fail-unless
      (elevation/c (syntax->datum #'x))
      "Elevation must be a number in feet"))
  (define-syntax-class power
    (pattern x:id
      #:fail-unless
      (power/c (syntax->datum #'x))
      "Size must be one of low, medium, or high"))
  (define-syntax-class country
    (pattern x:id
      #:fail-unless
      (country-code/c (syntax->datum #'x))
      "Country code must be internationally recognized"))
  #;(define-syntax-class plan
      (pattern
        (~or dst:nav-id
             (srd:nav-id (~datum D->) rest:plan)))))

(define-syntax (define-airport stx)
  (syntax-parse stx
    [(_ id:nav-id
        ;[name nav-id/c]
        (~alt
         (~once [(~datum coordinates) lat:latitude lon:longitude]
                #:name "coordinates"
                #:too-few "Missing coordinates")
         (~once [(~datum elevation) height:elevation]
                #:name "elevation"
                #:too-few "Missing elevation")
         (~once [(~datum country) count:country]
                #:name "country"
                #:too-few "Missing country")
         (~once [(~datum size) s:size]
                #:name "size"
                #:too-few "Missing size")
         (~once [(~datum radio) (name:string freq:airport-freq) ...]
                #:name "radio"
                #:too-few "Missing radio")
         ) ...
           #;[(~datum coordinates) lat:latitude lon:longitude]
           #;[(~datum elevation) height:elevation]
           #;[(~datum country) count:country]
           #;[(~datum size) s:size]
           #;[(~datum radio) (name:string freq:airport-freq) ...]
           )
     #'(define id (airport 'id (coord lat lon) height 'count 's
                           (make-immutable-hash
                            (list (cons name (mghz->hertz freq)) ...))))]))

(define-syntax (define-vor stx)
  (syntax-parse stx
    [(_ id:nav-id
        ;[name nav-id/c]
        (~alt
         (~once [(~datum coordinates) lat:latitude lon:longitude]
                #:name "coordinates"
                #:too-few "Missing coordinates")
         (~once [(~datum elevation) height:elevation]
                #:name "elevation"
                #:too-few "Missing elevation")
         (~once [(~datum country) count:country]
                #:name "country"
                #:too-few "Missing country")
         (~once [(~datum frequency) f:vor-freq]
                #:name "frequency"
                #:too-few "Missing frequency")
         (~once [(~datum power) p:power]
                #:name "power"
                #:too-few "Missing power")
         ) ...
           #;[(~datum coordinates) lat:latitude lon:longitude]
           #;[(~datum elevation) height:elevation]
           #;[(~datum country) count:country]
           #;[(~datum frequency) f:vor-freq]
           #;[(~datum power) p:power])
     #'(define id (vor 'id (coord lat lon) height 'count (mghz->hertz f) 'p))]))

(define-syntax (define-plan stx)
  (syntax-parse stx
    [(_ plan-name:id route-parts ...)
     #`(define plan-name (plan #,(compile-plan (attribute route-parts))))]))

(begin-for-syntax
  ;; plan := nav-id
  ;;       | nav-id D-> plan
  ;; PlanSyntax -> plan/c
  (define (compile-plan route)
    (syntax-parse route
      #:datum-literals (D->)
      [(dest:nav-id) #'(list dest)]
      [(src:nav-id D-> rest ...)
       #`(cons src #,(compile-plan (attribute rest)))])))

