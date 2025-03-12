#lang racket

(require syntax-spec-v3 (for-syntax syntax/parse
                                    "runtime-data.rkt"
                                    "country-codes.rkt"))

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
      "Country code must be internationally recognized")))

(define-syntax (define-airport stx)
  (syntax-parse stx
    [(_ id:nav-id
        ;[name nav-id/c]
        [(~datum coordinates) lat:latitude lon:longitude]
        [(~datum elevation) height:elevation]
        [(~datum country) count:country]
        [(~datum size) s:size]
        [(~datum radio) (name:id freq:airport-freq) ...]
        )
     #''TODO]))

(define-syntax (define-vor stx)
  (syntax-parse stx
    [(_ id:nav-id
        ;[name nav-id/c]
        [(~datum coordinates) lat:latitude lon:longitude]
        [(~datum elevation) height:elevation]
        [(~datum country) count:country]
        [(~datum freq) f:vor-freq]
        [(~datum power) p:power])
     #''TODO]))

(define-syntax (define-plan stx)
  (syntax-parse stx
    [(_ plan-name:id route-parts ...)
     (compile-plan (attribute route-parts))]))

(begin-for-syntax
  ;; PlanSyntax -> plan/c
  (define (compile-plan route)
    (syntax-parse route
      #:datum-literals (D->)
      [(dest:nav-id) #''TODO]
      [(src:nav-id D-> rest)
       #`(cons src #,(compile-plan (attribute rest)))])))

