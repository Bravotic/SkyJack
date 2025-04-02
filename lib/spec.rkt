#lang racket

(require syntax-spec-v3
         "runtime-data.rkt"
         "runtime-functions.rkt"
         (for-syntax syntax/parse
                     syntax/id-table
                     "runtime-data.rkt"
                     "runtime-functions.rkt"
                     "country-codes.rkt"))

(provide (all-defined-out))

(begin-for-syntax
  ;; (symbol-table NavIdSyntax Coord)
  (define navigable-coords (make-free-id-table))
  ;; (symbol-table PlanIdSyntax (cons Coord Coord))
  (define plan-coords (make-free-id-table))
  ;; maximum distance between navigable points in a plan in nautical miles
  (define MAX-JUMP 100)

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
      (vor-freq/c (mghz->khz (syntax->datum #'x)))
      "Vor frequency must be in the range of 108-118MHz in increments of 0.05MHz"))
  (define-syntax-class airport-freq
    (pattern x:number
      #:fail-unless
      (airport-freq/c (mghz->khz (syntax->datum #'x)))
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
         ) ...)
     #;(free-id-table-set! navigable-coords #'id (coord (syntax->datum #'lat)
                                                        (syntax->datum #'lon)))
     #'(begin
         (define id (airport 'id (coord lat lon) height 'count 's
                             (make-immutable-hash
                              (list (cons name (mghz->khz freq)) ...))))
         (begin-for-syntax
           (free-id-table-set! navigable-coords #'id (coord lat lon))))]))

(define-syntax (define-vor stx)
  (syntax-parse stx
    [(_ id:nav-id
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
         ) ...)
     #;(free-id-table-set! navigable-coords #'id (coord (syntax->datum #'lat)
                                                        (syntax->datum #'lon)))
     #'(begin
         (define id (vor 'id (coord lat lon) height 'count (mghz->khz f) 'p))
         (begin-for-syntax
           (free-id-table-set! navigable-coords #'id (coord lat lon))))]))

(define-syntax (define-plan stx)
  (syntax-parse stx
    [(_ plan-name:id route-parts ...)
     (define route-eles (compile-plan (attribute route-parts)))
     (check-max-distance! route-eles)
     (define/syntax-parse (nav ...)
       route-eles)
     (define plan-loc (get-plan-coords route-eles))
     (define plan-src (car plan-loc))
     (define plan-dst (cdr plan-loc))
     (define plan-src-lat (coord-lat plan-src))
     (define plan-src-lon (coord-lon plan-src))
     (define plan-dst-lat (coord-lat plan-dst))
     (define plan-dst-lon (coord-lon plan-dst))
     #`(begin
         (define plan-name (plan (expand-plans (list nav ...))))
         (begin-for-syntax
           (free-id-table-set! plan-coords #'plan-name
                               (cons (coord #,plan-src-lat #,plan-src-lon)
                                     (coord #,plan-dst-lat #,plan-dst-lon)))))]))

(begin-for-syntax
  ;; plan := nav-id
  ;;       | nav-id D-> plan
  ;; PlanSyntax -> (List NavIdSyntax)
  (define (compile-plan route)
    (syntax-parse route
      #:datum-literals (D->)
      [(dest:id) (list #'dest)]
      [(src:id D-> rest ...)
       (cons #'src (compile-plan (attribute rest)))]))

  ;; Errors if any nav-ids are farther than the max distance to the next navigable
  ;; (List NavIdSyntax) -> Void
  (define (check-max-distance! plan-eles)
    (cond [(and (pair? plan-eles)
                (pair? (cdr plan-eles)))
           (check-dist! (car plan-eles) (car (cdr plan-eles)))
           (check-max-distance! (cdr plan-eles))]
          [else (void)]))

  ;; (Union NavIdSyntax PlanIdSyntax) (Union NavIdSyntax PlanIdSyntax) -> Void
  (define (check-dist! src dst)
    (let ([src-loc (get-dst-coords src)]
          [dst-loc (get-src-coords dst)])
      (unless (closer-than src-loc dst-loc MAX-JUMP)
        (raise-syntax-error
         'plan
         (format "Plan segment makes a jump longer than ~a nautical miles: ~a"
                 MAX-JUMP
                 (distance-between src-loc dst-loc))
         src
         dst))))

  ;; (Union NavIdSyntax PlanIdSyntax) -> Coord
  (define (get-src-coords path-ele)
    (free-id-table-ref
     navigable-coords path-ele
     (λ () (car (free-id-table-ref
                 plan-coords path-ele
                 (λ () (raise-syntax-error
                        'plan
                        "Plan segment is not a predefined navigable or plan"
                        path-ele)))))))
  ;; (Union NavIdSyntax PlanIdSyntax) -> Coord
  (define (get-dst-coords path-ele)
    (free-id-table-ref
     navigable-coords path-ele
     (λ () (cdr (free-id-table-ref
                 plan-coords path-ele
                 (λ () (raise-syntax-error
                        'plan
                        "Plan segment is not a predefined navigable or plan"
                        path-ele)))))))
  
  ;; (List NavIdSyntax) -> (Cons Coord Coord)
  (define (get-plan-coords plan-eles)
    (cons
     (get-src-coords (first-ele plan-eles))
     (get-dst-coords (last-ele plan-eles))))
  
  ;; (List NavIdSyntax) -> NavIdSyntax
  (define (first-ele plan-eles)
    (if (pair? plan-eles)
        (car plan-eles)
        (error 'plan "empty plan")))
  
  ;; (List NavIdSyntax) -> NavIdSyntax
  (define (last-ele plan-eles)
    (cond [(and (pair? plan-eles)
                (null? (cdr plan-eles)))
           (car plan-eles)]
          [(pair? plan-eles)
           (last-ele (cdr plan-eles))]
          [else (error 'plan "empty plan")])))

