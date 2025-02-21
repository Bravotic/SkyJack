#lang racket

(require "country-codes.rkt" (for-syntax syntax/parse))
(provide (except-out (all-defined-out) partial-struct-guard navigable-guards))

(define (nav-id/c x)
  (and (symbol? x)
       (regexp-match? #rx"^[A-Z0-9-]+$" (symbol->string x))))

(define latitude/c (between/c -90 90))
(define longitude/c (between/c -180 180))

(define size/c (one-of/c 'small 'medium 'large))
(define (vor-freq/c x)
  (and (integer? x)
       (>= x 108000)
       (<= x 118000)
       (integer? (/ x 50))))
(define airport-freq/c (>=/c 0))
(define elevation/c real?)
(define power/c (one-of/c 'low 'medium 'high))

(define-syntax (partial-struct-guard stx)
  (syntax-parse stx
    [(_ name:id contracts ...)
     #'(define-syntax (name s)
         (syntax-parse s
           [(_ other (... ...))
            #'(struct-guard/c contracts ... other (... ...))]))]))

(struct coord [lat lon]
  #:guard (struct-guard/c latitude/c longitude/c))

(partial-struct-guard navigable-guards nav-id/c coord? elevation/c country-code/c)
(struct navigable [name coordinates elevation country]
  #:guard (navigable-guards)
  #;(struct-guard/c nav-id/c coord? country-code/c))

(struct airport navigable [size radio]
  #:guard (navigable-guards size/c (hash/c string? airport-freq/c))
  #;(struct-guard/c nav-id/c coord? country-code/c
                          size/c (hash/c string? airport-freq/c) elevation/c))

(struct waypoint navigable [freq power]
  #:guard (navigable-guards vor-freq/c))

(struct vor waypoint []
  #:guard (navigable-guards vor-freq/c))
(struct vor-dme vor []
  #:guard (navigable-guards vor-freq/c))

(define (nav-point/c x)
  (or (airport? x) (waypoint? x)))

(struct plan [start end sequence]
  #:guard (struct-guard/c nav-id/c nav-id/c (listof nav-point/c)))