#lang racket

(require "country-codes.rkt" (for-syntax syntax/parse))
(provide (all-defined-out))

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

(struct coord [lat lon])
(define coord/c (struct/dc coord [lat latitude/c] [lon longitude/c]))

(struct navigable [name coordinates elevation country])
(define navigable/c
  (struct/dc navigable
             [name nav-id/c]
             [coordinates coord/c]
             [elevation elevation/c]
             [country country-code/c]))

(struct airport navigable [size radio])
(define airport/c
  (and/c navigable/c
         (struct/dc airport
                    [size size/c]
                    [radio (hash/c string? airport-freq/c)])))

(struct waypoint navigable [freq power])
(define waypoint/c
  (and/c navigable/c
         (struct/dc waypoint
                    [freq vor-freq/c]
                    [power power/c])))

(struct vor waypoint [])
(define vor/c
  (and/c waypoint/c
         (struct/dc vor)))

(struct vor-dme vor [])
(define vor-dme/c
  (and/c waypoint/c
         (struct/dc vor-dme)))

(define nav-point/c
  (or/c airport/c waypoint/c))

(struct plan [sequence])
(define plan/c
  (struct/dc plan
             [sequence (listof nav-point/c)]))
