#lang racket

(require "country-codes.rkt" (for-syntax syntax/parse))
(provide (all-defined-out)
         (all-from-out "country-codes.rkt"))

(define (nav-id/c x)
  (and (symbol? x)
       (regexp-match? #rx"^[A-Z0-9:-]+$" (symbol->string x))))

(define latitude/c (between/c -90 90))
(define longitude/c (between/c -180 180))

(define size/c (one-of/c 'small 'medium 'large))

(define (mghz->khz x) (floor (* x 1000)))
(define (khz->mghz x) (/ x 1000.))

(define (vor-freq/c x)
  (and (integer? x)
       (>= x 108000)
       (<= x 118000)
       (integer? (/ x 50))))
(define airport-freq/c (and/c integer?
                              (>=/c 118000)
                              (<=/c 137000)))
(define elevation/c real?)
(define power/c (one-of/c 'low 'medium 'high))

(struct/contract coord ([lat latitude/c] [lon longitude/c])
                 #:transparent)
;(struct coord [lat lon])
;(define coord/c (struct/dc coord [lat latitude/c] [lon longitude/c]))

(struct/contract navigable ([name nav-id/c]
                            [coordinates coord?]
                            [elevation elevation/c]
                            [country country-code/c])
                 #:transparent)
;(struct navigable [name coordinates elevation country])
#;(define navigable/c
    (struct/dc navigable
               [name nav-id/c]
               [coordinates coord/c]
               [elevation elevation/c]
               [country country-code/c]))

(struct/contract airport navigable ([size size/c]
                                    [radio (hash/c string? airport-freq/c)])
                 #:transparent)
;(struct airport navigable [size radio])
#;(define airport/c
    (and/c navigable/c
           (struct/dc airport
                      [size size/c]
                      [radio (hash/c string? airport-freq/c)])))
(define airport-name navigable-name)
(define airport-coordinates navigable-coordinates)
(define airport-elevation navigable-elevation)
(define airport-country navigable-country)

(struct/contract waypoint navigable ([freq vor-freq/c]
                                     [power power/c])
                 #:transparent)
;(struct waypoint navigable [freq power])
#;(define waypoint/c
    (and/c navigable/c
           (struct/dc waypoint
                      [freq vor-freq/c]
                      [power power/c])))
(define waypoint-name navigable-name)
(define waypoint-coordinates navigable-coordinates)
(define waypoint-elevation navigable-elevation)
(define waypoint-country navigable-country)

(struct/contract vor waypoint ()
                 #:transparent)
;(struct vor waypoint [])
#;(define vor/c
    (and/c waypoint/c
           (struct/dc vor)))
(define vor-freq waypoint-freq)
(define vor-power waypoint-power)
(define vor-name waypoint-name)
(define vor-coordinates waypoint-coordinates)
(define vor-elevation waypoint-elevation)
(define vor-country waypoint-country)

(struct/contract vor-dme vor ()
                 #:transparent)
;(struct vor-dme vor [])
#;(define vor-dme/c
    (and/c waypoint/c
           (struct/dc vor-dme)))
(define vor-dme-freq vor-freq)
(define vor-dme-power vor-power)
(define vor-dme-name vor-name)
(define vor-dme-coordinates vor-coordinates)
(define vor-dme-elevation vor-elevation)
(define vor-dme-country vor-country)

(define nav-point?
  (or/c airport? waypoint?)
  #;(or/c airport/c waypoint/c))

(struct/contract plan ([sequence (listof nav-point?)])
                 #:transparent)
;(struct plan [sequence])
#;(define plan/c
    (struct/dc plan
               [sequence (listof nav-point/c)]))
