#lang racket

(require "runtime-data.rkt")

(provide (all-defined-out))

;; Calculates the distance between two locations in nautical miles
;; uses the haversine formula for great-circle distance
(define/contract (distance-between coord1 coord2)
  (-> coord? coord? real?)
  (let* ([lat1 (coord-lat coord1)]
         [lon1 (coord-lon coord1)]
         [lat2 (coord-lat coord2)]
         [lon2 (coord-lon coord2)]
         [R 3443.92] ; radius of the earth in nautical miles
         [phi1 (* lat1 (/ pi 180))]
         [phi2 (* lat2 (/ pi 180))]
         [delta-phi (* (- lat2 lat1) (/ pi 180))]
         [delta-lam (* (- lon2 lon1) (/ pi 180))]
         [a (+ (* (sin (/ delta-phi 2)) (sin (/ delta-phi 2)))
               (* (* (cos phi1) (cos phi2))
                  (* (sin (/ delta-lam 2)) (sin (/ delta-lam 2)))))]
         [c (* 2 (asin (min 1 (sqrt a))))])
    (* R c)))

;; Checks if two navigables are closer than the requested distance (in nautical miles)
(define/contract (closer-than c1 c2 dist)
  (-> coord? coord? (>=/c 0) boolean?)
  (< (distance-between c1 c2)
     dist))

;; Expands any plans into the list of navigables
(define/contract (expand-plans lon)
  (-> (listof (or/c plan? navigable?)) (listof navigable?))
  (flatten
   (map
    (λ (pon) (if (plan? pon)
                 (plan-sequence pon)
                 pon))
    lon)))

;; Produce the readable string of the given coordinates
(define/contract (coord->string c)
  (-> coord? string?)
  (define (dms n)
    (let* ([deg (floor n)]
           [min-degs (* (- n deg) 60)]
           [min (floor min-degs)]
           [sec (* (- min-degs min) 60)])
      (values deg min sec)))
  (let* ([lat (coord-lat c)]
         [lon (coord-lon c)]
         [ns (if (< lat 0) "S" "N")]
         [ew (if (< lon 0) "W" "E")])
    (let-values ([(lat-deg lat-min lat-sec) (dms (abs lat))]
                 [(lon-deg lon-min lon-sec) (dms (abs lon))])
      (format "~a° ~a' ~a\" ~a, ~a° ~a' ~a\" ~a"
              lat-deg lat-min lat-sec ns
              lon-deg lon-min lon-sec ew))))

;; Produce a string representing a hash of radio frequencies
(define/contract (radios->string radios)
  (-> (hash/c string? number?) string?)
  (string-join
   (for/list ([(name freq) (in-hash radios)])
     (format
      "~a: ~a MHz"
      name
      (khz->mghz freq)))
   "\n"))
