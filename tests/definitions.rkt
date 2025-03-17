#lang racket

(require rackunit
         "../lib/spec.rkt"
         "../lib/runtime-data.rkt"
         syntax/macro-testing)

(check-exn
 #rx"define-airport: Navid must be one"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-airport bad-nav-id
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
       ("Ramp" 134.05)])))))

(check-exn
 #rx"define-airport: Latitude must be a valid latitude"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-airport KBOS
      [coordinates 1337 -420]
      [elevation 20]
      [country US]
      [size large]
      [radio
       ("D-ATIS" 135.0)
       ("Tower East" 132.225)
       ("Tower West" 128.8)
       ("Ground" 121.75)
       ("Clearance" 121.65)
       ("Ramp" 134.05)])))))

(check-exn
 #rx"define-airport: Longitude must be a valid longitude"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-airport KBOS
      [coordinates 0 -420]
      [elevation 20]
      [country US]
      [size large]
      [radio
       ("D-ATIS" 135.0)
       ("Tower East" 132.225)
       ("Tower West" 128.8)
       ("Ground" 121.75)
       ("Clearance" 121.65)
       ("Ramp" 134.05)])))))

(check-exn
 #rx"define-airport: Elevation must be a number in feet"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-airport KBOS
      [coordinates 42.36197 -71.0079]
      [elevation +20i]
      [country US]
      [size large]
      [radio
       ("D-ATIS" 135.0)
       ("Tower East" 132.225)
       ("Tower West" 128.8)
       ("Ground" 121.75)
       ("Clearance" 121.65)
       ("Ramp" 134.05)])))))

(check-exn
 #rx"define-airport: Country code must be internationally recognized"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-airport KBOS
      [coordinates 42.36197 -71.0079]
      [elevation 20]
      [country USA]
      [size large]
      [radio
       ("D-ATIS" 135.0)
       ("Tower East" 132.225)
       ("Tower West" 128.8)
       ("Ground" 121.75)
       ("Clearance" 121.65)
       ("Ramp" 134.05)])))))

(check-exn
 #rx"define-airport: Size must be one of small, medium, or large"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-airport KBOS
      [coordinates 42.36197 -71.0079]
      [elevation 20]
      [country US]
      [size really-big]
      [radio
       ("D-ATIS" 135.0)
       ("Tower East" 132.225)
       ("Tower West" 128.8)
       ("Ground" 121.75)
       ("Clearance" 121.65)
       ("Ramp" 134.05)])))))

(check-exn
 #rx"define-airport: Airport frequency must be in the air band"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-airport KBOS
      [coordinates 42.36197 -71.0079]
      [elevation 20]
      [country US]
      [size large]
      [radio
       ("D-ATIS" 108.0)
       ("Tower East" 132.225)
       ("Tower West" 128.8)
       ("Ground" 121.75)
       ("Clearance" 121.65)
       ("Ramp" 134.05)])))))

;; ------------------------------------------------------------------------

(check-exn
 #rx"define-vor: Navid must be one"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-vor bad-nav-id
      [coordinates 41.5564002990723 -71.999397277832]
      [elevation 310]
      [country US]
      [frequency 110.0]
      [power high])))))

(check-exn
 #rx"define-vor: Latitude must be a valid latitude"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-vor PVD
      [coordinates 1337 -71.999397277832]
      [elevation 310]
      [country US]
      [frequency 110.0]
      [power high])))))

(check-exn
 #rx"define-vor: Longitude must be a valid longitude"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-vor PVD
      [coordinates 0 -420]
      [elevation 310]
      [country US]
      [frequency 110.0]
      [power high])))))

(check-exn
 #rx"define-vor: Elevation must be a number in feet"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-vor PVD
      [coordinates 0 0]
      [elevation +20i]
      [country US]
      [frequency 110.0]
      [power high])))))

(check-exn
 #rx"define-vor: Country code must be internationally recognized"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-vor PVD
      [coordinates 0 0]
      [elevation 20]
      [country USA]
      [frequency 110.0]
      [power high])))))

(check-exn
 #rx"define-vor: Vor frequency must be in the range of 108-118MHz in increments of 0.05MHz"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-vor PVD
      [coordinates 0 0]
      [elevation 20]
      [country US]
      [frequency 135.0]
      [power high])))))

(check-exn
 #rx"define-vor: Size must be one of low, medium, or high"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-vor PVD
      [coordinates 0 0]
      [elevation 20]
      [country US]
      [frequency 108.0]
      [power over-9000])))))

;; ------------------------------------------------------------------------

(check-exn
 #rx"expected more terms"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-plan boston-to-new-york-city
      KBOS
      D->)))))
(check-exn
 #rx"expected the literal symbol `D->'"
 (thunk
  (convert-compile-time-error
   (thunk
    (define-plan boston-to-new-york-city
      KBOS
      KJFK)))))