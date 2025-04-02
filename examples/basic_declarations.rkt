#lang racket

(require "../lib/spec.rkt" "../lib/runtime-data.rkt" "../lib/runtime-functions.rkt")

;; TODO: update expansions
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
#;(define KBOS
    (airport
     'KBOS
     (coord 42.36197 -71.0079)
     20
     'US
     'large
     (make-immutable-hash
      (list
       (cons "D-ATIS" (mghz->khz 135.0) #;135000)
       (cons "Tower East" (mghz->khz 132.225) #;132225)
       (cons "Tower West" (mghz->khz 128.8) #;128800)
       (cons "Ground" (mghz->khz 121.75) #;121750)
       (cons "Clearance" (mghz->khz 121.65) #;121650)
       (cons "Ramp" (mghz->khz 134.05) #;134050)
       ))))

(define-airport KJFK
  [radio
   ("D-ATIS" 128.725)
   ("Tower" 119.1)
   ("Tower Alt" 123.9)
   ("Ground" 121.9)
   ("Clearance" 135.05)]
  [elevation 13]
  [country US]
  [size large]
  [coordinates 40.639447 -73.779317])
#;(define KJFK
    (airport
     'KJFK
     (coord 40.639447 -73.779317)
     13
     'US
     'large
     (make-immutable-hash
      (list
       (cons "D-ATIS" (mghz->khz 128.725))
       (cons "Tower" (mghz->khz 119.1))
       (cons "Tower Alt" (mghz->khz 123.9))
       (cons "Ground" (mghz->khz 121.9))
       (cons "Clearance" (mghz->khz 135.05))))))

(define-vor PVD
  [coordinates 41.7243003845215 -71.4296035766602]
  [elevation 49]
  [country US]
  [frequency 115.6]
  [power high])
#;(define PVD (vor 'PVD (coord 41.7243003845215 -71.4296035766602)
                   49
                   'US
                   (mghz->khz 115.6) #;115600
                   'high))

(define-vor ORW
  [coordinates 41.5564002990723 -71.999397277832]
  [elevation 310]
  [country US]
  [frequency 110.0]
  [power high])
#;(define ORW (vor 'ORW (coord 41.5564002990723 -71.999397277832)
                   310
                   'US
                   (mghz->khz 110.0) #;110000
                   'high))
(define-vor CCC
  [coordinates 40.9295997619629 -72.7988967895508]
  [elevation 86]
  [country US]
  [frequency 117.2]
  [power medium])
#;(define CCC (vor 'CCC (coord 40.9295997619629 -72.7988967895508)
                   86
                   'US
                   (mghz->khz 117.2) #;117200
                   'medium))

(define-plan boston-to-new-york-city
  KBOS
  D-> PVD
  D-> ORW
  D-> CCC
  D-> KJFK)
#;(define boston-to-new-york-city
    (plan (cons KBOS (cons PVD (cons ORW (cons CCC (list KJFK)))))))
