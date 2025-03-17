#lang racket

(require "../lib/spec.rkt")


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

(define-airport KJFK
  [coordinates 40.639447 -73.779317]
  [elevation 13]
  [country US]
  [size large]
  [radio
   ("D-ATIS" 115.1)
   ("Tower" 119.1)
   ("Tower Alt" 123.9)
   ("Ground" 121.9)
   ("Clearance" 135.05)])

(define-vor PVD
  [coordinates 41.7243003845215 -71.4296035766602]
  [elevation 49]
  [country US]
  [frequency 115.6]
  [power high])
(define-vor ORW
  [coordinates 41.5564002990723 -71.999397277832]
  [elevation 310]
  [country US]
  [frequency 110.0]
  [power high])
(define-vor CCC
  [coordinates 40.9295997619629 -72.7988967895508]
  [elevation 86]
  [country US]
  [frequency 117.2]
  [power medium])

(define-plan boston-to-new-york-city
  KBOS
  D-> PVD
  D-> ORW
  D-> CCC
  D-> KJFK
  )
