#lang racket

(require skyjack)
(provide (all-defined-out))

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
(define-airport KBGR
  [country US]
  [size medium]
  [radio
   ("D-ATIS" 127.75)
   ("Tower" 120.7)
   ("Ground" 121.9)
   ("Clearance" 135.9)]
  [coordinates 44.8074 -68.828102]
  [elevation 192])
(define-airport KCON
  [size medium]
  [radio
   ("Approach/Departure" 127.35)
   ("Clearance" 133.65)
   ("Bangor RDO" 122.3)]
  [coordinates 43.20270157 -71.50229645]
  [elevation 342]
  [country US])

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
(define-vor PSM
  [frequency 116.5]
  [power medium]
  [country US]
  [elevation 99]
  [coordinates 43.0844993591309 -70.8320007324219])
(define-vor ENE
  [frequency 117.1]
  [power high]
  [country US]
  [elevation 190]
  [coordinates 43.4257011413574 -70.6135025024414])
(define-vor AUG
  [frequency 111.4]
  [power medium]
  [country US]
  [elevation 349]
  [coordinates 44.3199996948242 -69.7966003417969])
(define-vor BGR
  [frequency 114.8]
  [power high]
  [country US]
  [elevation 359]
  [coordinates 44.8418006896973 -68.8740005493164])
(define-vor SFO
  [frequency 115.8]
  [power medium]
  [country US]
  [elevation 13]
  [coordinates 37.619499206543 -122.374000549316])
(define-vor CON
  [frequency 112.9]
  [power medium]
  [country US]
  [elevation 710]
  [coordinates 43.2197990417481 -71.5755004882813])