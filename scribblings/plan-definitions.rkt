#lang racket

(require skyjack "./nav-definitions.rkt")
(provide (all-defined-out))

(define-plan boston-to-new-york-city
  KBOS
  D-> PVD
  D-> ORW
  D-> CCC
  D-> KJFK)

(define-plan boston-north-departure
  KBOS
  D-> PSM)

(define-plan boston-to-bangor
  boston-north-departure
  D-> ENE
  D-> AUG
  D-> BGR
  D-> KBGR)

(define-plan boston-to-concord
  boston-north-departure
  D-> CON
  D-> KCON)

(define-plan cali
 SFO)

(define-plan short-flight
  boston-north-departure
  D-> AUG
  D-> KBGR)