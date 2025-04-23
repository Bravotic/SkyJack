#lang racket

(require "../lib/spec.rkt" "../lib/text-generator.rkt"
         "../lib/runtime-data.rkt" "../lib/runtime-functions.rkt")
(require "./plan_reuse.rkt")

(define-plan short-flight
  boston-north-departure
  D-> AUG
  D-> KBGR)

(display (make-report short-flight))