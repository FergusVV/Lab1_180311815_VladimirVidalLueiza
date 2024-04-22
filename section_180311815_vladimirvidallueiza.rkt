#lang racket

(provide (all-defined-out))

(require "station_180311815_vladimirvidallueiza.rkt")

; Constructor para Section
(define section
  (lambda (point1 point2 distance cost)
    (if (and (station? point1) 
             (station? point2)
             (not (equal? (get-station-id point1) (get-station-id point2)))
             (number? distance) 
             (> distance 0) 
             (number? cost) 
             (>= cost 0))
        (list point1 point2 distance cost)
        (raise "Invalid section parameters"))))

; Función de pertenencia para Section
(define (is-section? section)
  (and (list? section)
       (= (length section) 4)
       (station? (get-section-point1 section))
       (station? (get-section-point2 section))
       (number? (get-section-distance section))
       (> (get-section-distance section) 0)
       (number? (get-section-cost section))
       (>= (get-section-cost section) 0)))

(define get-section-point1
  (lambda (section)
    (first section))) 

(define get-section-point2
  (lambda (section)
    (second section))) 

(define get-section-distance
  (lambda (section)
    (third section)))

(define get-section-cost
  (lambda (section)
    (fourth section)))