#lang racket

(require "station_180311815_vladimirvidallueiza.rkt")
(require "section_180311815_vladimirvidallueiza.rkt")

(define line
  (lambda (id name rail-type . sections)
    (if (and (integer? id)
             (string? name)
             (string? rail-type)
             (andmap is-section? sections))  ; Asegura que todos los elementos en 'sections' son secciones válidas.
        (list id name rail-type sections)  ; Envuelve 'sections' en una lista para mantener una estructura consistente.
        (raise "Invalid line parameters"))))

(define (is-line? line)
  (and (list? line)
       (integer? (line-id line))
       (string? (line-name line))
       (string? (line-rail-type line))
       (list? (line-sections line))  ; Asegura que las secciones son una lista.
       (andmap is-section? (line-sections line)))) 

(define line-id
  (lambda (line)
    (first line)))

(define line-name
  (lambda (line)
    (second line)))

(define line-rail-type
  (lambda (line)
    (third line)))

(define line-sections
  (lambda (line)
    (if (>= (length line) 4)
        (fourth line)
        '()))) 