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

(define (es-line? line)
  (and (list? line)
       (integer? (get-line-id line))
       (string? (get-line-name line))
       (string? (get-line-rail-type line))
       (list? (get-line-sections line))  ; Asegura que las secciones son una lista.
       (andmap is-section? (get-line-sections line)))) 

(define get-line-id
  (lambda (line)
    (first line)))

(define get-line-name
  (lambda (line)
    (second line)))

(define get-line-rail-type
  (lambda (line)
    (third line)))

(define get-line-sections
  (lambda (line)
    (if (>= (length line) 4)
        (fourth line)
        '())))

;otras funciones

(define (line-is-circular? line)
  (and (not (null? (get-line-sections line)))
       (equal? (get-station-id (get-section-point1 (first (get-line-sections line))))
               (get-station-id (get-section-point2 (last (get-line-sections line)))))))


(define (regular-line-length line)
  (apply + (map get-section-distance (get-line-sections line))))

(define (circular-line-length line)
  (apply + (map get-section-distance (get-line-sections line))))


(define (line-length line)
  (if (line-is-circular? line)
      (circular-line-length line)
      (regular-line-length line)))







