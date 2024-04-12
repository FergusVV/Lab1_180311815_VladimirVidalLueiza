#lang racket

(provide station)
(provide es-station?)
(provide get-station-id)
(provide get-station-name)
(provide get-station-type)
(provide get-station-stop-time)
(provide r)
(provide m)
(provide c)
(provide t)

(define r 'r)  ; Definición previa como un símbolo.
(define m 'm)
(define c 'c)
(define t 't)

(define station (lambda (id name type stop-time)
  (if (and (integer? id)
           (string? name)
           (member type '(r m c t))  ; Asume que type será una variable ya definida como símbolo.
           (integer? stop-time)
           (> stop-time 0))
      (list id name type stop-time)
      (raise "Parámetros inválidos de estación"))))


(define es-station?(lambda (station)
  (and (list? station)                     ; Debe ser una lista
       (= (length station) 4)              ; Debe tener exactamente cuatro elementos
       (integer? (get-station-id station))     ; El id debe ser un entero
       (string? (get-station-name station))    ; El nombre debe ser una cadena
       (member (get-station-type station) '(r m c t))  ; El tipo debe ser uno de los símbolos válidos
       (integer? (get-station-stop-time station))      ; El stop-time debe ser un entero
       (> (get-station-stop-time station) 0))))

(define (get-station-id st)
  (first st))

(define (get-station-name st)
  (second st))

(define (get-station-type st)
  (third st))

(define (get-station-stop-time st)
  (fourth st))
