#lang racket

(provide (all-defined-out))

(define (driver id name train-maker)
  (if (and (integer? id)                      ; Verifica que el ID sea un entero.
           (string? name)                     ; Verifica que el nombre sea una cadena.
           (string? train-maker))             ; Verifica que el fabricante del tren sea una cadena.
      (list id name train-maker)              ; Crea la estructura del conductor si todos los tipos son correctos.
      (raise "Invalid input types")))         ; Lanza un error si alguno de los tipos no es correcto.

;;Funcion de pertencia

(define (driver? driver)
  (and (list? driver)                                  ; Debe ser una lista.
       (= (length driver) 3)                           ; Debe tener exactamente tres elementos.
       (integer? (driver-id driver))                   ; El ID debe ser un entero.
       (string? (driver-name driver))                  ; El nombre debe ser una cadena.
       (string? (driver-train-maker driver))))         ; El fabricante del tren debe ser una cadena.

;; Selectoras de acceso para elementos del driver
(define (driver-id driver) (first driver))
(define (driver-name driver) (second driver))
(define (driver-train-maker driver) (third driver))
