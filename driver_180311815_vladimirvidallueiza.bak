#lang racket


(define (driver id name train-maker)
  (if (and (integer? id)                      ; Verifica que el ID sea un entero.
           (string? name)                     ; Verifica que el nombre sea una cadena.
           (string? train-maker))             ; Verifica que el fabricante del tren sea una cadena.
      (list id name train-maker)              ; Crea la estructura del conductor si todos los tipos son correctos.
      (raise "Invalid input types")))         ; Lanza un error si alguno de los tipos no es correcto.