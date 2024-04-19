#lang racket

(require "pcar_180311815_vladimirvidallueiza.rkt")
(require "train_180311815_vladimirvidallueiza.rkt")

(define (subway id name . optional-trains)
  (if (and (integer? id)                 ; Verifica que el ID sea un entero.
           (string? name))               ; Verifica que el nombre sea una cadena.
      (list id name (if (null? optional-trains) '() (first optional-trains))) ; Usa una lista vacía si no se proporcionan trenes, de lo contrario usa el argumento proporcionado.
      (raise "Invalid input types")))

; Funciones de acceso para elementos de subway
(define (subway-id subway)
  (first subway))

(define (subway-name subway)
  (second subway))

(define (subway-trains subway)
  (third subway))

(define (subway-add-train sub . trains)
  ;; Función recursiva interna para añadir trenes uno por uno
  (define (add-trains current-subway remaining-trains)
    (if (null? remaining-trains)  ; Si no hay más trenes para añadir, devolver el subway actual
        current-subway
        ;; Llamar recursivamente con el nuevo subway y el resto de trenes
        (add-trains (subway (subway-id current-subway)
                            (subway-name current-subway)
                            (append (subway-trains current-subway) (list (first remaining-trains))))
                    (rest remaining-trains))))
  ;; Iniciar la recursividad con el subway original y la lista completa de trenes
  (add-trains sub trains))