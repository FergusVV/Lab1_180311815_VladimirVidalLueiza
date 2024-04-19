#lang racket

(require "pcar_180311815_vladimirvidallueiza.rkt")

(define train
  (lambda (id maker rail-type speed station-stay-time . pcars)
    (if (and (integer? id)                            ; Verifica que el ID sea un entero.
             (string? maker)                          ; Verifica que el fabricante sea una cadena de texto.
             (string? rail-type)                      ; Verifica que el tipo de riel sea una cadena de texto.
             (number? speed)                          ; Verifica que la velocidad sea un número.
             (number? station-stay-time)              ; Verifica que el tiempo de estancia sea un número.
             (positive? speed)                        ; Verifica que la velocidad sea positiva.
             (>= station-stay-time 0)                 ; Verifica que el tiempo de estancia no sea negativo.
             (if (null? pcars)                        ; Si no hay carros, omite verificaciones de carros.
                 #t
                 (and (consistent-model? pcars)       ; Verifica la consistencia del modelo entre todos los carros.
                      (valid-train-car-order? pcars)))) ; Verifica el orden correcto de los carros.
        (list id maker rail-type speed station-stay-time pcars)
        (raise "Invalid train configuration"))))



;Selectores
; Funciones de acceso para los componentes del tren (se asumen definidas en el entorno).
(define (train-id train) (first train))
(define (train-maker train) (second train))
(define (train-rail-type train) (third train))
(define (train-speed train) (fourth train))
(define (train-stay-time train) (fifth train))
(define (train-cars train) (sixth train))

; Verifica que todos los carros tengan el mismo modelo. Funciona correctamente con lista vacía.
(define (consistent-model? pcars)
  (if (null? pcars)
      #t
      (let ((model (pcar-model (first pcars))))
        (andmap (lambda (pcar) (equal? (pcar-model pcar) model)) pcars))))


; Verifica que todos los carros intermedios sean de tipo central. Funciona correctamente con lista vacía.
(define (all-central-car-types? pcars)
  (if (null? pcars)
      #t
      (and (equal? (pcar-type (first pcars)) ct)
           (all-central-car-types? (rest pcars)))))


; Verifica el orden correcto de los carros en el tren. Funciona correctamente con lista vacía.
(define (valid-train-car-order? pcars)
  (or (null? pcars)  ; Si no hay carros, devuelve verdadero.
      (and (equal? (pcar-type (first pcars)) tr)  ; El primer carro debe ser terminal.
           (equal? (pcar-type (last pcars)) tr)  ; El último carro debe ser terminal.
           (all-central-car-types? (rest (reverse (rest (reverse pcars))))))))  ; Carros intermedios deben ser centrales.





; Función auxiliar para insertar un carro en una lista de carros en una posición específica.
(define (insert-pcar cars pcar position)
  (cond ((null? cars) (if (= position 0) (list pcar) (raise "Posición fuera de rango")))
        ((= position 0) (cons pcar cars))
        (else (cons (first cars) (insert-pcar (rest-of-list cars) pcar (- position 1))))))

; Función para añadir un carro a un tren en una posición dada.
(define (train-add-car train pcar position)
  (if (< position 0)
      (raise "Posicion inválida")
      (make-train (train-id train)
                  (train-maker train)
                  (train-rail-type train)
                  (train-speed train)
                  (train-stay-time train)
                  (insert-pcar (train-cars train) pcar position))))

(define (make-train id maker rail-type speed stay-time cars)
  (list id maker rail-type speed stay-time cars))

;;Funcion que remueve

(define (train-remove-car train position)
  (if (or (< position 0) (>= position (length (train-cars train))))
      (raise "Posicion fuera de rango")
      (make-train (train-id train)
                  (train-maker train)
                  (train-rail-type train)
                  (train-speed train)
                  (train-stay-time train)
                  (remove-car (train-cars train) position 0))))

(define remove-car
  (lambda (cars position acum)
    (if (null? cars)
        '()  ; Si no hay mas carros retorna la lista vacía
        (if (= acum position)
            (rest cars)  ; Si es la posicion del carro a eliminar, omite este carro
            (cons (first cars) (remove-car (rest cars) position (+ acum 1)))))))  ; De lo contrario sigue buscando


;;pertencia

(define (train? train)
  (and (list? train)                                  ; Verifica que train sea una lista.
       (= (length train) 6)                           ; Verifica que train tenga exactamente 6 elementos.
       (integer? (train-id train))                    ; Verifica que el ID sea un entero.
       (string? (train-maker train))                  ; Verifica que el fabricante sea una cadena.
       (string? (train-rail-type train))              ; Verifica que el tipo de riel sea una cadena.
       (number? (train-speed train))                  ; Verifica que la velocidad sea un número.
       (number? (train-stay-time train))              ; Verifica que el tiempo de estancia sea un número.
       (not (null? (train-cars train)))               ; Verifica que haya al menos un carro.
       (unique-ids? (train-cars train) '())               ; Verifica que todos los IDs de carros sean únicos, se entrega como parametro una lista vacia
       (valid-structure? (train-cars train))))  ; Verifica la estructura del tren según los requisitos.

(define (valid-structure? pcars)
  (and (>= (length pcars) 2)  ; Debe tener al menos dos carros.
       (equal? (pcar-type (first pcars)) tr)  ; El primer carro debe ser terminal.
       (equal? (pcar-type (last pcars)) tr)   ; El último carro debe ser terminal.
       (valid-central-car-types? (rest (reverse (rest (reverse pcars))))))) ; Verifica que carros intermedios sean centrales.

(define (valid-central-car-types? pcars)
  (if (null? pcars)
      #t  ; Si no hay carros intermedios, es válido (caso Tc-Tc).
      (andmap (lambda (pcar) (equal? (pcar-type pcar) ct)) pcars)))  ; Todos los carros intermedios deben ser centrales.


(define (unique-ids? pcars visto)
  (cond ((null? pcars) #t)                           ; Si no hay mas carros, todos los IDs eran únicos.
        ((member (pcar-id (first pcars)) visto) #f)     ; Si el ID actual ya fue visto, retorna falso.
        (else (unique-ids? (rest pcars)               ; De lo contrario, sigue revisando.
                            (cons (pcar-id (first pcars)) visto)))))  ; Añade el ID actual a la lista de vistos.
;;Función capacidad tren

(define (train-capacity train)
  (sum-car-capacities (train-cars train)))

(define (sum-car-capacities pcars)
  (if (null? pcars)
      0  ; Si no hay más carros, la suma es 0.
      (+ (pcar-capacity (car pcars))  ; Suma la capacidad del carro actual.
         (sum-car-capacities (cdr pcars)))))  ; Recursividad con el resto de los carros.


