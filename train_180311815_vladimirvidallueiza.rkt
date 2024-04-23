#lang racket

(provide (all-defined-out))
(require "station_180311815_vladimirvidallueiza.rkt")
(require "pcar_180311815_vladimirvidallueiza.rkt")
; TDA Train.
; Representada por una lista con
; id (int) X maker (string) X rail-type (string) X speed (positive number) X station-stay-time (positive number U {0}) X pcar* (* indica que pueden especificarse 0  o más carros  )

;---------------------Constructor---------------------
; train: función que crea una train a partir de su Id, name, type y stop-time.
; Dominio: id (int) X maker (string) X rail-type (string) X speed (positive number) X station-stay-time (positive number U {0})X pcar* (* indica que pueden especificarse 0  o más carros  )
; Recorrido: train

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
        '()))); se representa como nulo cuando no es válido


;---------------------Pertencia---------------------
; train?: comprueba que un listado de elementos cumpla con el formato de train
; Dominio: train
; Recorrido: boolean
; Uso de recursion de las funciones auxiliares
(define (train? train)
  (and (list? train)                                  ; Verifica que train sea una lista.
       (= (length train) 6)                           ; Verifica que train tenga exactamente 6 elementos.
       (integer? (get-train-id train))                    ; Verifica que el ID sea un entero.
       (string? (get-train-maker train))                  ; Verifica que el fabricante sea una cadena.
       (string? (get-train-rail-type train))              ; Verifica que el tipo de riel sea una cadena.
       (number? (get-train-speed train))                  ; Verifica que la velocidad sea un número.
       (number? (get-train-stay-time train))              ; Verifica que el tiempo de estancia sea un número.
       (not (null? (get-train-cars train)))               ; Verifica que haya al menos un carro.
       (unique-ids? (get-train-cars train) '())               ; Verifica que todos los IDs de carros sean únicos, se entrega como parametro una lista vacia
       (valid-structure? (get-train-cars train))))  ; Verifica la estructura del tren según los requisitos.

; valid-structure?: Función para verificar la estructura correcta de los carros dentro de un tren.
; Dominio: pcars
; Recorrido: boolean - Verdadero si todos los carros cumplen con la estructura necesaria.
(define (valid-structure? pcars)
  (and (>= (length pcars) 2)  ; Debe tener al menos dos carros.
       (equal? (get-pcar-type (first-of-list pcars)) tr)  ; El primer carro debe ser terminal.
       (equal? (get-pcar-type (last-of-list pcars)) tr)   ; El último carro debe ser terminal.
       (valid-central-car-types? (rest-of-list (reverse (rest-of-list (reverse pcars))))))) ; Verifica que carros intermedios sean centrales.

; valid-central-car-types?: Función para verificar que todos los carros intermedios sean del tipo central.
; Dominio: pcars
; Recorrido: boolean - Verdadero si todos son centrales.
(define (valid-central-car-types? pcars)
  (if (null? pcars)
      #t  ; Si no hay carros intermedios, es válido (caso Tc-Tc).
      (andmap (lambda (pcar) (equal? (get-pcar-type pcar) ct)) pcars)))  ; Todos los carros intermedios deben ser centrales.

; unique-ids?: Función para verificar que todos los IDs de carros sean únicos.
; Dominio: pcars x visto (list)
; Recorrido: boolean - Verdadero si todos los IDs son únicos.
(define (unique-ids? pcars visto)
  (cond ((null? pcars) #t)                           ; Si no hay mas carros, todos los IDs eran únicos.
        ((member (get-pcar-id (first pcars)) visto) #f)     ; Si el ID actual ya fue visto, retorna falso.
        (else (unique-ids? (rest pcars)               ; De lo contrario, sigue revisando.
                            (cons (get-pcar-id (first pcars)) visto)))))  ; Añade el ID actual a la lista de vistos.


;---------------------Selectores---------------------

; get-train-id: Función para obtener el identificador de un tren.
; Dominio: train
; Recorrido: id (integer)
(define (get-train-id train) 
  (first-of-list train))

; get-train-maker: Función para obtener el fabricante de un tren.
; Dominio: train
; Recorrido: maker (string
(define (get-train-maker train) 
  (second-of-list train)) 

; get-train-rail-type: Función para obtener el tipo de riel de un tren.
; Dominio: train
; Recorrido: rail-type (string)
(define (get-train-rail-type train) 
  (third-of-list train)) 

; get-train-speed: Función para obtener la velocidad de un tren.
; Dominio: train
; Recorrido: speed (number)
(define (get-train-speed train) 
  (fourth-of-list train))

; get-train-stay-time: Función para obtener el tiempo de estancia en estaciones de un tren.
; Dominio: train
; Recorrido: stay-time (number)
(define (get-train-stay-time train) 
  (fifth-of-list train))

; get-train-cars: Función para obtener la lista de carros de un tren.
; Dominio: train
; Recorrido: cars
(define (get-train-cars train) 
  (sixth-of-list train))

;---------------------Otras Funciones---------------------

; Verifica que todos los carros tengan el mismo modelo. Funciona correctamente con lista vacía.
(define (consistent-model? pcars)
  (if (null? pcars)
      #t
      (let ((model (get-pcar-model (first pcars))))
        (andmap (lambda (pcar) (equal? (get-pcar-model pcar) model)) pcars))))


; Verifica el orden correcto de los carros en el tren. Funciona correctamente con lista vacía.
(define (valid-train-car-order? pcars)
  (or (null? pcars)  ; Si no hay carros, devuelve verdadero.
      (and (equal? (get-pcar-type (first pcars)) tr)  ; El primer carro debe ser terminal.
           (equal? (get-pcar-type (last pcars)) tr)  ; El último carro debe ser terminal.
           (all-central-car-types? (rest (reverse (rest (reverse pcars))))))))  ; Carros intermedios deben ser centrales.

; Verifica que todos los carros intermedios sean de tipo central. Funciona correctamente con lista vacía.
(define (all-central-car-types? pcars)
  (if (null? pcars)
      #t
      (and (equal? (get-pcar-type (first pcars)) ct)
           (all-central-car-types? (rest pcars)))))

;----------------------Modificador--------------------


; train-add-car: Esta función inserta un carro en un tren en la posición especificada.
; Dominio: train (train) X pcar   (pcar) X position (positive-integer U {0})  
; Recorrido: train
; Uso de recursión de Cola
(define (train-add-car train pcar position)
  (if (< position 0)
      (raise "Posición inválida")  ; Lanza un error si la posición es menor que cero.
      (make-train (get-train-id train)                ; Recupera el ID del tren.
                  (get-train-maker train)             ; Recupera el fabricante del tren.
                  (get-train-rail-type train)         ; Recupera el tipo de riel del tren.
                  (get-train-speed train)             ; Recupera la velocidad del tren.
                  (get-train-stay-time train)         ; Recupera el tiempo de estancia del tren.
                  (insert-pcar (get-train-cars train) pcar position)))) ; Inserta el carro y devuelve un nuevo tren.

; Esta función crea una nueva estructura de tren.
; Dominio: id - identificador del tren, maker - fabricante, rail-type - tipo de riel, speed - velocidad,
;          stay-time - tiempo de estancia en estaciones, cars - lista de carros.
; Recorrido: train
(define (make-train id maker rail-type speed stay-time cars)
  (list id maker rail-type speed stay-time cars))  ; Empaqueta los argumentos en una lista.

; insert-pcar: Inserta un carro en una lista de carros en una posición específica usando recursividad.
; Dominio: cars X pcar X position
; Recorrido: pcars
(define (insert-pcar cars pcar position)
  (cond ((null? cars) 
         (if (= position 0) 
             (list pcar) 
             (raise "Posición fuera de rango")))  ; Si la lista es vacía y la posición es 0, devuelve una lista con el carro.
        ((= position 0) 
         (cons pcar cars))  ; Si la posición es 0, inserta el carro al principio.
        (else 
         (cons (first-of-list cars) (insert-pcar (rest-of-list cars) pcar (- position 1))))))  ; De lo contrario, recursivamente inserta el carro en la posición correcta.

;---------------------Modificador---------------------
; train-remove-car: Función que permite eliminar un carro desde el convoy.
; Dominio: train (train) X position (positive-integer U {0})
; Recorrido: train
; Uso de recursión de cola

(define (train-remove-car train position)
  (if (or (< position 0) (>= position (length (get-train-cars train))))
      (raise "Posicion fuera de rango")
      (make-train (get-train-id train)
                  (get-train-maker train)
                  (get-train-rail-type train)
                  (get-train-speed train)
                  (get-train-stay-time train)
                  (remove-car (get-train-cars train) position 0))))

(define remove-car
  (lambda (cars position acum)
    (if (null? cars)
        '()  ; Si no hay mas carros retorna la lista vacía
        (if (= acum position)
            (rest-of-list cars)  ; Si es la posicion del carro a eliminar, omite este carro
            (cons (first-of-list cars) (remove-car (rest-of-list cars) position (+ acum 1)))))))  ; De lo contrario sigue buscando

;------------------------------------------
; train-capacity: Función que permite determinar la capacidad máxima de pasajeros del tren.
; Dominio: train
; Recorrido: positive-number U {0}
; Uso de recursividad natural
(define (train-capacity train)
  (sum-car-capacities (get-train-cars train)))

(define (sum-car-capacities pcars)
  (if (null? pcars)
      0  ; Si no hay más carros, la suma es 0.
      (+ (get-pcar-capacity (first-of-list pcars))  ; Suma la capacidad del carro actual.
         (sum-car-capacities (rest-of-list pcars)))))  ; Recursividad con el resto de los carros.


