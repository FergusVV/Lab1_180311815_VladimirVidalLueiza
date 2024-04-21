#lang racket

(require "pcar_180311815_vladimirvidallueiza.rkt")
(require "train_180311815_vladimirvidallueiza.rkt")
(require "driver_180311815_vladimirvidallueiza.rkt")
(require "line_180311815_vladimirvidallueiza.rkt")
(require "station_180311815_vladimirvidallueiza.rkt")
(require "section_180311815_vladimirvidallueiza.rkt")

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

(define (subway-elements subway)
  (third subway))

(define (subway-add-train sub . trains)
  ;; Función recursiva para añadir trenes uno por uno
  (define (add-trains current-sub remaining-trains)
    (if (null? remaining-trains)  ; Si no hay más trenes para añadir, devolver el subway actual
        current-sub
        ;; Llamar recursivamente con el nuevo subway y el resto de trenes
        (add-trains (subway (subway-id current-sub)
                            (subway-name current-sub)
                            (append (subway-elements current-sub) (list (first remaining-trains))))
                    (rest remaining-trains))))
  ;; Iniciar la recursividad con el subway original y la lista completa de trenes
  (add-trains sub trains))


;;función agregar linea sin usar recursión
(define (subway-add-line sub . lines)
  ;; Actualiza la lista de elementos del subway, agregando nuevas líneas
  (let ((current-elements (subway-elements sub)))
    ;; Crea un nuevo subway con los elementos actualizados que incluyen las nuevas líneas
    (subway (subway-id sub)
            (subway-name sub)
            (append current-elements lines))))

;;función agregar driver
(define (subway-add-driver sub . drivers)
  ;; Asume que subway-elements extrae todos los elementos actuales del subway, incluidos trenes, líneas y conductores
  (let ((current-elements (subway-elements sub)))
    ;; Crea un nuevo subway con la lista actualizada de elementos que ahora incluye los nuevos conductores
    (subway (subway-id sub)
            (subway-name sub)
            (append current-elements drivers))))

;;funcion subway a string

;; Define una función para convertir la estructura de un objeto subway en una representación en forma de cadena de texto.
(define (subway->string sub)
  ;; Concatena el ID y el nombre del subway, junto con una introducción a los elementos que contiene.
  (string-append "Subway ID: " (number->string (subway-id sub)) "\n"
                 "Subway Name: " (subway-name sub) "\n"
                 "Elements:\n"
                 ;; Verifica si la lista de elementos del subway está vacía.
                 (if (null? (subway-elements sub))
                     "No elements."  ; Si está vacía, indica que no hay elementos.
                     ;; Si no está vacía, convierte cada elemento a string y los une con saltos de línea.
                     (string-join (map element-to-string (subway-elements sub)) "\n"))))

;; 22 Función para convertir un elemento individual del subway (tren, línea o conductor) en una cadena de texto.
(define (element-to-string element)
  ;; Usa condicional para determinar el tipo de elemento y formatear la información apropiadamente.
  (cond ((train? element)  ; Si el elemento es un tren.
         ;; Formatea la información del tren en una cadena de texto.
         (string-append "Train ID: " (number->string (first element))
                        ", Maker: " (second element)
                        ", Rail-Type: " (third element)
                        ", Speed: " (number->string (fourth element))
                        ", Station Stay-Time: " (number->string (fifth element))))
        ((line? element)  ; Si el elemento es una línea.
         ;; Formatea la información de la línea, incluyendo una lista de secciones si están presentes.
         (string-append "Line ID: " (number->string (first element))
                        ", Name: " (second element)
                        ", Rail-Type: " (third element)
                        ", Sections: " (if (null? (fourth element)) "No sections."
                                         (string-join (map section-to-string (fourth element)) ", "))))
        ((driver? element)  ; Si el elemento es un conductor.
         ;; Formatea la información del conductor en una cadena de texto.
         (string-append "Driver ID: " (number->string (first element))
                        ", Name: " (second element)
                        ", Train-Maker: " (third element)))
        (else "No element")))  ; Manejo de casos donde el elemento no es reconocido.

;; Define una función para convertir una sección de metro en cadena de texto.
(define (section-to-string section)
  ;; Formatea la información de la sección, incluyendo la estación de inicio, estación final, distancia y costo.
  (string-append "Section "
                 (get-station-name (first section))  ; Obtiene el nombre de la estación de inicio.
                 " - "
                 (get-station-name (second section))  ; Obtiene el nombre de la estación de final.
                 ", Distance: " (number->string (third section)) " km"
                 ", Cost: " (number->string (fourth section))))



;; 23 Función para ajustar el costo de las secciones en un subway.
(define (subway-rise-section-cost subway func)
  (list (subway-id subway)  ; ID del subway
        (subway-name subway)  ; Nombre del subway
        (map (lambda (element)  ; Procesa cada línea para ajustar costos
               (if (line? element)  ; Verifica que sea una línea 
                   (list (get-line-id element)  ; ID de la línea
                         (get-line-name element)  ; Nombre de la línea
                         (get-line-rail-type element)  ; Tipo de riel
                         (map (lambda (section)  ; Procesa cada sección
                                (list (get-section-point1 section)  ; Estación de inicio
                                      (get-section-point2 section)  ; Estación final
                                      (get-section-distance section)  ; Distancia
                                      (func (get-section-cost section))))  ; Aplica func al costo
                              (get-line-sections element)))  ; Secciones de la línea
                   element))  ; Devuelve el elemento sin cambios si no es una línea
             (subway-elements subway))))  ; Elementos del subway

;; 24 Función para modificar el tiempo de parada de una estación específica en un subway.
;; Función para modificar el tiempo de parada de una estación específica en un subway.
(define (subway-set-station-stoptime subway stationName time)
  (list (subway-id subway)  ; ID del subway
        (subway-name subway)  ; Nombre del subway
        (map (lambda (element)  ; Procesa cada elemento del subway para actualizar las líneas.
               (if (line? element)  ; Verifica si es una línea.
                   (list (get-line-id element)  ; ID de la línea
                         (get-line-name element)  ; Nombre de la línea
                         (get-line-rail-type element)  ; Tipo de riel
                         (map (lambda (section)  ; Procesa cada sección de la línea.
                                (list (update-station (get-section-point1 section) stationName time)  ; Estación de inicio
                                      (update-station (get-section-point2 section) stationName time)  ; Estación final
                                      (get-section-distance section)  ; Distancia
                                      (get-section-cost section)))  ; Costo
                              (get-line-sections element)))  ; Secciones de la línea
                   element))  ; Devuelve el elemento sin cambios si no es una línea
             (subway-elements subway))))  ; Elementos del subway

;; Función para actualizar el tiempo de parada de una estación si su nombre coincide
(define (update-station station stationName time)
  (if (string=? (get-station-name station) stationName)  ; Compara el nombre de la estación.
      (list (get-station-id station)  ; ID de la estación
            (get-station-name station)  ; Nombre de la estación
            (get-station-type station)  ; Tipo de estación
            time)  ; Nuevo tiempo de parada
      station))  ; Devuelve la estación sin cambios si el nombre no coincide





