#lang racket/gui

(provide genetico)
;;POBLACION INICIAL
;;----------------------------------------------------------------------------------------------------
;;se le da una alineacion '(num_defensas, num_medios,num_delanteros)
;;retorna una lista con listas con 3 veces mas la cantidad de los jugadores indicados en la alineacion
;;jugador = (posx, posy, interceptar [0-10], velocidad [0-10], punteria [0-10] )
;; casa es 1 si es local, visita es casa=0
(define (poblacion_inicial alineacion casa)
    (
      cond
      (
        (equal? casa 1)
        (list 
          alineacion 
          (jugadores_iniciales (* 3 (car alineacion)) 1 '()) 
          (jugadores_iniciales (* 3 (cadr alineacion)) 2 '()) 
          (jugadores_iniciales (* 3 (caddr alineacion)) 3 '())
        )
      )
      (
        else
        (list 
          alineacion 
          (jugadores_iniciales (* 3 (car alineacion)) 3 '()) 
          (jugadores_iniciales (* 3 (cadr alineacion)) 2 '()) 
          (jugadores_iniciales (* 3 (caddr alineacion)) 1 '())
        )
      )
    )
    
)

(define (jugadores_iniciales cantidad pos jugadores)
    (
        cond 
        (
            (zero? cantidad)
            jugadores
        )
        (
            (jugadores_iniciales 
                (- cantidad 1) 
                pos 
                (cons
                    (list 
                        (car (random-coords pos)) 
                        (cadr (random-coords pos)) 
                        (quotient (random 101) 10)
                        (quotient (random 101) 10) 
                        (quotient (random 101) 10)
                    )
                    jugadores 

                ) 
            )
        )
    )
)
;;supone cancha de 80px de largo, iniciando en pixel 22 y  55px de ancho
;;limites horizontales: [22-170], [171-468], [469-618] 
;;397y x596
;;limite vertical [14-411]
(define (random-coords position)
    (
        cond

        (
            ;;defensas
            (equal? position 1)
            (list (+ 22 (* 37 (random (round (/ (/ 596 4) 37))))) (+ 40 (* 36 (random (quotient 397 36)))))
        )
        
        (
            ;;mediocampistas
            (equal? position 2)
            (list (+ (+ 22 (/ 596 4)) (* 37 (random (round (/ (/ 596 2) 37))))) (+ 40 (* 36 (random (quotient 397 36)))))
        )
        (
            ;;delanteros
            (equal? position 3)
            (list (+ (+ 22 (/ 596 4) (/ 596 2)) (* 37 (random (round (/ (/ 596 4) 37))))) (+ 40 (* 36 (random (quotient 397 36)))))
        )
    )
   
)
;;(poblacion_inicial '(4 4 2))
;;----------------------------------------------------------------------------------------------------


;;FITNESS 
;;Recibe lista de jugadores (defensas, medios, delanteros) y la alineacion
;;----------------------------------------------------------------------------------------------------
;; casa es 1 o 0. 1 si es home, 0 si es visita
(define (fitness poblacion-alineacion casa)
  (fitness-aux (cdr poblacion-alineacion) (car poblacion-alineacion) casa)
)

(define (fitness-aux poblacion alineacion casa)
    (
      cond
      (
        (equal? casa 1)
        (list 
          alineacion
          (select-cuantity  (car alineacion) (quicksort  (car poblacion) 1) '())
          (select-cuantity  (cadr alineacion) (quicksort  (cadr poblacion) 2) '())
          (select-cuantity  (caddr alineacion) (quicksort  (caddr poblacion) 3) '())
        )
      )
      (
        else
        (list 
          alineacion
          (select-cuantity  (car alineacion) (quicksort  (car poblacion) 3) '())
          (select-cuantity  (cadr alineacion) (quicksort  (cadr poblacion) 2) '())
          (select-cuantity  (caddr alineacion) (quicksort  (caddr poblacion) 1) '())
        )
      )
    )
    
)


;;selecciona la cantidad de jugadores necesarios de acuerdo a la alineacion y una poblacion de jugadores ordenada en orden descendiente
(define (select-cuantity cantidad poblacion jugadores-seleccionados)
    (
        cond
        (
            (zero? cantidad)
            jugadores-seleccionados
        )
        
        (
            else
            (select-cuantity (- cantidad 1 ) (cdr poblacion) (cons (car poblacion) jugadores-seleccionados))
        )
    )
)

(define (quicksort lista posicion)
  (cond ( (null? lista)
          '())
        (else
           (append 
           (quicksort (cadr (smaller_grater (car lista) (cdr lista) '() '() posicion)) posicion)
           (list (car lista)) 
           (quicksort (car (smaller_grater (car lista) (cdr lista) '() '() posicion)) posicion) 
           
           )
        )
  )
)
;;ELEMENTO DE COMPARACION EN EL QUICKSORT,
;; delanteros: ((interceptar [0-10] + velocidad [0-10] +  punteria [0-10])/ 8) + punteria
;; medios: ((interceptar [0-10] + velocidad [0-10] +  punteria [0-10]) / 8) + velocidad
;; defensas: ((interceptar [0-10] + velocidad [0-10] + punteria [0-10]) / ) + interceptar
(define (smaller_grater pivot lista menores mayores posicion)
  (cond ( (null? lista)
          (list menores mayores))
            
        ( (<= 
            ( + 
                ( / 
                    (+ 
                        (n-esimo (+ 2 1) (car lista) )
                        (n-esimo (+ 2 2) (car lista) ) 
                        (n-esimo (+ 2 3) (car lista) )
                    ) 
                    3
                ) 
                (n-esimo  (+ 2 posicion) (car lista))
            )
            (n-esimo  (+ 2 posicion) pivot) 
          )
          
          (smaller_grater pivot (cdr lista) (cons (car lista) menores) mayores posicion))
        ( else
          (smaller_grater pivot (cdr lista) menores (cons (car lista) mayores) posicion)
        )
    )
)
(define (largo lista)
  (
    cond 
    (
      (null? lista)
      0
    )
    (
      else
      (largo-aux 0 lista)
    )
  )
)

(define (largo-aux n-elementos lista)
  (
    cond 
    (
      (null? lista)
      n-elementos
    )
    (
      else
      (largo-aux (+ 1 n-elementos) (cdr lista))
    )
  )
)

(define (n-esimo n lista)
  (
    cond 
    (
      (> n (largo lista))
      #f
    )
    (
      else
      (n-esimo-aux 1 n lista)
    )
  )
)

(define (n-esimo-aux indice n lista)
  (
    cond
    (
      (equal? indice n)
      (car lista)
    )
    (
      else
      (n-esimo-aux (+ 1 indice) n (cdr lista))
    )
  )
)

;;'((4 4 2) ((22 25 4 7 9) (27 30 0 9 9) (32 35 3 8 2) (27 10 5 3 1) (22 10 2 9 7) (22 15 4 6 4) (22 35 7 0 4) (32 50 5 2 7) (27 5 1 8 3) (32 35 2 1 0) (22 30 10 4 4) (27 10 3 0 8)) ((42 15 3 4 1) (72 40 9 9 2) (42 0 0 7 0) (77 35 6 8 6) (62 50 8 1 1) (52 45 7 1 9) (57 40 5 7 9) (42 25 5 8 5) (42 0 4 6 1) (52 15 0 2 4) (57 10 6 6 2) (72 20 1 2 1)) ((92 45 4 5 6) (82 30 6 10 0) (82 30 9 2 9) (87 50 4 5 2) (97 25 2 0 0) (82 5 3 9 0)))

#|
(fitness (cdr '((4 4 2) ((22 25 4 7 9) (27 30 0 9 9) (32 35 3 8 2) (27 10 5 3 1) (22 10 2 9 7) (22 15 4 6 4) (22 35 7 0 4) (32 50 5 2 7) (27 5 1 8 3) (32 35 2 1 0) (22 30 10 4 4) (27 10 3 0 8)) ((42 15 3 4 1) (72 40 9 9 2) (42 0 0 7 0) (77 35 6 8 6) (62 50 8 1 1) (52 45 7 1 9) (57 40 5 7 9) (42 25 5 8 5) (42 0 4 6 1) (52 15 0 2 4) (57 10 6 6 2) (72 20 1 2 1)) ((92 45 4 5 6) (82 30 6 10 0) (82 30 9 2 9) (87 50 4 5 2) (97 25 2 0 0) (82 5 3 9 0))))
(car '((4 4 2) ((22 25 4 7 9) (27 30 0 9 9) (32 35 3 8 2) (27 10 5 3 1) (22 10 2 9 7) (22 15 4 6 4) (22 35 7 0 4) (32 50 5 2 7) (27 5 1 8 3) (32 35 2 1 0) (22 30 10 4 4) (27 10 3 0 8)) ((42 15 3 4 1) (72 40 9 9 2) (42 0 0 7 0) (77 35 6 8 6) (62 50 8 1 1) (52 45 7 1 9) (57 40 5 7 9) (42 25 5 8 5) (42 0 4 6 1) (52 15 0 2 4) (57 10 6 6 2) (72 20 1 2 1)) ((92 45 4 5 6) (82 30 6 10 0) (82 30 9 2 9) (87 50 4 5 2) (97 25 2 0 0) (82 5 3 9 0)))))
|#

;;----------------------------------------------------------------------------------------------------

;;CROSSOVER
;;Recbe una poblacion de individuos seleccionados y devuelve una poblacion 
;;----------------------------------------------------------------------------------------------------
;;genera una cantidad de individuos cruzados igual a la cantidad de poblacion de entrada
;;casa es 1 si es local, 0 si es visita
(define (crossover population casa)
  (
    cond
    (
      (equal? casa 1)
      (list 
        ;;se hace crossover individualmente a defensas, medios y delanteros
        (car population)
        (crossover-perposition (cadr population) 1) 
        (crossover-perposition (caddr population) 2) 
        (crossover-perposition (cadddr population) 3)
      )
    )
    (
      else
      (list 
        ;;se hace crossover individualmente a defensas, medios y delanteros
        (car population)
        (crossover-perposition (cadr population) 3) 
        (crossover-perposition (caddr population) 2) 
        (crossover-perposition (cadddr population) 1)
      )
    )
  )
  
)

;;hace crossover de los jugadores con el mismo y el siguiente a partir de un punto de corte
(define (cross-parent-genes jugador1 jugador2 posicion)
  (cross-genes-aux1 jugador1 jugador2 (+ 2 posicion))
)

(define (cross-genes-aux1 jugador1 jugador2 punto-corte) 
  (
    cond
    (
      (null? jugador2)
      '()
    )
    (
      (<= punto-corte 1) ;;en el punto de corte, se pasa añadir los datos del jugador2 
      (cons (car jugador2) (cross-genes-aux1 jugador1 (cdr jugador2) punto-corte) )
    )
    (
      else
      ;;antes del punto de corte se añaden los datos del jugador1
      (cons (car jugador1) (cross-genes-aux1 (cdr jugador1) (cdr jugador2) (- punto-corte 1)))
    )
  )
)

;;(cross-parent-genes '(45 34 4 7 3) '(78 90 10 1 6) 1)

;;hace un crosover de los jugadores, para un solo tipo de jugador
(define (crossover-perposition jugadores posicion)
  (crossover-per-position-aux jugadores (car jugadores) posicion)
)

(define (crossover-per-position-aux jugadores primer-jugador posicion)
  (
    cond
    (
      (null? jugadores);;se recorrio la lista de jugadores
      '()
    )
    (
      (null? (cdr jugadores))
      ;;se hace crossover entre el primer y ultimo elemento de la lista
      (cons (cross-parent-genes (car jugadores) primer-jugador posicion) (crossover-per-position-aux (cdr jugadores) primer-jugador posicion)) ;;el jugador actual es el ultimo en la lista
    )
    (
      else 
      ;; elementos que no son el ultimo elemento de la lista
      (cons (cross-parent-genes (car jugadores) (cadr jugadores) posicion) (crossover-per-position-aux (cdr jugadores) primer-jugador posicion))
    )
  )
)
;;(crossover-perposition '((22 10 2 9 7) (22 15 4 6 4) (22 30 10 4 4) (22 35 7 0 4)) 1)


;;(crossover '(((22 10 2 9 7) (22 15 4 6 4) (22 30 10 4 4) (22 35 7 0 4)) ((42 0 0 7 0) (42 25 5 8 5) (77 35 6 8 6) (57 40 5 7 9)) ((52 45 7 1 9) (57 40 5 7 9))))
;;----------------------------------------------------------------------------------------------------


;;MUTATION
;;----------------------------------------------------------------------------------------------------


;;;posicion, visita o local? ,indice inicial en 1,indice a modificar (1 a 5)
(define (mutate-player player posicion local-visita index index-modify)
  (cond
    (
      (null? player)
      '()
    )
    (
      ;;llega al valor a modificar
      (equal? index index-modify)
      (
        cond
        ;; es una coordenada
        (
          ;;es la coordenada x
          (equal? index-modify 1)
          (
            cond
            (
              ;;es  delantero y visita o defensa y local 
              (or (and (equal? posicion 3) (equal? local-visita 0)) (and (equal? posicion 1) (equal? local-visita 1)))
              (
                cond
                ( 
                  ;;si esta dentro del rango horizontal lo aumenta en 37 (una unidad de la)
                  (<= (+ 37 (car player)) 170 )
                  (cons (+ 37 (car player)) (mutate-player (cdr player) posicion local-visita (+ 1 index) index-modify))
                )
                (
                  else
                  ;;si no lo devuelve al inicio del rango
                  (cons 22 (mutate-player (cdr player) posicion local-visita (+ 1 index) index-modify))
                )
              )
            )
            (
              ;;es delantero y local o defensa y visita 
              (or (and (equal? posicion 3) (equal? local-visita 1)) (and (equal? posicion 1) (equal? local-visita 0)))
              (
                cond
                ( 
                  ;;si esta dentro del rango horizontal lo aumenta en 37 (una unidad de la)
                  (<= (+ 37 (car player)) 618 )
                  (cons (+ 37 (car player)) (mutate-player (cdr player) posicion local-visita (+ 1 index) index-modify))
                )
                (
                  else
                  ;;si no lo devuelve al inicio del rango
                  (cons 469 (mutate-player (cdr player) posicion local-visita (+ 1 index) index-modify))
                )
              )
            )
            (
              ;;es medio campista 
              (equal? posicion 2)
              (
                cond
                ( 
                  ;;si esta dentro del rango horizontal lo aumenta en 37 (una unidad de la)
                  (<= (+ 37 (car player)) 468 )
                  (cons (+ 37 (car player)) (mutate-player (cdr player) posicion local-visita (+ 1 index) index-modify))
                )
                (
                  else
                  ;;si no lo devuelve al inicio del rango
                  (cons 171 (mutate-player (cdr player) posicion local-visita (+ 1 index) index-modify))
                )
              )
            )

          )
        )

        (
          ;;es la coordenada y
          (equal? index-modify 2)
          (
            cond
            (
              (<= (+ 36 (car player)) 411)
              (cons (+ 36 (car player)) (mutate-player (cdr player) posicion local-visita (+ 1 index) index-modify))
            )
          )
        )
        (
          ;;es alguna habilidad
          else
          (
            cond
            (
              (<= (+ 1 (car player)) 10)
              (cons (+ 1 (car player)) (mutate-player (cdr player) posicion local-visita (+ 1 index) index-modify))
            )
            (
              else
              (cons 0 (mutate-player (cdr player) posicion local-visita (+ 1 index) index-modify))
            )

          )
        )
      )
    )

    (
      else
      (cons (car player) (mutate-player (cdr player) posicion local-visita (+ 1 index) index-modify))
    )
  )

)
;;limites horizontales: [22-170], [171-468], [469-618] 
;;397y x596
;;limite vertical [14-411]

;;encuentra la cantidad de digitos que tiene un numero
(define (digit-num num)
  (
    cond
    (
      (zero? num)
      0
    )
    (
      else
      (+ 1 (digit-num (quotient num 10)))
    )
  )
)

;;(digit-num 900000)
;;----------------------------------------------------------------------------------------------------

;;(fitness (poblacion_inicial '(3 5 2)))


;;GENETICO

;;genera una nueva poblacion usando la actual generacion, unida al crossover y la mutacion de la misma
(define (new_population generacion casa)
  (append 
    (list (car generacion))
    (list (append (cadr generacion) (cadr (crossover generacion casa))))
    (list (append (caddr generacion) (caddr (crossover generacion casa))))
    (list (append (cadddr generacion) (cadddr (crossover generacion casa))))
  )
)
(define (genetico locales visita n-generaciones)
  (genetico-aux locales visita n-generaciones 0 '() '())
)

;;argumentos:
;;locales->alineacion de locales
;;visita-> alineacion de visita
;;
(define (genetico-aux locales visita n-generaciones curr-gen local-gen visita-gen)
  (
    cond
    (
      (equal? n-generaciones curr-gen)
      (list local-gen visita-gen)
    )
    (
      (zero? curr-gen) ;;poblacion 0
      (genetico-aux 
        locales visita 
        n-generaciones 
        (+ 1 curr-gen) 
        (append local-gen (list (fitness (poblacion_inicial locales 1) 1))) 
        (append local-gen (list (fitness (poblacion_inicial visita 0) 0)))
      )
    )
    (
      else
      (genetico-aux
       locales visita 
       n-generaciones 
       (+ 1 curr-gen) 
       (append local-gen (list (fitness (new_population (n-esimo (largo local-gen) local-gen ) 1) 1))) ;;agrega nuevas generaciones
       (append visita-gen (list (fitness (new_population (n-esimo (largo visita-gen) visita-gen ) 0) 0))))
    )
  )

  
  

)
;;defensa, visita ,indice inicial en 1,modifica x(indice 1)
;;(mutate-player '(617 364 10 2 7) 1 0 1 3)
;;(genetico '(4 4 2) '(3 3 4) 3)

;;(new_population '((3 5 2) ((133 112 7 0 5) (133 400 8 4 6) (59 256 6 4 0)) ((208 400 6 6 9) (319 40 6 7 9) (282 256 2 6 5) (208 400 9 6 5) (430 148 4 9 5)) ((208 400 9 6 5) (430 148 4 9 5))) 1)