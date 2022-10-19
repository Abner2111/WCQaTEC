#lang racket/gui

(require racket/gui/base)

; Se define las variables del marcador
(define scoreTeam1 0)
(define scoreTeam2 0)

; Creates window frame
(define frame (new frame%
                   [label "World Cup QaTec"]
                   [width 640]
                   [height 500]))

; Creates a canvas into the mainframe
(define canvas
(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)

                ; coloca la imagen
                (send dc draw-bitmap(read-bitmap "Resources/field.jpg") 0 35 )
                (send dc draw-bitmap(read-bitmap "Resources/CRC1.png") (send CRC1 get-xpos) (send CRC1 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/CRC2.png") (send CRC2 get-xpos) (send CRC2 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/CRC3.png") (send CRC3 get-xpos) (send CRC3 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/CRC4.png") (send CRC4 get-xpos) (send CRC4 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/CRC5.png") (send CRC5 get-xpos) (send CRC5 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/CRC6.png") (send CRC6 get-xpos) (send CRC6 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/CRC7.png") (send CRC7 get-xpos) (send CRC7 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/CRC8.png") (send CRC8 get-xpos) (send CRC8 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/CRC9.png") (send CRC9 get-xpos) (send CRC9 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/CRC10.png") (send CRC10 get-xpos) (send CRC10 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/CRC11.png") (send CRC11 get-xpos) (send CRC11 get-ypos))

                (send dc draw-bitmap(read-bitmap "Resources/BRA1.png") (send BRA1 get-xpos) (send BRA1 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/BRA2.png") (send BRA2 get-xpos) (send BRA2 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/BRA3.png") (send BRA3 get-xpos) (send BRA3 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/BRA4.png") (send BRA4 get-xpos) (send BRA4 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/BRA5.png") (send BRA5 get-xpos) (send BRA5 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/BRA6.png") (send BRA6 get-xpos) (send BRA6 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/BRA7.png") (send BRA7 get-xpos) (send BRA7 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/BRA8.png") (send BRA8 get-xpos) (send BRA8 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/BRA9.png") (send BRA9 get-xpos) (send BRA9 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/BRA10.png") (send BRA10 get-xpos) (send BRA10 get-ypos))
                (send dc draw-bitmap(read-bitmap "Resources/BRA11.png") (send BRA11 get-xpos) (send BRA11 get-ypos))
                
                ; coloca el texto del marcador
                (send dc draw-text (~a "CRC  " scoreTeam1 " BRA  " scoreTeam2) 10 5)
                )
              ]
             )
)

; Make a button in the frame
(new button% [parent frame]
             [label "prueba"]
             [callback (lambda (button event)
                         (set! scoreTeam1 (+ scoreTeam1 1))
                         (newpos players (coord-to-mat (car (cdr coods2))))
                         )
                         
                ]
)

; Class for players

(define player%
  (class object%

    ; atributos iniciales
    (init x y)

    ; Se definen los atributos
    (define xPos (mat-to-cordx x)) ; coordenada x para las imagenes
    (define yPos (mat-to-cordy y)) ; coordenada y para las imagenes
    (define matx x) ; Numero de fila en la matriz 
    (define maty y) ; Numero de columna en la matriz 
    (define aim 0)
    (define speed 0)
    (define block 0)

    (super-new)

    ; Se definen los metodos
    
    ; ---> getters
    (define/public (get-xpos) xPos)
    (define/public (get-ypos) yPos)
    (define/public (get-xmat) matx)
    (define/public (get-ymat) maty)
    (define/public (get-aim) aim)
    (define/public (get-speed) speed)
    (define/public (get-block) block)

    ; ---> setters
    (define/public (set-xpos newX)
      (set! xPos newX))
    (define/public (set-ypos newY)
      (set! yPos newY))
    (define/public (set-xmat newX)
      (set! matx newX))
    (define/public (set-ymat newY)
      (set! maty newY))
    (define/public (set-aim newAim)
      (set! aim newAim))
    (define/public (set-speed newSpeed)
      (set! speed newSpeed))
    (define/public (set-block newBlock)
      (set! block newBlock))
  ))

;Class for ball
(define ball%
  (class object%

    ;atributos iniciales
    (init xi yi xf yf)

    ;Definicion de atributos
    (define xIni xi)
    (define yIni yi)
    (define xFin xf)
    (define yFin yf)

    (super-new) ;No sé si es necesario ponerlo

    ;Getters
    (define/public (get-xini) xIni)
    (define/public (get-yini) yIni)
    (define/public (get-xfin) xFin)
    (define/public (get-yfin) yFin)

    ;Setters
    (define/public (set-xini newXini)
      (set! xIni newXini))
    (define/public (set-yini newYini)
      (set! yIni newYini))
    (define/public (set-xfin newXfin)
      (set! xFin newXfin))
    (define/public (set-yfin newYfin)
      (set! yFin newYfin))
    (define/public (ballpass newXfin newYfin)
      (set! xIni newXfin)
      (set! yIni newYfin))
    ))

; Crea las posiciones iniciales de todos los jugadores 
(define (posiniciales listajugs listacoord canvas)
  (cond ((or (null? listacoord) (null? listajugs))
         '())
        (else
         (send (car listajugs) set-xpos (car (car listacoord)))
         (send (car listajugs) set-ypos (car (cdr (car listacoord))))
         (send (car listajugs) set-draw canvas)
         (posiniciales (cdr listajugs) (cdr listacoord) canvas))))

;;Metodo para realizar animaciones de todos los jugadores
(define (newpos listajugs listacoord)
  (cond ((or (null? listacoord) (null? listajugs))
         '())
        (else
          (moveObj (car listajugs) (caar listacoord) (cadr (car listacoord)))
          (newpos (cdr listajugs) (cdr listacoord)))))
        
; Cambia las coordenadas para la animacion HACER HILOS
#|
(define (animation player xNewPos yNewPos)
  (cond
    ((and (equal? (send player get-xpos)  xNewPos) (equal? (send player get-ypos) yNewPos)) 0)
    ((and (> (send player get-xpos)  xNewPos) (> (send player get-ypos) yNewPos))
      (send player set-xpos (- (send player get-xpos) 1))
      (send player set-ypos (- (send player get-ypos) 1))
      ;(sleep 0.01)
      (send canvas refresh-now)
      (animation player xNewPos yNewPos)  
    )


    ((and (< (send player get-xpos)  xNewPos ) (< (send player get-ypos) yNewPos))
      (send player set-xpos (+ (send player get-xpos) 1))
      (send player set-ypos (+ (send player get-ypos) 1))
      ;(sleep 0.01)
      (send canvas refresh-now)
      (animation player xNewPos yNewPos)
    )

    ((and (< (send player get-xpos)  xNewPos ) (> (send player get-ypos) yNewPos))
      (send player set-xpos (+ (send player get-xpos) 1))
      (send player set-ypos (- (send player get-ypos) 1))
      ;(sleep 0.01)
      (send canvas refresh-now)
      (animation player xNewPos yNewPos)
    )

    ((and (> (send player get-xpos)  xNewPos ) (< (send player get-ypos) yNewPos))
      (send player set-xpos (- (send player get-xpos) 1))
      (send player set-ypos (+ (send player get-ypos) 1))
      ;(sleep 0.01)
      (send canvas refresh-now)
      (animation player xNewPos yNewPos)
    ) 
    (else
      0
    )
  )
)
|#

(define (animation player xNewPos yNewPos)
  
  (sleep/yield 0.005)
  (send canvas refresh-now)
 
  (cond
    ((and (equal? (send player get-xpos)  xNewPos) (not (equal? (send player get-ypos) yNewPos)))
     (cond
       ((> (send player get-ypos) yNewPos)
        (send player set-ypos (- (send player get-ypos) 5))
        ;(send canvas refresh-now)
        ;(sleep 0.02)
        (animation player xNewPos yNewPos))
       
       ((< (send player get-ypos) yNewPos)
        (send player set-ypos (+ (send player get-ypos) 5))
        ;(send canvas refresh-now)
        ;(sleep 0.02)
        (animation player xNewPos yNewPos))
       )
     )
     ((and (not(equal? (send player get-xpos)  xNewPos)) (equal? (send player get-ypos) yNewPos))
     (cond
       ((> (send player get-xpos) xNewPos)
        (send player set-xpos (- (send player get-xpos) 5))
        ;(send canvas refresh-now)
        ;(sleep 0.02)
        (animation player xNewPos yNewPos))
       
       ((< (send player get-xpos) xNewPos)
        (send player set-xpos (+ (send player get-xpos) 5))
        ;(send canvas refresh-now)
        ;(sleep 0.02)
        (animation player xNewPos yNewPos))
       )
     )
    ((and (equal? (send player get-xpos)  xNewPos) (equal? (send player get-ypos) yNewPos)) 0)
    ((and (> (send player get-xpos)  xNewPos) (> (send player get-ypos) yNewPos))
      (send player set-xpos (- (send player get-xpos) 5))
      (send player set-ypos (- (send player get-ypos) 5))
      ;(send canvas refresh-now)
      ;(sleep 0.02)
      (animation player xNewPos yNewPos)  
    )


    ((and (< (send player get-xpos)  xNewPos ) (< (send player get-ypos) yNewPos))
      (send player set-xpos (+ (send player get-xpos) 5))
      (send player set-ypos (+ (send player get-ypos) 5))
      ;(send canvas refresh-now)
      ;(sleep 0.02)
      (animation player xNewPos yNewPos)
    )

    ((and (< (send player get-xpos)  xNewPos ) (> (send player get-ypos) yNewPos))
      (send player set-xpos (+ (send player get-xpos) 5))
      (send player set-ypos (- (send player get-ypos) 5))
      ;(send canvas refresh-now)
      ;(sleep 0.02)
      (animation player xNewPos yNewPos)
    )

    ((and (> (send player get-xpos)  xNewPos ) (< (send player get-ypos) yNewPos))
      (send player set-xpos (- (send player get-xpos) 5))
      (send player set-ypos (+ (send player get-ypos) 5))
      ;(send canvas refresh-now)
      ;(sleep 0.02)
      (animation player xNewPos yNewPos)
    ) 
    
    (else
      0
    )
  )
)

; convierte las coordenadas del algoritmo genetico a numeros de la matriz 
(define (coord-to-mat listCoords)
  (cond
    ((null? listCoords) '())
    (else (append (list (list (/ (+ (caar listCoords) 15) 37) (/ (- (cadar listCoords) 4) 36))) (coord-to-mat (cdr listCoords)))
    )
  )
)

; convierte las coordenadas en la fila de matriz
(define (cord-to-matx cord)
 (/ (+ cord 15) 37)
)

; convierte las coordenadas en la columna de matriz
(define (cord-to-maty cord)
 (/ (- cord 4) 36)
)

; convierte la fila de matriz en las coordenadas 
(define (mat-to-cordx mat)
  (+ (* (- mat 1) 37) 22)
)

; convierte la columna de matriz en las coordenadas 
(define (mat-to-cordy mat)
  (+ (* (- mat 1) 36) 40)
)

(define (moveObj obj xNewPos yNewPos) ; x numero mat y numero mat
  
  (sleep/yield 0.2)
  (send canvas refresh-now)
 
  (cond
    ((and (equal? (send obj get-xmat) xNewPos) (not (equal? (send obj get-ymat) yNewPos)))
     (cond
       ((> (send obj get-ymat) yNewPos)
        (send obj set-ymat (- (send obj get-ymat) 1))
        (send obj set-ypos (mat-to-cordy (send obj get-ymat)))
        (moveObj obj xNewPos yNewPos))
       
       ((< (send obj get-ypos) (mat-to-cordy yNewPos))
        (send obj set-ymat (+ (send obj get-ymat) 1))
        (send obj set-ypos (mat-to-cordy (send obj get-ymat)))
        (moveObj obj xNewPos yNewPos))
       )
     )
     ((and (not(equal? (send obj get-xmat)  xNewPos)) (equal? (send obj get-ymat)  yNewPos))
     (cond
       ((> (send obj get-xmat)  xNewPos)
        (send obj set-xmat (- (send obj get-xmat) 1))
        (send obj set-xpos (mat-to-cordx (send obj get-xmat)))
        (moveObj obj xNewPos yNewPos))
       
       ((< (send obj get-xmat)  xNewPos)
        (send obj set-xmat (+ (send obj get-xmat) 1))
        (send obj set-xpos (mat-to-cordx (send obj get-xmat)))
        (moveObj obj xNewPos yNewPos))
       )
     )

     ; condicion de Stop
    ((and (equal? (send obj get-xmat)  xNewPos) (equal? (send obj get-ymat)  yNewPos)) 0)

    ; XY nuevas menor a las viejas
    ((and (> (send obj get-xmat) xNewPos) (> (send obj get-ymat) yNewPos))

      ; correcion en x
      (send obj set-xmat (- (send obj get-xmat) 1))
      (send obj set-xpos (mat-to-cordx (send obj get-xmat)))

      ; correcion en y
      (send obj set-ymat (- (send obj get-ymat) 1))
      (send obj set-ypos (mat-to-cordy (send obj get-ymat)))

      (moveObj obj xNewPos yNewPos)  
    )
    ; XY nuevas mayor a las viejas
    ((and (< (send obj get-xmat)  xNewPos) (< (send obj get-ymat) yNewPos))

      ;correcion en x
      (send obj set-xmat (+ (send obj get-xmat) 1))
      (send obj set-xpos (mat-to-cordx (send obj get-xmat)))

      ;correcion en y
      (send obj set-ymat (+ (send obj get-ymat) 1))
      (send obj set-ypos (mat-to-cordy (send obj get-ymat)))

      (moveObj obj xNewPos yNewPos)
    )

    ((and (< (send obj get-xmat) xNewPos) (> (send obj get-ymat) yNewPos))

      ;correcion en x
      (send obj set-xmat (+ (send obj get-xmat) 1))
      (send obj set-xpos (mat-to-cordx (send obj get-xmat)))

      ; correcion en y
      (send obj set-ymat (- (send obj get-ymat) 1))
      (send obj set-ypos (mat-to-cordy (send obj get-ymat)))

      (moveObj obj xNewPos yNewPos)
    )

    ((and (> (send obj get-xmat)  xNewPos ) (< (send obj get-ymat) yNewPos))

      ; correcion en x
      (send obj set-xmat (- (send obj get-xmat) 1))
      (send obj set-xpos (mat-to-cordx (send obj get-xmat)))

      ;correcion en y
      (send obj set-ymat (+ (send obj get-ymat) 1))
      (send obj set-ypos (mat-to-cordy (send obj get-ymat)))

      (moveObj obj xNewPos yNewPos)
    ) 

    (else
      0
    )
  )
)

; jugadores ----------------------------------------------------------
(define CRC1 (new player% (x 2) (y 1)))
(define CRC2 (new player% (x 2) (y 2)))
(define CRC3 (new player% (x 2) (y 3)))
(define CRC4 (new player% (x 2) (y 4)))
(define CRC5 (new player% (x 2) (y 5)))
(define CRC6 (new player% (x 2) (y 6)))
(define CRC7 (new player% (x 2) (y 7)))
(define CRC8 (new player% (x 2) (y 8)))
(define CRC9 (new player% (x 2) (y 9)))
(define CRC10 (new player% (x 2) (y 10)))
(define CRC11 (new player% (x 2) (y 11)))
(define BRA1 (new player% (x 7) (y 1)))
(define BRA2 (new player% (x 7) (y 2)))
(define BRA3 (new player% (x 7) (y 3)))
(define BRA4 (new player% (x 7) (y 4)))
(define BRA5 (new player% (x 7) (y 5)))
(define BRA6 (new player% (x 7) (y 6)))
(define BRA7 (new player% (x 7) (y 7)))
(define BRA8 (new player% (x 7) (y 8)))
(define BRA9 (new player% (x 7) (y 9)))
(define BRA10 (new player% (x 7) (y 10)))
(define BRA11 (new player% (x 7) (y 11)))



; pruebas ----------------------------------------------------------

(define players (list CRC1 CRC2 CRC3 CRC4 CRC5 CRC6 CRC7 CRC8 CRC9 CRC10 CRC11 BRA1 BRA2 BRA3 BRA4 BRA5 BRA6 BRA7 BRA8 BRA9 BRA10 BRA11))
(define coords (list (list 50 50) (list 50 50) 
                     (list 50 50) (list 50 50)
                     (list 50 50) (list 50 50)
                     (list 50 50) (list 50 50)
                     (list 50 50) (list 50 50)
                     (list 50 50) (list 50 50)
                     (list 50 50) (list 50 50)
                     (list 50 50) (list 50 50)
                     (list 50 50) (list 50 50)
                     (list 50 50) (list 50 50)
                     (list 50 50) (list 50 50))
  
  )


(define coods2 (list (list 3 5 2)  
(list (list 133 112 1 1 1) (list 133 112 1 1 1) 
(list 133 112 1 1 1) (list 133 112 1 1 1)  
(list 133 112 1 1 1) (list 133 112 1 1 1) 
(list 133 112 1 1 1) (list 133 112 1 1 1) 
(list 133 112 1 1 1) (list 133 112 1 1 1) 
(list 133 112 1 1 1) (list 133 112 1 1 1) 
(list 133 112 1 1 1) (list 133 112 1 1 1) 
(list 133 112 1 1 1) (list 133 112 1 1 1) 
(list 133 112 1 1 1) (list 133 112 1 1 1) 
(list 133 112 1 1 1) (list 133 112 1 1 1) 
(list 133 112 1 1 1) (list 133 112 1 1 1))))


(send frame show #t)



