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
                (send dc draw-bitmap(read-bitmap "Resources/Ball.png") (send Ball get-xpos) (send Ball get-ypos))
                
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
                         ;(set! scoreTeam1 (+ scoreTeam1 1))
                         ;(newpos players coords)
                         (animball Ball 200 392)
                         (send canvas refresh-now)
                         (collisionBL Ball)
                         (collisionBM Ball)
                         )
                       ]
             )

; Class for players

(define player%
  (class object%

    ; atributos iniciales
    (init x y)

    ; Se definen los atributos
    (define xPos x)
    (define yPos y)
    (define aim 0)
    (define speed 0)
    (define block 0)
    (define rectangulo 0) 
    (define tag "player")

    (super-new)

    ; Se definen los metodos
    
    ; ---> getters
    (define/public (get-xpos) xPos)
    (define/public (get-ypos) yPos)
    (define/public (get-aim) aim)
    (define/public (get-speed) speed)
    (define/public (get-block) block)
    (define/public (get-rec) rectangulo)
    (define/public (get-tag) tag)

    ; ---> setters
    (define/public (set-xpos newX)
      (set! xPos newX))
    (define/public (set-ypos newY)
      (set! yPos newY))
    (define/public (set-aim newAim)
      (set! aim newAim))
    (define/public (set-speed newSpeed)
      (set! speed newSpeed))
    (define/public (set-block newBlock)
      (set! block newBlock))
    (define/public (move x y dc)
      (send dc translate x y))  
  ))

;Class for ball
(define ball%
  (class object%

    ;atributos iniciales
    (init x y)

    ;Definicion de atributos
    ;(define xPos x)
    ;(define yPos y)
    (define xIni x)
    (define yIni y)
    (define xFin x)
    (define yFin y)
    (define tag "ball")

    (super-new) ;No sé si es necesario ponerlo

    ;Getters
    ;(define/public (get-xpos) xPos)
    ;(define/public (get-ypos) yPos)
    (define/public (get-xpos) xIni)
    (define/public (get-ypos) yIni)
    (define/public (get-xfin) xFin)
    (define/public (get-yfin) yFin)
    (define/public (get-tag) tag)

    ;Setters
    (define/public (set-xpos newXini)
      (set! xIni newXini))
    (define/public (set-ypos newYini)
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
         (ani (car listajugs) (caar listacoord) (cadr (car listacoord)))
         (newpos (cdr listajugs) (cdr listacoord)))))
        
        
  
; Cambia las coordenadas para la animacion HACER HILOS

(define (ani player xNewPos yNewPos)
  (sleep/yield 0.005)
  (send canvas refresh-now)
;(cond (equal? (send player get-tag) "player"))
  ;(collisionBL Ball xNewPos yNewPos)
  (collisionBM Ball)
  (cond
    ((and (equal? (send player get-xpos)  xNewPos) (not (equal? (send player get-ypos) yNewPos))) ;si ya llego a la pos x y si no ha llegado a la pos y
     (cond
       ((> (send player get-ypos) yNewPos) ;si ya llego a x, solo se mueve en y
        (send player set-ypos (- (send player get-ypos) 1))
        (ani player xNewPos yNewPos))
       
       ((< (send player get-ypos) yNewPos)
        (send player set-ypos (+ (send player get-ypos) 1))
        (ani player xNewPos yNewPos))
       )
     )
     ((and (not(equal? (send player get-xpos)  xNewPos)) (equal? (send player get-ypos) yNewPos))
     (cond
       ((> (send player get-xpos) xNewPos)
        (send player set-xpos (- (send player get-xpos) 1))
        (ani player xNewPos yNewPos))
       
       ((< (send player get-xpos) xNewPos)
        (send player set-xpos (+ (send player get-xpos) 1))
        (ani player xNewPos yNewPos))
       )
     )
    ((and (equal? (send player get-xpos)  xNewPos) (equal? (send player get-ypos) yNewPos)) 0)
    ((and (> (send player get-xpos)  xNewPos) (> (send player get-ypos) yNewPos))
      (send player set-xpos (- (send player get-xpos) 1))
      (send player set-ypos (- (send player get-ypos) 1))
      (ani player xNewPos yNewPos)  
    )


    ((and (< (send player get-xpos)  xNewPos ) (< (send player get-ypos) yNewPos))
      (send player set-xpos (+ (send player get-xpos) 1))
      (send player set-ypos (+ (send player get-ypos) 1))
      (ani player xNewPos yNewPos)
    )

    ((and (< (send player get-xpos)  xNewPos ) (> (send player get-ypos) yNewPos))
      (send player set-xpos (+ (send player get-xpos) 1))
      (send player set-ypos (- (send player get-ypos) 1))
      (ani player xNewPos yNewPos)
    )

    ((and (> (send player get-xpos)  xNewPos ) (< (send player get-ypos) yNewPos))
      (send player set-xpos (- (send player get-xpos) 1))
      (send player set-ypos (+ (send player get-ypos) 1))
      (ani player xNewPos yNewPos)
    )
    (else
      0
    )
  )
)

(define (animball ball xNewPos yNewPos)
;(cond (equal? (send ball get-tag)  ball"))  
  (sleep/yield 0.005)
  (send canvas refresh-now)
  (collisionBM Ball)
  (collisionBL Ball xNewPos yNewPos)
  (cond
    ((and (equal? (send ball get-xpos)  xNewPos) (not (equal? (send ball get-ypos) yNewPos))) ;si ya llego a la pos x y si no ha llegado a la pos y
     (cond
       ((> (send ball get-ypos) yNewPos) ;si ya llego a x, solo se mueve en y
        (send ball set-ypos (- (send ball get-ypos) 1))
        (animball ball xNewPos yNewPos))
       
       ((< (send ball get-ypos) yNewPos)
        (send ball set-ypos (+ (send ball get-ypos) 1))
        (animball ball xNewPos yNewPos))
       )
     )
     ((and (not(equal? (send ball get-xpos)  xNewPos)) (equal? (send ball get-ypos) yNewPos))
     (cond
       ((> (send ball get-xpos) xNewPos)
        (send ball set-xpos (- (send ball get-xpos) 1))
        (animball ball xNewPos yNewPos))
       
       ((< (send ball get-xpos) xNewPos)
        (send ball set-xpos (+ (send ball get-xpos) 1))
        (animball ball xNewPos yNewPos))
       )
     )
    ((and (equal? (send ball get-xpos)  xNewPos) (equal? (send ball get-ypos) yNewPos)) 0)
    ((and (> (send ball get-xpos)  xNewPos) (> (send ball get-ypos) yNewPos))
      (send ball set-xpos (- (send ball get-xpos) 1))
      (send ball set-ypos (- (send ball get-ypos) 1))
      (animball ball xNewPos yNewPos)  
    )


    ((and (< (send ball get-xpos)  xNewPos ) (< (send ball get-ypos) yNewPos))
      (send ball set-xpos (+ (send ball get-xpos) 1))
      (send ball set-ypos (+ (send ball get-ypos) 1))
      (animball ball xNewPos yNewPos)
    )

    ((and (< (send ball get-xpos)  xNewPos ) (> (send ball get-ypos) yNewPos))
      (send ball set-xpos (+ (send ball get-xpos) 1))
      (send ball set-ypos (- (send ball get-ypos) 1))
      (animball ball xNewPos yNewPos)
    )

    ((and (> (send ball get-xpos)  xNewPos ) (< (send ball get-ypos) yNewPos))
      (send ball set-xpos (- (send ball get-xpos) 1))
      (send ball set-ypos (+ (send ball get-ypos) 1))
      (animball ball xNewPos yNewPos)
    )
    (else
      0
    )
  )
)


(define (collisionBL ball xNewPos yNewPos) ;colision bola-limites cancha
  (cond ((equal? (send ball get-ypos) 392 ) ;limite inferior
        ;;(send ball set-ypos (* (send ball get-ypos) 1))
        ;;(send ball set-xpos (* (send ball get-xpos) -1))
        (print "inf"))
        ;(send canvas refresh-now))
        ((equal? (send ball get-ypos) 14);limite superior
         ;(send ball set-ypos (* (send ball get-ypos) 1))
         ;(send ball set-xpos (* (send ball get-xpos) -1))
        ;(send canvas refresh-now))
         (print "sup"))
         ((equal? (send ball get-xpos) 598) ;limite derecho
          ;(send ball set-ypos (* (send ball get-ypos) -1))
          ;(send ball set-xpos (* (send ball get-xpos) 1))
        ;(send canvas refresh-now))
          (print "der"))
         ((equal? (send ball get-xpos) 22) ;limite izquierdo
          ;(send ball set-ypos (* (send ball get-ypos) -1))
          ;(send ball set-xpos (* (send ball get-xpos) 1)))
        ;(send canvas refresh-now)
          (print "izq"))
         )
  )


(define (collisionBM ball) ;colision bola-marco
  (cond ((and (equal? (send ball get-xpos) 25) ;marco izquierdo
          (and (>= (send ball get-ypos) 180) (<= (send ball get-ypos) 225))
          (set! scoreTeam1 (+ scoreTeam1 1))))
         ((and (equal? (send ball get-xpos) 615) ;marco derecho
          (and (>= (send ball get-ypos) 180) (<= (send ball get-ypos) 225))
          (set! scoreTeam2 (+ scoreTeam2 1))))))

  
; jugadores y bola ----------------------------------------------------------
(define CRC1 (new player% (x 320) (y 10)))
(define CRC2 (new player% (x 320) (y 10)))
(define CRC3 (new player% (x 320) (y 10)))
(define CRC4 (new player% (x 320) (y 10)))
(define CRC5 (new player% (x 320) (y 10)))
(define CRC6 (new player% (x 320) (y 10)))
(define CRC7 (new player% (x 320) (y 10)))
(define CRC8 (new player% (x 320) (y 10)))
(define CRC9 (new player% (x 320) (y 10)))
(define CRC10 (new player% (x 320) (y 10)))
(define CRC11 (new player% (x 320) (y 10)))
(define BRA1 (new player% (x 360) (y 10)))
(define BRA2 (new player% (x 360) (y 10)))
(define BRA3 (new player% (x 360) (y 10)))
(define BRA4 (new player% (x 360) (y 10)))
(define BRA5 (new player% (x 360) (y 10)))
(define BRA6 (new player% (x 360) (y 10)))
(define BRA7 (new player% (x 360) (y 10)))
(define BRA8 (new player% (x 360) (y 10)))
(define BRA9 (new player% (x 360) (y 10)))
(define BRA10 (new player% (x 360) (y 10)))
(define BRA11 (new player% (x 360) (y 10)))
(define Ball (new ball% (x (+ 22 (random 597))) (y (+ 14 (random 397)))))


; pruebas ----------------------------------------------------------

(define players (list CRC1 CRC2 CRC3 CRC4 CRC5 CRC6 CRC7 CRC8 CRC9 CRC10 CRC11 BRA1 BRA2 BRA3 BRA4 BRA5 BRA6 BRA7 BRA8 BRA9 BRA10 BRA11))
(define coords (list (list 50 50) (list 654 200) 
                     (list 50 80) (list 456 200)
                     (list 465 50) (list 450 234)
                     (list 50 657) (list 254 200)
                     (list 23 50) (list 450 12)
                     (list 50 345) (list 32 200)
                     (list 345 50) (list 450 46)
                     (list 50 123) (list 234 200)
                     (list 245 50) (list 450 333)
                     (list 50 555) (list 222 200)
                     (list 267 50) (list 450 111))
  
  )

(send frame show #t)

 

