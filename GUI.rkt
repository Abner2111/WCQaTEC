#lang racket/gui

(require racket/gui/base)

(define scoreTeam1 0)
(define scoreTeam2 0)
(define xRec 10)
(define yRec 10)

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
                (send dc draw-bitmap(read-bitmap "Resources/field.jpg") 0 35 )
                (send dc draw-text (~a "CRC  " scoreTeam1 " BRA  " scoreTeam2) 10 5)
                (send dc draw-rectangle xRec yRec 30 30)
                )
              ]
             )
  )

; Make a button in the frame
(new button% [parent frame]
             [label "prueba"]
             [callback (lambda (button event)
                         (set! scoreTeam1 (+ scoreTeam1 1))
                         (animation 320 250)
                         
                         
                         )
                       ]
             )

(define (animation xNewPos yNewPos)
  (cond
    ((and (>= xRec xNewPos) (>= yRec yNewPos)) 0)
    (else
     (set! xRec (+ xRec 5))
     (set! yRec (+ yRec 5))
     (print (~a xRec "  " yRec))
     (sleep 0.1)
     (send canvas refresh-now)
     (animation xNewPos yNewPos)
    )
  )
  )
(send frame show #t)