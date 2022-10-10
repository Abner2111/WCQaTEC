#lang racket/gui

(require racket/gui/base)

(define scoreTeam1 0)
(define scoreTeam2 0)

; Creates window frame
(define frame (new frame%
                   [label "Example"]
                   [width 640]
                   [height 500]))

; Creates a canvas into the mainframe
(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc draw-bitmap(read-bitmap "Resources/field.jpg") 0 35 )
                (send dc draw-text (~a "CRC  " scoreTeam1 " BRA  " scoreTeam2) 10 5)
                )
              ]
             )

(send frame show #t)