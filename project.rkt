#lang racket
(require gigls/unsafe)


;Citation: procedure "paste-and-scale!" is copied from http://www.cs.grinnell.edu/~klingeti/courses/s2017/csc151/readings/collage-reading,
;created by Charlie Curtsinger, Janet Davis, Titus Klinge, Samuel A. Rebelsky, and Jerod Weinman.

(define image-series
  (lambda (n width height)
    (random-seed n)
    (let* ([canvas (image-compute (lambda (col row)
                                    (let* ([x (sqrt (+ (square (- col 400)) (square (- row 400))))]
                                           [red (+ 80 (random 101))]
                                           [green (+ 85 (random 101))]
                                           [blue (+ 105 (random 101))]
                                           [red1 (+ 255 (* (/ (- red 255) (* 400 (sqrt 2))) x))]
                                           [green1 (+ 255 (* (/ (- green 255) (* 400 (sqrt 2))) x))]
                                           [blue1 (+ 255 (* (/ (- blue 255) (* 400 (sqrt 2))) x))])
                                      (irgb red1 green1 blue1))) 800 800)]
           [draw-circle! (lambda ()
                           (let ([x-center (* 400 (/ (+ 99 (random 3)) 100))]
                                 [y-center (* 400 (/ (+ 99 (random 3)) 100))]
                                 [h-radius (* 200 (/ (+ 96 (random 9)) 100))]
                                 [v-radius (* 200 (/ (+ 96 (random 9)) 100))])
                             (image-select-ellipse! canvas REPLACE 
                                                    (- x-center h-radius) 
                                                    (- y-center v-radius)
                                                    (* 2 h-radius)
                                                    (* 2 v-radius))
                             (image-stroke-selection! canvas)))]
           [pick-spot! (lambda (x-center y-center)
                         (context-set-fgcolor! (irgb 240 240 240))
                         (image-select-ellipse! canvas REPLACE 
                                                (- x-center 62) (- y-center 62) 124 124)
                         (image-fill-selection! canvas))]
           [draw-spot! (lambda (x)
                         (cond [(= x 0)
                                (pick-spot! 400 200)]
                               [(= x 1)
                                (pick-spot! 517.56 238.20)]
                               [(= x 2)
                                (pick-spot! 590.21 338.20)]
                               [(= x 3)
                                (pick-spot! 590.21 461.80)]
                               [(= x 4)
                                (pick-spot! 517.56 561.80)]
                               [(= x 5)
                                (pick-spot! 400 600)]
                               [(= x 6)
                                (pick-spot! 282.44 561.80)]
                               [(= x 7)
                                (pick-spot! 209.79 461.80)]
                               [(= x 8)
                                (pick-spot! 209.79 338.20)]
                               [else
                                (pick-spot! 282.44 238.20)]))]
           [pick-turtle! (lambda (x y)
                           (let ([tommy (turtle-new canvas)])
                             (turtle-teleport! tommy x y)
                             (let* ([ran (random 10)]
                                   [turs (map turtle-clone (make-list (+ ran 5) tommy))])
                               (letrec ([kernel (lambda (turtle size)
                                                  (when (> size 0)
                                                    (turtle-set-brush! turtle "2. Block 02" size)
                                                    (turtle-turn! turtle (random 360))
                                                    (turtle-forward! turtle (+ size (random 15)))
                                                    (kernel turtle (- size 1))))])
                                 (map kernel turs (make-list (+ ran 5) 25))))))]
           [turtle-crawl! (lambda (z)
                            (cond [(= z 0)
                                (pick-turtle! 200 400)]
                               [(= z 1)
                                (pick-turtle! 238.20 517.56)]
                               [(= z 2)
                                (pick-turtle! 338.20 590.21)]
                               [(= z 3)
                                (pick-turtle! 461.80 590.21)]
                               [(= z 4)
                                (pick-turtle! 561.80 517.56)]
                               [(= z 5)
                                (pick-turtle! 600 400)]
                               [(= z 6)
                                (pick-turtle! 561.80 282.44)]
                               [(= z 7)
                                (pick-turtle! 461.80 209.79)]
                               [(= z 8)
                                (pick-turtle! 338.20 209.79)]
                               [else
                                (pick-turtle! 238.20 282.44)]))]
           [paste-and-scale! (lambda (target width height)
                              (image-select-rectangle! target REPLACE 0 0 width height)
                              (let ([pasted (car (gimp-edit-paste (image-get-layer target) 1))])
                                (image-select-nothing! target)
                                (gimp-layer-scale pasted width height 1)
                                (gimp-image-flatten target)
                                target))]
           [target (image-new width height)])
      (context-set-fgcolor! "black")
      (context-set-brush! "2. Hardness 100" 5)
      (image-select-ellipse! canvas REPLACE 200 200 400 400)
      (image-stroke-selection! canvas)
      (repeat 30 draw-circle!)
      (image-select-nothing! canvas)
      (draw-spot! (random 10))
      (image-select-nothing! canvas)
      (turtle-crawl! (random 10))
      (image-select-all! canvas)
      (gimp-edit-copy-visible canvas)
      (paste-and-scale! target width height))))
