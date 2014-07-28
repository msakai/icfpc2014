; foo
(define (main initial-world ghost-progs) (tuple 42 step))

(define (step state world)
  (let* 
    ((current-map (tproj_4_0 world))
     (lambda-man (tproj_4_1 world))
     (ghosts (tproj_4_2 world))
     (fruits (tproj_4_3 world))
     (lambda-man-pos (tproj_5_1 lambda-man))
     (lambda-man-dir (tproj_5_2 lambda-man))
     (f (lambda (dir) (is-item (lookup-map current-map (move lambda-man-pos dir)))))
     )
    (if (f UP) (tuple state UP)
        (if (f RIGHT) (tuple state RIGHT)
            (if (f DOWN) (tuple state DOWN)
                (if (f LEFT) (tuple state LEFT)
                    (let* ((lambda-man-next-pos (move lambda-man-pos lambda-man-dir))
			   (cell (lookup-map current-map lambda-man-next-pos)))
                      (if (not (= cell WALL))
                          (tuple state lambda-man-dir)
                          (tuple (+ state 1) (turn-clockwise lambda-man-dir))
                          ))))))))

(define (is-item cell)
  (or (= cell PILL)
      (or (= cell POWERPILL)
          (= cell FRUIT))))

(define (nth xs i) (if (= i 0) (car xs) (nth (cdr xs) (- i 1))))

(define (any f xs)
  (if (null? xs)
      #f
      (or (f (car xs)) (any f (cdr xs)))))

(define (filter f xs)
  (if (null? xs)
      xs
      (if (f (car xs))
          (cons (car xs) (filter f (cdr xs)))
          (filter f (cdr xs)))))

(define (lookup-map map pos)
  (let ((x (fst pos)) (y (snd pos)))
       (nth (nth map y) x)))

(define (move pos dir)
   (let ((x (fst pos)) (y (snd pos)))
        (if (= dir UP)
            (tuple x (- y 1))
            (if (= dir RIGHT)
                (tuple (+ x 1) y)
                (if (= dir DOWN)
                    (tuple x (+ y 1))
                    (tuple (- x 1) y))))))

(define (turn-clockwise dir)
  (if (= dir UP) RIGHT
      (if (= dir RIGHT) DOWN
          (if (= dir DOWN) LEFT
              UP))))
