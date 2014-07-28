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

     (strategy1
        (lambda ()
          (let* ((lambda-man-next-pos (move lambda-man-pos lambda-man-dir))
                 (cell (lookup-map current-map lambda-man-next-pos)))
            (if (not (= cell WALL))
                (tuple state lambda-man-dir)
                (tuple (+ state 1) (turn-clockwise lambda-man-dir))
                ))))

     (strategy3
        (lambda ()
	  (tuple state (traverse lambda-man-pos current-map))	  
	  ))
     
     (strategy2
        (lambda ()
          (if (f UP) (tuple state UP)
              (if (f RIGHT) (tuple state RIGHT)
                  (if (f DOWN) (tuple state DOWN)
                      (if (f LEFT) (tuple state LEFT)
                          (strategy3)))))))

     )

    (strategy3)))

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


(define (pos-eq p1 p2)
  (let ((x1 (fst p1)) (y1 (snd p1)) (x2 (fst p2)) (y2 (snd p2)))
       (and (= x1 x2) (= y1 y2))))

; Pos -> [Pos]
(define (neighbors pos)
  (let ((x (fst pos)) (y (snd pos)))
    (list (tuple (+ x 1) y)
	  (tuple (- x 1) y)
	  (tuple x (+ y 1))
	  (tuple x (- y 1)))))

; Pos -> [(Pos, Dir)]
(define (neighbors-with-dir pos)
  (let ((x (fst pos)) (y (snd pos)))
    (list (tuple (tuple (+ x 1) y) RIGHT)
	  (tuple (tuple (- x 1) y) LEFT)
	  (tuple (tuple x (+ y 1)) DOWN)
	  (tuple (tuple x (- y 1)) UP))))

(define (traverse pos current-map)
  (letrec
      ((f (lambda (ps visited) ; f :: [(Pos,Dir)] -> [Pos] -> Dir
            (letrec
                ((g (lambda (ps visited next-ps) ; g :: [(Pos,Dir)] -> [Pos] -> [(Pos,Dir)] -> Dir
                      (if (null? ps)
                          (f next-ps visited)
                          (let* ((q (car ps))
                                 (qs (cdr ps))
                                 (cell (lookup-map current-map (fst q))))
                            (if (or (any (lambda (e) (pos-eq (fst e) (fst q))) visited) (= cell WALL))
                                (g qs visited next-ps)
                                (if (is-item cell)
                                    (snd q)
                                    (g qs (cons q visited) (append (map (lambda (pos) (tuple pos (snd q))) (neighbors (fst q))) next-ps))))))
                      )))
              (g ps visited nil)
              ))
          ))
    (f (neighbors-with-dir pos) nil)
    ))


; 見つかった場合には1要素リストを、見つからなかった場合には空リストを返す
(define (find f xs)
  (if (null? xs)
      nil
      (if (f (car xs))
          (list (car xs))
          (find f (cdr xs)))))

(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (concat xs)
  (if (null? xs)
      nil
      (append (car xs) (concat (cdr xs)))))

(define (map f xs)
  (if (null? xs)
      nil
      (cons (f (car xs)) (map f (cdr xs)))))
