;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake-test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

    
   
    
(require 2htdp/image)
(require 2htdp/universe)
(define SCALE 10)
(define SCENE  (place-image (square 1600 "solid" "black") 0 0 (empty-scene 800 800)))
(define SEGMENT (circle 5 "solid" "red"))
(define FOOD (circle 5 "solid" "blue"))
(define-struct world (worm food score))
(define-struct worm (head segments))
(define-struct head (posn dir))
(define-struct segment (posn id))
(define-struct food (posn))
;helper functions
(define (sub10 x)
  (- x 10))

(define (add10 x)
  (+ x 10))

(define (add-to-list l x)
  (cond
    [(empty? l) (cons x empty)]
    [else (cons (first l) (add-to-list (rest l) x))]))
(define (get-segment l id)
  (cond
    [(empty? l) "error"]
    [(= (segment-id (first l)) id) (first l)]
    [else (get-segment (rest l)id)]))
(define (posn=? a b)
  (cond
    [(and (= (posn-x a) (posn-x b)) (= (posn-y a) (posn-y b))) #t]
    [else #f]))

;controlling functions

(define (move-worm l)
  (cons (make-segment (head-posn (worm-head (world-worm l))) 1)(move-segments (worm-segments (world-worm l)) empty (worm-segments (world-worm l)))))
(define (move-segments l w stor) 
(cond
      [(empty? l) w]
      [(= (segment-id (first l)) 1) (move-segments (rest l)w stor)]
      [else (cond
      [(empty? w) (move-segments (rest l) (cons (make-segment (segment-posn (get-segment stor (sub1 (segment-id (first l))))) (segment-id (first l)) )empty)stor)]
      [else (move-segments (rest l) (add-to-list w (make-segment (segment-posn (get-segment stor (sub1 (segment-id (first l))))) (segment-id (first l))))stor)])]))

(define (controller w cmd)
  (let* ([x (posn-x (head-posn (worm-head (world-worm w))))]
         [y (posn-y (head-posn (worm-head (world-worm w))))]
         [dir (head-dir (worm-head (world-worm w)))]
         [head (worm-head (world-worm w))]
         [worm (world-worm w)]
         [food (world-food w)]
         
         )
    (cond
      [(and (key=? cmd "w") (false? (string=? dir "d"))) (make-world (make-worm (make-head (head-posn head) "u") (worm-segments worm)) food "score")]
      [(and (key=? cmd "a") (false? (string=? dir "r"))) (make-world (make-worm (make-head (head-posn head) "l") (worm-segments worm)) food "score")]
      [(and (key=? cmd "s") (false? (string=? dir "u"))) (make-world (make-worm (make-head (head-posn head) "d") (worm-segments worm)) food "score")]
      [(and (key=? cmd "d") (false? (string=? dir "l"))) (make-world (make-worm (make-head (head-posn head) "r") (worm-segments worm)) food "score")]
      [else w])))


(define (move w)
  (let* ([x (posn-x (head-posn (worm-head (world-worm w))))]
         [y (posn-y (head-posn (worm-head (world-worm w))))]
         [head-posn (head-posn (worm-head (world-worm w)))]
         [food-posn (food-posn  (world-food w))]
         [dir (head-dir (worm-head (world-worm w)))]
         [head (worm-head (world-worm w))]
         [worm (world-worm w)]
         [segments (worm-segments (world-worm w))]
         )
    (cond
      [(and (posn=? head-posn food-posn)(string=? dir "u")) (make-world (make-worm (make-head (make-posn x  (sub10 y))  "u") (move-worm w)) (make-food (make-posn (* (random 79) 10) (* (random 79) 10))) "score")]
      [(and (posn=? head-posn food-posn)(string=? dir "l")) (make-world (make-worm (make-head (make-posn  (sub10 x) y) "l") (move-worm w)) (make-food (make-posn (* (random 79) 10) (* (random 79) 10))) "score")]
      [(and (posn=? head-posn food-posn)(string=? dir "d")) (make-world (make-worm (make-head (make-posn  x  (add10 y)) "d") (move-worm w)) (make-food (make-posn (* (random 79) 10) (* (random 79) 10))) "score")]
      [(and (posn=? head-posn food-posn)(string=? dir "r")) (make-world (make-worm (make-head (make-posn  (add10 x) y) "r") (move-worm w)) (make-food (make-posn (* (random 79) 10) (* (random 79) 10))) "score")]
      [(string=? dir "u") (make-world (make-worm (make-head (make-posn x  (sub10 y))  "u") (move-worm w)) food "score")]
      [(string=? dir "l") (make-world (make-worm (make-head (make-posn  (sub10 x) y) "l") (move-worm w)) food "score")]
      [(string=? dir "d") (make-world (make-worm (make-head (make-posn  x  (add10 y)) "d") (move-worm w)) food "score")]
      [(string=? dir "r") (make-world (make-worm (make-head (make-posn  (add10 x) y) "r") (move-worm w)) food "score")])))


(define (render-segments l)
  (cond
    [(empty? l) SCENE]
    [else (place-image SEGMENT (posn-x (segment-posn (first l))) (posn-y (segment-posn (first l))) (render-segments (rest l)))]))

(define (render w)
  (let* ([x (posn-x (head-posn (worm-head (world-worm w))))]
         [y (posn-y (head-posn (worm-head (world-worm w))))]
         [dir (head-dir (worm-head (world-worm w)))]
         [head (worm-head (world-worm w))]
         [worm (world-worm w)])
         (place-image SEGMENT x y (place-image FOOD (posn-x (food-posn (world-food w))) (posn-y (food-posn (world-food)))(render-segments (worm-segments (world-worm w)))) )))
(define (stop w)
  (cond
    [(or (wall-collide w) (segment-collide (worm-segments (world-worm w)) (worm-head (world-worm w)))) #t]
    [else #f]))
(define (wall-collide w)
   (let* ([x (posn-x (head-posn (worm-head (world-worm w))))]
         [y (posn-y (head-posn (worm-head (world-worm w))))]
         )
     (cond
      [(>= x 800) #t]
      [(>= y 800) #t]
       [(<= x 0) #t]
      [(<= y 0) #t]
       [else false])))
(define (segment-collide l head)
  
  (cond
    [(empty? l) #f]
    [(posn=? (segment-posn (first l)) (head-posn head)) #t]
    [else (segment-collide (rest l)head)]))

(big-bang (move (make-world (make-worm (make-head (make-posn 200 200) "d") (cons (make-segment (make-posn 200 190)  1) (cons (make-segment (make-posn 200 180) 2) (cons (make-segment (make-posn 200 170) 3) (cons (make-segment (make-posn 200 160) 4) (cons (make-segment (make-posn 200 150) 5) (cons (make-segment (make-posn 200 140) 6) (cons (make-segment (make-posn 200 130) 7) (cons (make-segment (make-posn 200 120) 8) (cons (make-segment (make-posn 200 110) 9) (cons (make-segment (make-posn 200 90) 10) (cons (make-segment (make-posn 200 80) 11) (cons (make-segment (make-posn 200 70) 12) empty))))))))))))) (make-food (make-posn (* (random 79) 10) (* (random 79) 10))) "score"))
          (on-key controller)
          (on-tick move)
          (to-draw render)
          (stop-when stop))

