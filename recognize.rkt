;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname recognize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Starter Code
;; ***************************************************
;;

(require "templates.rkt")

;; "templates.rkt" provides templates, a TemplateLibrary (see data definition)
;; It also provides the following test gestures for your recognizer: 
;;    testd testk tests testy testa testt


;; A Point is a (list Num Num)

;; A Gesture is a (listof (list Num Num))

;; A BoundingBox (BB) is a (list Point Point)
;; requires: the coordinate values in the first point are less than the
;;             respective values in the second point

;; A TemplateLibrary (TL) is a (listof (list Sym Gesture))
;; requires: the list is non-empty
;;           each Sym key is unqiue
;;           each Gesture value is not both vertical and horizontal
       



;; 3a)
;; These are helper functions. See assignment for design recipe requirements.

;; 3ai)

;; (get-x pt) produces the x-coordinate of point pt
;; Examples:
(check-expect (get-x (list 1 4)) 1)

;; get-x: Point -> Num
(define (get-x pt)
  (first pt))


;; (get-y pt) produces the y-coordinate of point pt
;; Examples:
(check-expect (get-y (list 1 4)) 4)

;; get-y: Point -> Num
(define (get-y pt)
  (second pt))

;; 3aii)

;; (translate-gesture ges x-offset y-offset) produces a new Gesture such that
;;    each Point in ges ((list x y)) now has value
;;    (list (+ x x-offset) (+ y y-offset))
;; Examples:
(check-expect (translate-gesture empty 1 2) empty)
(check-expect (translate-gesture (list (list 1 2) (list 12 13) (list 7 2))
                                 5 6)
              (list (list 6 8) (list 17 19) (list 12 8)))

;; translate-gesture: Gesture Num Num -> Gesture
(define (translate-gesture ges x-offset y-offset)
  (cond [(empty? ges) empty]
        [else (cons (list (+ x-offset (get-x (first ges)))
                          (+ y-offset (get-y (first ges))))
                    (translate-gesture (rest ges) x-offset y-offset))]))

;; 3aiii)

;; (scale-gesture ges x-scale y-scale) produces a new Gesture such that
;;    each Point in ges ((list x y)) now has value
;;    (list (* x x-scale) (* y y-scale))
;; Examples:
(check-expect (scale-gesture empty 12 6) empty)
(check-expect (scale-gesture (list (list 1 2) (list 12 13) (list 7 2))
                             3 4)
              (list (list 3 8) (list 36 52) (list 21 8)))

;; scale-gesture: Gesture Num Num -> Gesture
;; Requires:
;;    x-scale > 0
;;    y-scale > 0
(define (scale-gesture ges x-scale y-scale)
  (cond [(empty? ges) empty]
        [else (cons (list (* x-scale (get-x (first ges)))
                          (* y-scale (get-y (first ges))))
                    (scale-gesture (rest ges) x-scale y-scale))]))

;; 3aiv)

;; (get-b-box ges) produces the BoundingBox of the Gesture ges
;; Examples:
(check-expect (get-b-box (list (list 3 1) (list 2 5)))
              (list (list 2 1) (list 3 5)))
(check-expect (get-b-box (list (list 13 14) (list 22 105) (list 12 4)))
              (list (list 12 4) (list 22 105)))

;; get-b-box: Gesture -> BB
;; Requires: (length ges) > 1
(define (get-b-box ges)
  (list (list (min-x ges) (min-y ges))
        (list (max-x ges) (max-y ges))))


;; (min-x ges) produces the minimun x-coordinate of the Gesture ges
;; Examples:
(check-expect (min-x (list (list 1 2))) 1)
(check-expect (min-x (list (list 3 1) (list 2 5))) 2)

;; min-x: Gesture -> Num
;; Requires: (length ges) > 0
(define (min-x ges)
  (cond [(empty? (rest ges))
         (get-x (first ges))]
        [else (min (get-x (first ges))
                   (min-x (rest ges)))]))


;; (min-y ges) produces the minimun y-coordinate of the Gesture ges
;; Examples:
(check-expect (min-y (list (list 1 2))) 2)
(check-expect (min-y (list (list 3 1) (list 2 5))) 1)

;; min-y: Gesture -> Num
;; Requires: (length ges) > 0
(define (min-y ges)
  (cond [(empty? (rest ges))
         (get-y (first ges))]
        [else (min (get-y (first ges))
                   (min-y (rest ges)))]))


;; (max-x ges) produces the maximum x-coordinate of the Gesture ges
;; Examples:
(check-expect (max-x (list (list 1 2))) 1)
(check-expect (max-x (list (list 3 1) (list 2 5))) 3)

;; max-x: Gesture -> Num
;; Requires: (length ges) > 0
(define (max-x ges)
  (cond [(empty? (rest ges))
         (get-x (first ges))]
        [else (max (get-x (first ges))
                   (max-x (rest ges)))]))


;; (max-y ges) produces the maximum y-coordinate of the Gesture ges
;; Examples:
(check-expect (max-y (list (list 1 2))) 2)
(check-expect (max-y (list (list 3 1) (list 2 5))) 5)

;; max-y: Gesture -> Num
;; Requires: (length ges) > 0
(define (max-y ges)
  (cond [(empty? (rest ges))
         (get-y (first ges))]
        [else (max (get-y (first ges))
                   (max-y (rest ges)))]))




;; 3b)
;; Full design recipe required.

;; 3bi)

;; (gesture-length ges) produces the length of the Gesture ges
;; Examples:
(check-expect (gesture-length empty) 0)
(check-expect (gesture-length (list (list 1 1))) 0)
(check-expect (gesture-length (list (list 4 1) (list 16 6) (list 13 2)))
              18)

;; gesture-length: Gesture -> Num
(define (gesture-length ges)
  (cond [(empty? ges) 0]
        [(empty? (rest ges)) 0]
        [else (+ (dist-points (first ges) (second ges))
                 (gesture-length (rest ges)))]))

;; Tests:
(check-expect (gesture-length (list (list 16 6) (list 4 1)
                                    (list 7 5) (list 10 1)))
              23)                                     


;; (dist-points pt1 pt2) produces the distance between
;;    the points pt1 and pt2
;; Examples:
(check-expect (dist-points (list 4 1) (list 1 5)) 5)

;; dist-points: Point Point -> Num
(define (dist-points pt1 pt2)
  (sqrt (+ (sqr (- (get-x pt2) (get-x pt1)))
           (sqr (- (get-y pt2) (get-y pt1))))))

;; 3bii)

(define mygest1 (list (list 100 0) (list 200 100) (list 100 200)
                      (list 0 100) (list 100 50)))
(define mygest2 (list (list 12 13) (list 14 15) (list 16 17)
                      (list 18 19) (list 20 21)))

;; (get-points g nat-list) produces a new Gesture where each Point
;;    in the produced gesture is indexed by the corresponding Nat in
;;    nat-list
;; Examples:
(check-expect (get-points (list (list 1 1)) (list 0))
              (list (list 1 1)))
(check-expect (get-points mygest1 (list 0 0 2 4 4))
              (list (list 100 0) (list 100 0) (list 100 200)
                    (list 100 50) (list 100 50)))

;; get-points: Gesture (ne-listof Nat) -> Gesture
;; Requires:
;;    (length g) > 0
;;    each Nat in nat-list is in the range [0, (length g) - 1]
;;    nat-list is sorted in non-decreasing order
(define (get-points g nat-list)
  (cond [(empty? (rest nat-list))
         (list (point-at (first nat-list) g))]
        [else
         (cons (point-at (first nat-list) g)
               (get-points g (rest nat-list)))]))

;; Tests:
(check-expect (get-points mygest2 (list 2 1 3 4 0 2))
              (list (list 16 17) (list 14 15) (list 18 19)
                    (list 20 21) (list 12 13) (list 16 17)))


;; (point-at ind ges) produces the point at index ind
;;    of Gesture ges
;; Examples:
(check-expect (point-at 0 mygest1) (list 100 0))
(check-expect (point-at 4 mygest1) (list 100 50))

;; point-at: Nat Gesture -> Point
;; Requires:
;;    (length ges) > 0
;;    ind < (length ges)
(define (point-at ind ges)
  (cond [(zero? ind) (first ges)]
        [else (point-at (sub1 ind) (rest ges))]))

                              


;; 3c) Starter code definitions

;; 3ci)

;;(five-sample gesture) produces a sampling of gesture 5 points
;;  the first, n/4th, n/2th, 3n/4th, and last point.
;; Examples:
(check-expect (five-sample (list (list 1 1) (list 2 2)))
              (list (list 1 1) (list 1 1) (list 2 2) (list 2 2) (list 2 2)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                (list 5 5) (list 6 6) (list 7 7) (list 8 8)))
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))

;; five-sample: Gesture -> Gesture
;; requires: gesture is non-empty
(define (five-sample gesture)
  (list (point-at 0 gesture)
        (point-at (floor (* 0.25 (length gesture))) gesture)
        (point-at (floor (* 0.5 (length gesture))) gesture)
        (point-at (floor (* 0.75 (length gesture))) gesture)
        (point-at (- (length gesture) 1) gesture)))

;; Tests:
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 4 4)))
(check-expect (five-sample (list (list 1 1)))
              (list (list 1 1) (list 1 1) (list 1 1) (list 1 1) (list 1 1)))
(check-expect (five-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5)))
              (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))

;; 3cii)

;;(move-and-scale gesture x-scale y-scale) moves gesture to (0, 0) and
;;  scales it by (x-scale)x(y-scale)
;; Examples:
(check-expect (move-and-scale (list (list 1 1)) 1 1) (list (list 0 0)))
(check-expect (move-and-scale (list (list 1 5) (list 3 4)) 1 2)
              (list (list 0 2) (list 2 0)))

;; move-and-scale: Gesture Num Num -> Gesture
;; requires: gesture is non-empty
;;           x-scale > 0
;;           y-scale > 0
(define (move-and-scale gesture x-scale y-scale)
  (scale-gesture
   (translate-gesture gesture (- (min-x gesture)) (- (min-y gesture)))
   x-scale
   y-scale))

;; Test:
(check-expect (move-and-scale (list (list 5 5) (list 2 2)) 3 0.5)
              (list (list 9 1.5) (list 0 0)))

;; 3ciii)

(define min-width 30)
(define min-height 30)
(define norm-size 200)

;;(normalize-gesture gesture) normalizes gesture to (0,0) and a standard size
;; Examples:
(check-within (normalize-gesture (list (list 0 0) (list 100 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 100 0) (list 100 50) (list 200 50)))
              (list (list 0 0) (list 0 200) (list 200 200)) 0.01)

;; normalize-gesture: Gesture -> Gesture
;; requires:
;;    gesture is not both vertical and horizontal
;;    gesture is non-empty
(define (normalize-gesture gesture)
  (cond [(vertical? gesture)
         (move-and-scale gesture
                         1
                         (/ norm-size (max-y (translated-gesture gesture))))]
        [(horizontal? gesture)
         (move-and-scale gesture
                         (/ norm-size (max-x (translated-gesture gesture)))
                         1)]
        [else
         (move-and-scale gesture
                         (/ norm-size (max-x (translated-gesture gesture)))
                         (/ norm-size (max-y (translated-gesture gesture))))]))

;; Tests:
(check-within (normalize-gesture (list (list 0 0) (list 100 30)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 100 29)))
              (list (list 0 0) (list 200 29)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 30 100)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 29 100)))
              (list (list 0 0) (list 29 200)) 0.01)
(check-within (normalize-gesture (list (list 0 0) (list 400 400)))
              (list (list 0 0) (list 200 200)) 0.01)
(check-within (get-b-box
               (normalize-gesture (list (list 0 0) (list 400 400))))
              (list (list 0 0) (list 200 200)) 0.01)


;; (translated-gesture ges) translates the Gesture ges such that its
;;    BoundingBox's top left corner is (0, 0)
;; Examples:
(check-expect (translated-gesture (list (list 100 200) (list 200 500)))
              (list (list 0 0) (list 100 300)))

;; translated-gesture: Gesture -> Gesture
(define (translated-gesture ges)
  (translate-gesture ges (- (min-x ges)) (- (min-y ges))))


;; (vertical? ges) produces true if the Gesture ges is
;;    vertical, and false otherwise
;; Examples:
(check-expect (vertical? (list (list 100 100) (list 120 120) (list 129 129)))
              true)
(check-expect (vertical? (list (list 100 100) (list 120 120) (list 130 130)))
              false)

;; vertical?: Gesture -> Bool
;; Requires: (length ges) > 0
(define (vertical? ges)
  (< (- (max-x ges) (min-x ges))
     min-width))


;; (horizontal? ges) produces true if the Gesture ges is
;;    horizontal, and false otherwise
;; Examples:
(check-expect (horizontal? (list (list 100 100) (list 120 120) (list 129 129)))
              true)
(check-expect (horizontal? (list (list 100 100) (list 120 120) (list 130 130)))
              false)

;; horizontal?: Gesture -> Bool
;; Requires: (length ges) > 0
(define (horizontal? ges)
  (< (- (max-y ges) (min-y ges))
     min-height))             

;; 3civ)

;;(geometric-5match gesture1 gesture2) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with k points
;; Examples:
(check-within (geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50) (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30) (list 40 40) (list 40 40)))
               16.16 0.01)

;; geometric-5match: Gesture Gesture -> Num
;; requires: gesture1 and gesture2 are each not both vertical and horizontal
(define (geometric-5match gesture1 gesture2)
  (avg-dist (normalize-gesture (five-sample gesture1))
            (normalize-gesture (five-sample gesture2))))

;; Tests:
(check-within (geometric-5match (second (fourth templates))
                                (second (fourth templates)))
              0 0.1)              


;; (avg-dist ges1 ges2) calculates the average distance between the points
;;    in ges1 and ges2
;; Examples:
(check-expect (avg-dist (list (list 4 1)) (list (list 1 5))) 5)
(check-expect (avg-dist (list (list 5 2) (list 23 23))
                        (list (list 8 6) (list 20 19)))
              5)

;; avg-dist: Gesture Gesture -> Num
;; Requires: (length ges1) = (length ges2) > 0
(define (avg-dist ges1 ges2)
  (/ (sum-dist ges1 ges2) (length ges1)))


;; (sum-dist ges1 ges2) calculates the sum of the distances between the
;;    corresponding points in ges1 and ges2
;; Examples:
(check-expect (sum-dist empty empty) 0)
(check-expect (sum-dist (list (list 4 1)) (list (list 1 5))) 5)
(check-expect (sum-dist (list (list 5 2) (list 23 23))
                        (list (list 8 6) (list 20 19)))
              10)

;; sum-dist: Gesture Gesture -> Num
;; Requires: (length ges1) = (length ges2)
(define (sum-dist ges1 ges2)
  (cond [(empty? ges1) 0]
        [else (+ (dist-points (first ges1) (first ges2))
                 (sum-dist (rest ges1) (rest ges2)))]))

;; 3cv)

;;(five-point-rec candidate template-library) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (five-point-rec testd templates) 'd)
(check-expect (five-point-rec testk templates) 'k)

;; five-point-rec Gesture TL -> Sym
;; requires: candidate is not both vertical and horizontal
(define (five-point-rec candidate template-library)
  (first (closest-record candidate template-library))) 

;; Tests
(check-expect (five-point-rec tests templates) 's)
(check-expect (five-point-rec testy templates) 'y)
(check-expect (five-point-rec (second (third templates)) templates)
              'c)


;; (closest-record ges template-library) produces a (list Sym Gesture) that
;;    exists in template-library such that (geometric-5match ges Gesture) is
;;    the minimum geometric-5match value for all Gestures in all
;;    (list Sym Gesture)'s in template-library
;; Examples:
(check-expect (closest-record mygest1 (list (list 'apple mygest1)))
              (list 'apple mygest1))

;; closest-record: Gesture TL -> (list Sym Gesture)
;; Requires: ges is not both vertical and horizontal
(define (closest-record ges template-library)
  (cond [(empty? (rest template-library))
         (first template-library)]
        [(= (geometric-5match ges (second (first template-library)))
            (min-geometric-5match ges template-library))
         (first template-library)]
        [else
         (closest-record ges (rest template-library))]))


;; (min-geometric-5match ges template-library) produces the minimum
;;    geometric-5match value for all Gestures in all (list Sym Gesture)'s
;;    in template-library
;; Examples:
(check-within (min-geometric-5match
               (list (list 10 10) (list 30 30) (list 50 50)
                     (list 70 70) (list 80 80))
               (list (list 'apple
                           (list (list 10 10) (list 20 20) (list 30 30)
                                 (list 40 40) (list 40 40)))))
               16.16 0.01)

;; min-geometric-5match: Gesture TL -> Num
;; Requires: ges is not both vertical and horizontal
(define (min-geometric-5match ges template-library)
  (cond [(empty? (rest template-library))
         (geometric-5match ges (second (first template-library)))]
        [else
         (min (geometric-5match ges (second (first template-library)))
              (min-geometric-5match ges (rest template-library)))]))




;; 3d)

;; (sub-sample ges k) produces a sub-sample of k points that represents
;;    the Gesture ges
;; Examples:
(check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)
                                 (list 5 5) (list 6 6) (list 7 7) (list 8 8))
                           5)
              (list (list 1 1) (list 3 3) (list 5 5) (list 7 7) (list 8 8)))

;; sub-sample: Gesture Nat -> Gesture
;; Requires:
;;    ges is non-empty
;;    k > 2
(define (sub-sample ges k)
  (points-upto k 0 ges))

;; Tests:
  (check-expect (sub-sample (list (list 1 1) (list 2 2) (list 3 3)
                                  (list 4 4) (list 5 5))
                            5)
                (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)))
                                
              
;; (points-upto k i ges) produces a sub-sample of k points that represents
;;    the Gesture ges by using i, which increments with each
;;    recursive call
;; Examples: see wrapper function sub-sample

;; points-upto: Nat Nat Gesture -> Gesture
;; Requires:
;;    ges is non-empty
;;    k > 2
(define (points-upto k i ges)
  (cond [(= i (- k 1))
         (list (point-at (- (length ges) 1) ges))]
        [else
         (cons (point-at (floor (* i (/ (length ges) (- k 1))))
                         ges) 
               (points-upto k (add1 i) ges))]))


;;(geometric-match gesture1 gesture2 k) produces the average distance between
;;  points in sub-sampled gesture1 and gesture2 after sub-sampling them with
;;  k points
;; Examples:
(check-within (geometric-match
               (list (list 10 10) (list 30 30) (list 50 50)
                     (list 70 70) (list 80 80))
               (list (list 10 10) (list 20 20) (list 30 30)
                     (list 40 40) (list 40 40))
               5)
              16.16 0.01)

;; geometric-match: Gesture Gesture Nat -> Num
;; Requires:
;;    gesture1 and gesture2 are each not both vertical and horizontal
;;    k > 2
(define (geometric-match gesture1 gesture2 k)
  (avg-dist (normalize-gesture (sub-sample gesture1 k))
            (normalize-gesture (sub-sample gesture2 k))))

;; Tests:
(check-within (geometric-match (second (fourth templates))
                               (second (fourth templates))
                               18)
              0 0.1)


;;(k-point-rec candidate template-library k) produces the symbol in
;;  template-library closest to candidate
;; Examples:
(check-expect (k-point-rec testd templates 34) 'd)
(check-expect (k-point-rec testk templates 15) 'k)

;; k-point-rec Gesture TL Nat -> Sym
;; Requires:
;;    candidate is not both vertical and horizontal
;;    k > 2
(define (k-point-rec candidate template-library k)
  (first (closest-record-k candidate template-library k)))

;; Tests:
(check-expect (k-point-rec tests templates 5) 's)
(check-expect (k-point-rec testy templates 20) 'y)
(check-expect (k-point-rec (second (third templates)) templates 12)
              'c)






;; (closest-record-k ges template-library k) produces a (list Sym Gesture) that
;;    exists in template-library such that (geometric-match ges Gesture) is
;;    the minimum geometric-match value for all Gestures in all
;;    (list Sym Gesture)'s in template-library
;; Examples:
(check-expect (closest-record-k mygest1 (list (list 'apple mygest1)) 5)
              (list 'apple mygest1))

;; closest-record-k: Gesture TL Nat -> (list Sym Gesture)
;; Requires:
;;    ges is not both vertical and horizontal
;;    k > 2
(define (closest-record-k ges template-library k)
  (cond [(empty? (rest template-library))
         (first template-library)]
        [(= (geometric-match ges (second (first template-library)) k)
            (min-geometric-match ges template-library k))
         (first template-library)]
        [else
         (closest-record-k ges (rest template-library) k)]))


;; (min-geometric-match ges template-library k) produces the minimum
;;    geometric-match value for all Gestures in all (list Sym Gesture)'s
;;    in template-library
;; Examples:
(check-within (min-geometric-match
               (list (list 10 10) (list 30 30) (list 50 50)
                     (list 70 70) (list 80 80))
               (list (list 'apple
                           (list (list 10 10) (list 20 20) (list 30 30)
                                 (list 40 40) (list 40 40))))
               5)
               16.16 0.01)

;; min-geometric-match: Gesture TL Nat -> Num
;; Requires:
;;    ges is not both vertical and horizontal
;;    k > 2
(define (min-geometric-match ges template-library k)
  (cond [(empty? (rest template-library))
         (geometric-match ges (second (first template-library)) k)]
        [else
         (min (geometric-match ges (second (first template-library)) k)
              (min-geometric-match ges (rest template-library) k))]))





