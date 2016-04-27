;; initialize pazzle
(defun init ()
  ;; initial puzzle list
  (defun init-loop (i)
    (if (> i 1)
	(cons 0  (init-loop (- i 1)))
	(list 0)))
  ;; set initial numbers
  (defun set-numbers (ls puzzle)
    (let ((a  (random 9)))
      (if (not (equal  (length ls) 0))
	  (if (equal (nth a puzzle) 0)
	      (progn
		(setf (nth a puzzle) (first ls))
		(set-numbers (rest ls) puzzle))
	      (set-numbers ls puzzle))
	  puzzle)))
  (set-numbers (list 1 2 3 4 5 6 7 8 ) (init-loop 9)))

;; print puzzle as a table
(defun print-puzzle (ls)
  (pprint (get-list-from-head ls 3))
  (pprint (get-list-from-head (cdddr ls) 3))
  (pprint (get-list-from-head (cdddr (cdddr ls)) 3)))

;; get elements 
(defun get-list-from-head (ls n)
  (if (> n 0)
      (cons (car ls) (get-list-from-head (cdr ls) (- n 1)))
      '()))

(defun find-zero-pos (puzzle pos)
  (if (equal (car puzzle) 0)
      pos
      (find-zero-pos (cdr puzzle) (+ pos 1))))

;; move a movable panel toward to selected direction
(defun move-panel (puzzle direct)
  (let ((zero (find-zero-pos puzzle 0)))
    (cond
      ((equal direct 'u)
       (if (and (>= zero 6) (<= zero 8))
	   (error "**invalid input**")
	   (rotatef (nth zero puzzle) (nth (+ zero 3) puzzle))))
      ((equal direct 'd)
       (if (and (>= zero 0) (<= zero 2))
	   (error "**invalid input**")
	   (rotatef (nth zero puzzle) (nth (- zero 3) puzzle))))
      ((equal direct 'l)
       (if (or (equal zero 2) (equal zero 5) (equal zero 8))
	   (error "**invalid input**")
	   (rotatef (nth zero puzzle) (nth (+ zero 1) puzzle))))
      ((equal direct 'r)
       (if (or (equal zero 0) (equal zero 3) (equal zero 6))
	   (error "**invalid input**"))
       	   (rotatef (nth zero puzzle) (nth (- zero 1) puzzle))))

;;  (rotatef (nth panel-pos puzzle) (nth zero puzzle))
  (print-puzzle puzzle)
  puzzle))

;; calculate numbers of panels on different place
(defun calc-diff-num (puzzle)
  (defun calc-diff-num-loop (puzzle num-list diff-num)
    (if (equal puzzle '())
	diff-num
	(if (and (not (equal (car puzzle) (car num-list))) (not (equal (car puzzle) 0)))
	    (calc-diff-num-loop (cdr puzzle) (cdr num-list) (1+ diff-num))
	    (calc-diff-num-loop (cdr puzzle) (cdr num-list) diff-num))))
  (calc-diff-num-loop puzzle '(1 2 3 4 5 6 7 8) 0))

;; calculate the sum of manhattan distance
(defun calc-manhat-dist (puzzle)
  (defun calc-manhat-dist-loop (puzzle pos sum)
    (let ((one-cost-field '(0 1 2 1 2 3 2 3 4))
	  (two-cost-field '(1 0 1 2 1 2 3 2 3))
	  (three-cost-field '(2 1 0 3 2 1 4 3 2))
	  (four-cost-field '(1 2 3 0 1 2 1 2 3))
	  (five-cost-field '(2 1 2 1 0 1 2 1 2))
	  (six-cost-field '(3 2 1 2 1 0 3 2 1))
	  (seven-cost-field '(2 3 4 1 2 3 0 1 2))
	  (eight-cost-field '(3 2 3 2 1 2 1 0 1)))
      (if (equal puzzle '())
	  sum
	  (cond
	    ((equal (car puzzle) 0) (calc-manhat-dist-loop (cdr puzzle) (1+ pos) sum))
	    ((equal (car puzzle) 1) (calc-manhat-dist-loop (cdr puzzle) (1+ pos) (+ sum (nth pos one-cost-field))))
	    ((equal (car puzzle) 2) (calc-manhat-dist-loop (cdr puzzle) (1+ pos) (+ sum (nth pos two-cost-field))))
	    ((equal (car puzzle) 3) (calc-manhat-dist-loop (cdr puzzle) (1+ pos) (+ sum (nth pos three-cost-field))))
	    ((equal (car puzzle) 4) (calc-manhat-dist-loop (cdr puzzle) (1+ pos) (+ sum (nth pos four-cost-field))))
	    ((equal (car puzzle) 5) (calc-manhat-dist-loop (cdr puzzle) (1+ pos) (+ sum (nth pos five-cost-field))))
	    ((equal (car puzzle) 6) (calc-manhat-dist-loop (cdr puzzle) (1+ pos) (+ sum (nth pos six-cost-field))))
	    ((equal (car puzzle) 7) (calc-manhat-dist-loop (cdr puzzle) (1+ pos) (+ sum (nth pos seven-cost-field))))
	    ((equal (car puzzle) 8) (calc-manhat-dist-loop (cdr puzzle) (1+ pos) (+ sum (nth pos eight-cost-field))))
	     ))))
  (calc-manhat-dist-loop puzzle 0 0))
