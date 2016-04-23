;; initialize pazzle
(defun init ()
  ;; initial pazzle list
  (defun init-loop (i)
    (if (> i 1)
	(cons 0  (init-loop (- i 1)))
	(list 0)))
  ;; set initial numbers
  (defun set-numbers (ls pazzle)
    (let ((a  (random 9)))
      (if (not (equal  (length ls) 0))
	  (if (equal (nth a pazzle) 0)
	      (progn
		(setf (nth a pazzle) (first ls))
		(set-numbers (rest ls) pazzle))
	      (set-numbers ls pazzle))
	  pazzle)))
  (set-numbers (list 1 2 3 4 5 6 7 8 ) (init-loop 9)))

;; print puzzle as a table
(defun print-pazzle (ls)
  (pprint (get-list-from-head ls 3))
  (pprint (get-list-from-head (cdddr ls) 3))
  (pprint (get-list-from-head (cdddr (cdddr ls)) 3)))

;; get elements 
(defun get-list-from-head (ls n)
  (if (> n 0)
      (cons (car ls) (get-list-from-head (cdr ls) (- n 1)))
      '()))

(defun find-zero-pos (pazzle pos)
  (if (equal (car pazzle) 0)
      pos
      (find-zero-pos (cdr pazzle) (+ pos 1))))

(defun move-panel (pazzle direct)
  (let ((zero (find-zero-pos pazzle 0)))
    (cond
      ((equal direct 'u)
       (if (and (>= zero 6) (<= zero 8))
	   (error "**invalid input**")
	   (rotatef (nth zero pazzle) (nth (+ zero 3) pazzle))))
      ((equal direct 'd)
       (if (and (>= zero 0) (<= zero 2))
	   (error "**invalid input**")
	   (rotatef (nth zero pazzle) (nth (- zero 3) pazzle))))
      ((equal direct 'l)
       (if (or (equal zero 2) (equal zero 5) (equal zero 8))
	   (error "**invalid input**")
	   (rotatef (nth zero pazzle) (nth (+ zero 1) pazzle))))
      ((equal direct 'r)
       (if (or (equal zero 0) (equal zero 3) (equal zero 6))
	   (error "**invalid input**"))
       	   (rotatef (nth zero pazzle) (nth (- zero 1) pazzle))))

;;  (rotatef (nth panel-pos pazzle) (nth zero pazzle))
  (print-pazzle pazzle)
  pazzle))
