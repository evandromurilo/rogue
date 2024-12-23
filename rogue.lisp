(ql:quickload :cl-charms)

(defpackage :rogue
  (:use :cl)
  (:export :main))

(in-package :rogue)

(defstruct game-state
  (map nil)
  (width 0)
  (height 0)
  (msg "Welcome to the dungeon")
  (discovered nil)
  (p-x 5)
  (p-y 5))

(defun main ()
  (let ((gs (init-game-state)))
    (charms:with-curses ()
      (charms/ll:start-color)
      (charms/ll:init-pair 1 charms/ll:COLOR_WHITE charms/ll:COLOR_BLACK)
      (charms/ll:init-pair 2 charms/ll:COLOR_YELLOW charms/ll:COLOR_BLACK)
      
      (loop
	(charms/ll:clear)
	(dotimes (row (game-state-height gs))
	  (dotimes (col (game-state-width gs))
	    (if (is-visible gs col row)
		(setf attr (charms/ll:color-pair 1))
		(setf attr (charms/ll:color-pair 2)))
	    (charms/ll:attron attr)
	    (charms/ll:mvaddch row col (char-code (visible-tile-at gs col row)))))
	(setf attr (charms/ll:color-pair 1))
	(charms/ll:attron attr)
	(charms/ll:mvaddch (game-state-p-y gs) (game-state-p-x gs) (char-code #\@))
	(charms/ll:mvaddstr (+ (game-state-height gs) 1) 0 (game-state-msg gs))
	(charms/ll:refresh)
	(let ((ch (charms/ll:getch)))
	  (case (code-char ch)
	    (#\h (attempt-move gs -1  0))
	    (#\j (attempt-move gs  0  1))
	    (#\k (attempt-move gs  0 -1))
	    (#\l (attempt-move gs  1  0))
	    (#\y (attempt-move gs -1 -1))
	    (#\u (attempt-move gs  1 -1))
	    (#\b (attempt-move gs -1  1))
	    (#\n (attempt-move gs  1  1))
	    (#\q (return))
	    (otherwise nil)))))))

(defun init-game-state ()
  (let ((gs (make-game-state
	     :map (load-map))))
    (setf (game-state-width gs)
	  (array-dimension (game-state-map gs) 1))
    (setf (game-state-height gs)
	  (array-dimension (game-state-map gs) 0))
    (setf (game-state-discovered gs) (make-array (list (game-state-height gs) (game-state-width gs)) :initial-element nil))
    (discover gs)
    gs))

(defun tile-at (gs x y)
  (aref (game-state-map gs) y x))

(defun visible-tile-at (gs x y)
  (if (aref (game-state-discovered gs) y x)
      (tile-at gs x y)
      #\ ))

(defun discover (gs)
  (let ((view-range 2)
	(x (game-state-p-x gs))
	(y (game-state-p-y gs))
	(w (game-state-width gs))
	(h (game-state-height gs)))
    (let ((lx (max (- x view-range)  0)) ;; low x
	  (hx (min (+ x view-range)  (- w 1))) ;; high x
	  (ly (max (- y view-range)  0)) ;; low y
	  (hy (min (+ y view-range)  (- h 1)))) ;; high y
      (loop for tx from lx to hx do
	(loop for ty from ly to hy do
	  (if (has-direct-path gs x y tx ty)
	      (setf (aref (game-state-discovered gs) ty tx) t)))))))

(defun has-direct-path (gs sx sy tx ty)
  (cond ((and (= sx tx) (= sy ty)) t) ;; already there
	((eq (tile-at gs sx sy) #\#) nil) ;; cannot pass a wall
	((and (> sx tx) (= sy ty))
	 (has-direct-path gs (- sx 1) sy tx ty)) ;; directly to the left
	((and (< sx tx) (= sy ty))
	 (has-direct-path gs (+ sx 1) sy tx ty)) ;; directly to the right
	((and (= sx tx) (> sy ty))
	 (has-direct-path gs sx (- sy 1) tx ty)) ;; directly above
	((and (= sx tx) (< sy ty))
	 (has-direct-path gs sx (+ sy 1) tx ty)) ;; directly below
	((and (< sx tx) (< sy ty))
	 (has-direct-path gs (+ sx 1) (+ sy 1) tx ty)) ;; diagonal
	((and (< sx tx) (> sy ty))
	 (has-direct-path gs (+ sx 1) (- sy 1) tx ty)) ;; diagonal
	((and (> sx tx) (> sy ty))
	 (has-direct-path gs (- sx 1) (- sy 1) tx ty)) ;; diagonal
	((and (> sx tx) (< sy ty))
	 (has-direct-path gs (- sx 1) (+ sy 1) tx ty)) ;; diagonal
	(t nil)))

(defun is-visible (gs tx ty)
  (let ((view-range 2)
	(dx (abs (- (game-state-p-x gs) tx)))
	(dy (abs (- (game-state-p-y gs) ty))))
    (and (<= (max dx dy) view-range)
	 (has-direct-path gs (game-state-p-x gs) (game-state-p-y gs) tx ty))))
	   
(defun attempt-move (gs dx dy)
  (let ((x (+ (game-state-p-x gs) dx))
	(y (+ (game-state-p-y gs) dy)))
    (case (tile-at gs x y)
      (#\# (setf (game-state-msg gs) "There is a wall there!"))
      (otherwise (progn
		   (setf (game-state-msg gs) "")
		   (setf (game-state-p-x gs) x)
		   (setf (game-state-p-y gs) y)
		   (discover gs))))))
    
(defun load-map ()
  (with-open-file (stream "map.txt" :direction :input)
    (let ((lines (loop for line = (read-line stream nil nil)
		       while line
		       collect line)))
      (let* ((height (length lines))
	     (width (apply #'max (mapcar #'length lines)))
	     (game-map (make-array (list height width) :initial-element #\  )))
	(dotimes (row height)
	  (let ((line (nth row lines)))
	    (dotimes (col width)
	      (when (< col (length line))
		(setf (aref game-map row col) (char line col))))))
	game-map))))
      

		       
      
