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
      (loop
	(charms/ll:clear)
	(dotimes (row (game-state-height gs))
	  (dotimes (col (game-state-width gs))
	    (charms/ll:mvaddch row col (char-code (visible-tile-at gs col row)))))
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
	  (hx (min (+ x view-range)  w)) ;; high x
	  (ly (max (- y view-range)  0)) ;; low y
	  (hy (min (+ y view-range)  h))) ;; high y
      (loop for x from lx to (- hx 1) do
	(loop for y from ly to (- hy 1) do
	  (setf (aref (game-state-discovered gs) y x) t))))))
	   
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
	     (width (length (car lines)))
	     (game-map (make-array (list height width))))
	(dotimes (row height)
	  (dotimes (col width)
	    (setf (aref game-map row col) (char (nth row lines) col))))
	game-map))))
      

		       
      
