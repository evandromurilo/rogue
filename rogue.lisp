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
  (p-x 5)
  (p-y 5))

(defun main ()
  (let ((gs (init-game-state)))
    (charms:with-curses ()
      (loop
	(charms/ll:clear)
	(dotimes (row (game-state-height gs))
	  (dotimes (col (game-state-width gs))
	    (charms/ll:mvaddch row col (char-code (tile-at gs row col)))))
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
    gs))

(defun tile-at (gs x y)
  (aref (game-state-map gs) x y))

(defun attempt-move (gs dx dy)
  (incf (game-state-p-x gs) dx)
  (incf (game-state-p-y gs) dy))
  
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
      

		       
      
