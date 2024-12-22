(ql:quickload :cl-charms)

(defpackage :rogue
  (:use :cl)
  (:export :main))

(in-package :rogue)

(defun main ()
  (let* ((game-map (load-map))
	 (width (array-dimension game-map 1))
	 (height (array-dimension game-map 0))
	 (p-x 5)
	 (p-y 5))
    (charms:with-curses ()
      (loop
	(charms/ll:clear)
	(dotimes (row height)
	  (dotimes (col width)
	    (charms/ll:mvaddch row col (char-code (aref game-map row col)))))
	(charms/ll:mvaddch p-y p-x (char-code #\@))
	(charms/ll:mvaddstr (+ height 1) 0 "Welcome to the dungeon")
	(charms/ll:refresh)
	(let ((ch (charms/ll:getch)))
	  (case (code-char ch)
	    (#\h (decf p-x))
	    (#\j (incf p-y))
	    (#\k (decf p-y))
	    (#\l (incf p-x))
	    (#\y (progn (decf p-y) (decf p-x)))
	    (#\u (progn (decf p-y) (incf p-x)))
	    (#\b (progn (incf p-y) (decf p-x)))
	    (#\n (progn (incf p-y) (incf p-x)))
	    (#\q (return))
	    (otherwise nil)))))))
	  
  
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
      

		       
      
