(ql:quickload :cl-charms)

(defpackage :rogue
  (:use :cl)
  (:export :main))

(in-package :rogue)

(defun main ()
  (let ((game-map (make-array '(20 20) :initial-element #\.))
	(p-x 5)
	(p-y 5))
    (charms:with-curses ()
      (loop
	(charms/ll:clear)
	(dotimes (row 20)
	  (dotimes (col 20)
	    (charms/ll:mvaddch row col (char-code (aref game-map row col)))))
	(charms/ll:mvaddch p-y p-x (char-code #\@))
	(charms/ll:mvaddstr 22 0 "Welcome to the dungeon")
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
	  
  
  
