(ql:quickload :cl-charms)

(defpackage :rogue
  (:use :cl)
  (:export :main))

(in-package :rogue)

(defparameter *screen-height* 24)
(defparameter *view-range* 2)

(defclass game-map ()
    ((gmap :initarg :gmap :accessor gmap)
     (level :initarg :level :accessor level :initform 1)
     (width :initarg :width :accessor width)
     (height :initarg :height :accessor height)
     (discovered :initarg discovered :accessor discovered)))
     
(defclass game-state ()
  ((gmap :initarg :gmap :accessor gmap)
   (msg :initarg :msg :accessor msg :initform "Welcome to the dungeon")
   (px :initarg :px :accessor px :initform 5)
   (py :initarg :py :accessor py :initform 5)
   (map-hash :initarg :map-hash :accessor map-hash :initform nil)))

(defmethod select-map ((gs game-state) level)
  (cond ((gethash level (map-hash gs))
	 (setf (gmap gs) (gethash level (map-hash gs))))
	(t
	 (let ((gmap (init-map level)))
	   (setf (gethash level (map-hash gs)) gmap)
	   (setf (gmap gs) gmap))))
  (discover gs))
  
(defun main ()
  (let ((gs (init-game-state)))
    (charms:with-curses ()
      (charms/ll:start-color)
      (charms/ll:init-pair 1 charms/ll:COLOR_WHITE charms/ll:COLOR_BLACK)
      (charms/ll:init-pair 2 charms/ll:COLOR_YELLOW charms/ll:COLOR_BLACK)
      
      (loop
	(charms/ll:clear)

	(multiple-value-bind (sx sy ex ey) (get-viewport gs)
	  (loop for x from sx to ex do
	    (loop for y from sy to ey do
	      (if (is-visible gs x y)
		(setf attr (charms/ll:color-pair 1))
		(setf attr (charms/ll:color-pair 2)))
	      (charms/ll:attron attr)
	      (charms/ll:mvaddch (- y sy) (- x sx) (char-code (visible-tile-at gs x y)))))
	
	  (setf attr (charms/ll:color-pair 1))
	  (charms/ll:attron attr)
	  (charms/ll:mvaddch (- (py gs) sy) (- (px gs) sx) (char-code #\@))
	  (charms/ll:mvaddstr (+ *screen-height* 1) 0 (msg gs))
	  (charms/ll:mvaddstr (+ *screen-height* 2) 0 (format nil "(~a, ~a)" (px gs) (py gs))))

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
	    (#\> (attempt-descend-stairs gs))
	    (#\< (attempt-ascend-stairs gs))
	    (#\q (return))
	    (otherwise nil)))))))

(defun init-map (level)
  (let ((gm (make-instance 'game-map
			   :level level
			   :gmap (load-map level))))
    (setf (width gm)
	  (array-dimension (gmap gm) 1))
    (setf (height gm)
	  (array-dimension (gmap gm) 0))
    (setf (discovered gm)
	  (make-array (list (height gm) (width gm)) :initial-element nil))
    gm))

(defun init-game-state ()
  (let ((gs (make-instance 'game-state :map-hash (make-hash-table))))
    (select-map gs 1)
    gs))

(defun get-viewport (gs)
  (let* ((ay (/ *screen-height* 2))
	 (by *screen-height*)
	 (ax 18)
	 (bx (* ax 2))
	 (px (px gs))
	 (py (py gs))
	 (w  (- (width (gmap gs)) 1))
	 (h  (- (height (gmap gs)) 1))
	 (sx (max (- px ax) 0))
	 (sy (max (- py ay) 0))
	 (ex (min (+ px ax) w))
	 (ey (min (+ py ay) h))
	 (dx (- bx (- ex sx)))
	 (dy (- by (- ey sy))))
    (when (> dx 0)
      (if (= sx 0)
	  (incf ex dx)
	  (decf sx dx)))
    (when (> dy 0)
      (if (= sy 0)
	  (incf ey dy)
	  (decf sy dy)))
    (values sx sy ex ey)))
	    	    
(defun tile-at (gs x y)
  (aref (gmap (gmap gs)) y x))

(defun visible-tile-at (gs x y)
  (if (aref (discovered (gmap gs)) y x)
      (tile-at gs x y)
      #\ ))

(defun discover (gs)
  (let ((x (px gs))
	(y (py gs))
	(w (width (gmap gs)))
	(h (height (gmap gs))))
    (let ((lx (max (- x *view-range*)  0)) ;; low x
	  (hx (min (+ x *view-range*)  (- w 1))) ;; high x
	  (ly (max (- y *view-range*)  0)) ;; low y
	  (hy (min (+ y *view-range*)  (- h 1)))) ;; high y
      (loop for tx from lx to hx do
	(loop for ty from ly to hy do
	  (if (has-direct-path gs x y tx ty)
	      (setf (aref (discovered (gmap gs)) ty tx) t)))))))

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

(defun is-visible (gs tx ty) ;; in view range of player
  (let ((dx (abs (- (px gs) tx)))
	(dy (abs (- (py gs) ty))))
    (and (<= (max dx dy) *view-range*)
	 (has-direct-path gs (px gs) (py gs) tx ty))))
	   
(defun attempt-move (gs dx dy)
  (let ((x (+ (px gs) dx))
	(y (+ (py gs) dy)))
    (case (tile-at gs x y)
      (#\# (setf (msg gs) "There is a wall there!"))
      (otherwise (progn
		   (setf (msg gs) "")
		   (setf (px gs) x)
		   (setf (py gs) y)
		   (discover gs))))))

(defun attempt-descend-stairs (gs)
  (if (eq (tile-at gs (px gs) (py gs)) #\>)
      (progn
	(select-map gs (+ (level (gmap gs)) 1))
	(setf (msg gs) "You descend deeper!"))
      (setf (msg gs) "You see no stair going down.")))

(defun attempt-ascend-stairs (gs)
  (if (eq (tile-at gs (px gs) (py gs)) #\<)
      (progn 
	(select-map gs (- (level (gmap gs)) 1))
	(setf (msg gs) "You ascend a level!!"))
      (setf (msg gs) "You see no stair going up.")))
    
(defun load-map (level)
  (with-open-file (stream (format nil "map~a.txt" level) :direction :input)
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
      

		       
      
