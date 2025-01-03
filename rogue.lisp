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
     (lost-items :accessor lost-items :initform nil)
     (discovered :initarg discovered :accessor discovered)))
     
(defclass game-state ()
  ((gmap :initarg :gmap :accessor gmap)
   (msg :initarg :msg :accessor msg :initform "Welcome to the dungeon")
   (px :initarg :px :accessor px :initform 5)
   (py :initarg :py :accessor py :initform 5)
   (cx :initarg :cx :accessor cx :initform 5)
   (cy :initarg :cy :accessor cy :initform 5)
   (phase :accessor game-phase :initform :playing)
   (inventory :accessor inventory :initform nil)
   (stair-list :initarg stair-list :accessor stair-list :initform nil)
   (map-hash :initarg :map-hash :accessor map-hash :initform nil)))

(defstruct stair-pair
  (start 0)
  (sx 0)
  (sy 0)
  (end 0)
  (ex 0)
  (ey 0))

(defstruct game-item
  (x 0)
  (y 0)
  (name "Lost socks")
  (quantity 1)
  (value 0))
        
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
	  
	  (when (equal (game-phase gs) :exploring)
	    (charms/ll:mvaddch (- (cy gs) sy) (- (cx gs) sx) (char-code #\x)))
	  
	  (charms/ll:mvaddstr (+ *screen-height* 1) 0 (msg gs))
	  (charms/ll:mvaddstr (+ *screen-height* 2) 0 (format nil "(~a, ~a)" (px gs) (py gs))))

	(charms/ll:refresh)
	(let ((ch (charms/ll:getch))
	      (phase (game-phase gs)))
	  (case phase
	    (:playing
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
	       (#\. (look-at gs (px gs) (py gs)))
	       (#\, (take-at gs (px gs) (py gs)))
	       (#\x (toggle-xplore gs))
	       (#\q (return))
	       (otherwise nil)))
	    (:exploring
	     (case (code-char ch)
	       (#\h (move-cursor gs -1  0))
	       (#\j (move-cursor gs  0  1))
	       (#\k (move-cursor gs  0 -1))
	       (#\l (move-cursor gs  1  0))
	       (#\y (move-cursor gs -1 -1))
	       (#\u (move-cursor gs  1 -1))
	       (#\b (move-cursor gs -1  1))
	       (#\n (move-cursor gs  1  1))
	       (#\q (toggle-xplore gs))))))))))

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
  (let ((gs (make-instance 'game-state :map-hash (make-hash-table)))
	(stair-list nil))
    (push (make-stair-pair :start 1 :sx 22 :sy 10
			   :end   2 :ex 5  :ey 5)
	  stair-list)
  (setf (stair-list gs) stair-list)
    (select-map gs 1)
    (push (make-game-item :x 8 :y 10) (lost-items (gmap gs)))
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

(defun toggle-xplore (gs)
  (if (eq (game-phase gs) :playing)
      (progn
	(setf (game-phase gs) :exploring)
	(setf (cx gs) (px gs))
	(setf (cy gs) (py gs))
	(setf (msg gs) "You can look around now"))
      (setf (game-phase gs) :playing)))
	    	    
(defun tile-at (gs x y)
  (aref (gmap (gmap gs)) y x))

(defun visible-tile-at (gs x y)
  (if (aref (discovered (gmap gs)) y x)
      (cond ((ascending-stair-at gs x y) #\<)
	    ((descending-stair-at gs x y) #\>)
	    ((item-at gs x y) #\!)
	    (t (tile-at gs x y)))
      #\ ))

;; TODO find a better way of finding stairs, curently we iterate all stairs on all visible coordinates every frame
(defun ascending-stair-at (gs x y)
  (find-if (lambda (s)
	     (and (= (stair-pair-end s) (level (gmap gs)))
		  (= (stair-pair-ex s) x)
		  (= (stair-pair-ey s) y)))
	   (stair-list gs)))

(defun descending-stair-at (gs x y)
  (find-if (lambda (s)
	     (and (= (stair-pair-start s) (level (gmap gs)))
		  (= (stair-pair-sx s) x)
		  (= (stair-pair-sy s) y)))
	   (stair-list gs)))

(defun item-at (gs x y)
  (find-if (lambda (i)
	     (and (= (game-item-x i) x)
		  (= (game-item-y i) y)))
	   (lost-items (gmap gs))))

(defun look-at (gs x y)
  (let ((item (item-at gs x y)))
    (if item
	(setf (msg gs)
	      (format nil "You see \"~a\" (x~a)"
		      (game-item-name item)
		      (game-item-quantity item)))
	(setf (msg gs)
	      (case (visible-tile-at gs x y)
		(#\> "You see a stair going down")
		(#\< "You see a stair going up")
		(#\# "Wall")
		(#\  "Darkness")
		(otherwise "Nothing here"))))))
		 
(defun take-at (gs x y)
  (let ((item (item-at gs x y)))
    (if item
	(progn
	  (add-to-inventory gs item)
	  (setf (msg gs)
		(format nil "You take \"~a\" (x~a)"
			(game-item-name item)
			(game-item-quantity item)))
	  (remove-item gs item))
	(setf (msg gs) "Nothing to take here"))))

(defun add-to-inventory (gs item)
  (push item (inventory gs)))

(defun remove-item (gs item)
  (setf (lost-items (gmap gs))
	(remove item (lost-items (gmap gs)))))
  	
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

(defun move-cursor (gs dx dy)
  (let ((x (+ (cx gs) dx))
	(y (+ (cy gs) dy)))
    (look-at gs x y)
    (setf (cx gs) x)
    (setf (cy gs) y)))
      
(defun attempt-descend-stairs (gs)
  (let ((stair (descending-stair-at gs (px gs) (py gs))))
    (if stair
        (progn
	  (setf (px gs) (stair-pair-ex stair))
	  (setf (py gs) (stair-pair-ey stair))
	  (select-map gs (stair-pair-end stair))
	  (setf (msg gs) "You descend deeper!"))
	(setf (msg gs) "You see no stair going down."))))

(defun attempt-ascend-stairs (gs)
    (let ((stair (ascending-stair-at gs (px gs) (py gs))))
    (if stair
        (progn
	  (setf (px gs) (stair-pair-sx stair))
	  (setf (py gs) (stair-pair-sy stair))
	  (select-map gs (stair-pair-start stair))
	  (setf (msg gs) "You ascend a level!"))
	(setf (msg gs) "You see no stair going up."))))

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
      

		       
      
