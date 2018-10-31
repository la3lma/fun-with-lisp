;;; 
;;; The 8 queens problem, just as a kata :-)
;;;


(defun all-permutations (list)
  "Generate all permutations of the elements in a list"
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
		 append (mapcar (lambda (l) (cons element l))
				(all-permutations (remove element list)))))))


(defun solutionp (candidate)
  "Looking for queens on diagonals, true iff not found"
  (if (null candidate)
      (return-from solutionp t)
    (let ((h (car candidate))
	  (o 1))
      (dolist (tl (cdr candidate))
	(if (= (abs (- tl h)) o)
	    (return-from solutionp nil))
	(incf o))))
  (solutionp (cdr candidate)))


(defun 8q ()
  "Naive implementation, not removing any symmetries, finding all 92 possible solutions."
  (remove-if-not #'solutionp (all-permutations (list 0 1 2 3 4 5 6 7))))

(defun uq8 ()
  "Find the 12 essential solutions to the eight queen problem"
  (symmetry-zapper (8q)))

(defun remove-symmetries (symmetry-generators candidates)
  "Very bogus function, does not work yet"
  (let ((result '()))
    (dolist (candidate candidates)
      (block candidate-loop
	(dolist (symmetry-generator symmetry-generators)
	  (let ((symmetry (apply symmetry-generator (list candidate))))
	    (when (member symmetry result :test #'equal)
	      (return-from candidate-loop))))
	(setq result (cons candidate result))))
    result))


(defun symmetry-zapper (candidates)
  (remove-symmetries
   (list
    #'rotation90 #'rotation180 #'rotation270
    #'mirror-horizontal #'mirror-vertical
    #'mirror-diagonal1 #'mirror-diagonal2)
   candidates))

;; The three essential symmetries

(defun symmetry-builder (candidate index-calculator)
  (let ((v (make-array 8)))
    (dotimes (x 8)
      (let ((y (nth x candidate)))
	(setf (aref v (apply index-calculator (list y))) x)))
    (list (aref v 0)
	  (aref v 1)
	  (aref v 2)
	  (aref v 3)
	  (aref v 4)
	  (aref v 5)
	  (aref v 6)
	  (aref v 7))))

(defun rotation90 (candidate)
  (symmetry-builder candidate #'(lambda (y) (- 7 y))))

(defun mirror-diagonal1 (candidate)
  (symmetry-builder candidate #'(lambda (y) y)))

(defun mirror-horizontal (candidate)
  (reverse candidate))

;; The remaining symmetries are constructed by composing
;; the essential symmetries in various ways

(defun rotation180 (candidate)
  (rotation90 (rotation90 candidate)))

(defun rotation270 (candidate)
  (rotation180 (rotation90 candidate)))


(defun mirror-diagonal2 (candidate)
 (rotation90 (mirror-diagonal1 (rotation-90 candidate))))

(defun rotation-90 (candidate)
  (rotation270 candidate))

(defun mirror-vertical (candidate)
  (rotation90 (mirror-horizontal (rotation-90 candidate))))

