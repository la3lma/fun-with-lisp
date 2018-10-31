;;;
;;; LISP
;;;
;;; "einsteins riddle"

(defun all-permutations (list)
  "Generate all permutations of the elements in a list"
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
		 append (mapcar (lambda (l) (cons element l))
				(all-permutations (remove element list)))))))

;;
;; The parameters being used in the constraints
;;
(defparameter *colours* '(yellow    green red    blue   gray))
(defparameter *animals* '(bird      cat   fish   dog    horse))
(defparameter *cars*    '(opel      lada  volvo  toyota ford))
(defparameter *drink*   '(water     milk  soda   tea    coffee))
(defparameter *nation*  '(norwegian swede german danish english))



(defun checker (&key (out *standard-output*))
  "The checker that will find all the permutations of the parameters
   satisfy the constraints, using early cutoffs"
  (let ((all-colors
	 (remove-if-not #'c4 (all-permutations *colours*)))
	(all-animals (all-permutations *animals*))
	(all-cars    (all-permutations *cars*))
	(all-drinks
	 (remove-if-not #'c8 (all-permutations *drink*)))
	(all-nations
	 (remove-if-not #'c9 (all-permutations *nation*)))

	(color-count 0)
	(drink-count 0)
	(nation-count 0)
	(animal-count 0)
	(car-count 0))


    (format out "~%===~%    Solutions~2%")

    (dolist (co all-colors)
      (incf color-count)
      (dolist (dr all-drinks)
	(incf drink-count)
	(if  (c5 co dr)

	    (dolist (na all-nations)
	      (incf nation-count)
	      (if (and
		   (c1 na co)
		   (c14 na co)
		   (c3 na dr))
		  (dolist (an all-animals)
		    (incf animal-count)
		    (if (c2 na an)
			(dolist (ca all-cars)
			  (incf car-count)
			  (if (and
			       (c6 ca an)
			       (c7 ca co)
			       (c10 ca an)
			       (c11 ca an)
			       (c12 ca dr)
			       (c13 ca na)
			       (c15 ca dr))
			      (if (is-match-p co an ca dr na)
				  (format out "~%----~{~%~{~:(~10s~)~}~}"
					  (list co an ca dr na))))))))))))

    (format out "~%===~%   Number of altenatives tried")
    (format out "~2%    color-count = ~d" color-count)
    (format out "~%    drink-count = ~d" drink-count)
    (format out "~%   nation-count = ~d" nation-count)
    (format out "~%   animal-count = ~d" animal-count)
    (format out "~%      car-count = ~d" car-count)))


;;
;; Helper functions for expressing spatial relationships
;;
(defun at-same-position-p (key1 set1 key2 set2)
  (= (position key1 set1)
     (position key2 set2)))

(defun are-neigbours-p (key1 set1 key2 set2)
  (= 1 (abs (- (position key1 set1) (position key2 set2)))))

(defun immediately-to-the-right-of-p (key1 set1 key2 set2)
  (= (- (position key1 set1) (position key2 set2)) 1))

(defun at-location-p (index item set)
  (eq item (nth index set)))

;;
;;  The clauses from the problem
;;
(defun c1 (na co)
  "The englishman lives in the red house"
  (at-same-position-p 'english na 'red co))

(defun c2 (na an)
  "The swede has a dog as a pet"
  (at-same-position-p 'swede na 'dog an))

(defun c3 (na dr)
  "The dane drinks tea"
  (at-same-position-p 'danish na'tea dr))

(defun c4 (co)
  "The green house is the right door neigbour of the gray house"
  (immediately-to-the-right-of-p 'gray co 'green co))

(defun c5 (co dr)
  "The owner of the green house drinks coffee"
  (at-same-position-p 'coffee dr 'green co))

(defun c6 (ca an)
  "The owner of the opel has a bird as pet"
  (at-same-position-p  'opel ca 'bird an))

(defun c7 (ca co)
  "The owner of the yellow house drives Toyota"
  (at-same-position-p 'toyota ca 'yellow co))

(defun c8 (dr)
  "The man at the hose in the middle drinks milk"
  (at-location-p 2 'milk dr))

(defun c9 (na)
  "The norwegian lives in house #1 (base zero :-))"
  (at-location-p 0 'norwegian na))

(defun c10 (ca an)
  "The man driving a ford lives besides the man owning a cat"
  (are-neigbours-p 'ford ca 'cat an))

(defun c11 (ca an)
  "The man owning a horse lives besides the man driving a toyota"
  (are-neigbours-p 'toyota ca 'horse an))

(defun c12 (ca dr)
  "The downer of the lada drinks soda"
  (at-same-position-p 'lada ca 'soda dr))

(defun c13 (ca na)
  "The german drives volvo"
  (at-same-position-p 'german na 'volvo ca))

(defun c14 (na co)
  "The norwegian lives besides the blue house"
  (are-neigbours-p 'blue co 'norwegian na))

(defun c15 (ca dr)
  "The man driving the ford has a neigbour drinking water"
  (are-neigbours-p 'ford ca 'water dr))

(defun is-match-p (co an ca dr na)
  (and
   (c4 co)
   (c8 dr)
   (c9 na)
   (c1 na co)
   (c14 na co)
   (c3 na dr)
   (c5 co dr)
   (c2 na an)
   (c6 ca an)
   (c7 ca co)
   (c10 ca an)
   (c11 ca an)
   (c12 ca dr)
   (c13 ca na)
   (c15 ca dr)))
