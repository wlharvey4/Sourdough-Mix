;;; sourdough-mix --- Work with sourdough mixes
;;; 2020-08-05 09:15

;;; Commentary:


(defun mix (weight hydration)
  "A sourdough mixture is a combination of a total weight flour
and water and a hydration percentage (e.g., 65%).

The hydration percentage is the amount of water compared to the
amount of flour. Weights can be in ounces or grams, but all
weights should be consistent. For example, a hydration percentage
of 100% would mean an equal amount by weight of both flour and
water. A hydration level of 50% would mean twice as much flour by
weight as water. The components are stored as floating point
numbers, but they can be given as integers if they are whole."

  (cons (float weight) (float hydration)))

(defun weight (mix)
  "Return the weight component of a mix."

  (car mix))

(defun hydration (mix)
  "Return the hydration percentage component of a mix."

  (cdr mix))

(defun make-mix (flour water)
  "Given some flour and water by weight, produce a mix."

  (let ((f (float flour))
	(w (float water)))
    (mix
     (+ f w)
     (* (/ w f) 100.0))))

(defun mix-components (mix)
  "Give the separate components of a mix."

  (list
   (cons "flour" (flour mix))
   (cons "water" (water mix))))

(defun hydration-per (mix)
  "Return the hydration level as a decimal percentage."

  (/ (hydration mix) 100.0))

(defun flour (mix)
  "This operator extracts the amount of flour in a mix."

  (let ((w (weight mix))
	(h (hydration-per mix)))
    (/ w (1+ h))))

(defun water (mix)
  "This operator extracts the amount of water in a mix."

  (let ((w (weight mix))
	(h (hydration-per mix)))
    (* h (flour mix))))

(defun add-mixes (mix1 mix2)
  "Add two mixes together to produce a third mix."

  (let ((f1 (flour mix1))
	(w1 (water mix1))
	(f2 (flour mix2))
	(w2 (water mix2)))
    (make-mix
     (+ f1 f2)
     (+ w1 w2))))

(defun produce-mix (mix starter)
  "Given a starter mix, calculate what should be added to produce
a desired mix."

  (let ((f (flour mix))
	(w (water mix))
	(sf (flour starter))
	(sw (water starter)))
    (make-mix
     (- f sf)
     (- w sw))))

;;; end sourdough-mix
