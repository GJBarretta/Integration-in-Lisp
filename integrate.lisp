;;CSC 345 Project 1: Symbolic Integration in Lisp
;;Giuseppe Barretta

;;==========================================================================
;;Main Integration Functions                                             
;;--------------------------------------------------------------------------
;;The main functions. Integration can either be definite which evaluates 
;;to a numerical value, or indefinite which is returned as an equation.  
;;Either way, we will always want the equation because we need it in order
;;to evaluate the numerical value of the definite integral. Therefore, we
;;first derive the equation through the indef-integral function. Once that
;;is done, we pass the equation into the def-integral function with hi lo
;;arguments which may or may not exist. If the hi lo arguments exist, then
;;we calculate the numerical value of the definite integral through the use
;;of the equation. If no hi lo arguments exist, then we return the equation.     
;;--------------------------------------------------------------------------
(defun integrate (F V &optional lo hi)
  (def-integral (indef-integral F (make-variable-name V)) (make-variable-name V) lo hi))

;;indef-integral: for the construction of the equation
(defun indef-integral (F V)
        ;if F is a variable by itself then simply return (1/2)*(F^2)
  (cond ((or (numberp F)(and (listp F)(numberp (first F))))(make-product F V))
	((or (variable-p F)(and (listp F)(variable-p (first F))))(make-product 1/2 (make-power F 2)))
	;if F is a number then simply return the  product of F and V
	;((numberp F)(make-product F V))
	;if sum-operator exists and the sum operands are equal then take one operand and multiply by 2 (for aesthetics)
	((and (sum-p F)(equal (sum-operand-1 F)(sum-operand-2 F)))
	 (make-product 2 (indef-integral (sum-operand-1 F) V)))
	;if sum-operator exists and sum operands are not equal then sum the arguments
	((sum-p F)(make-sum(indef-integral(sum-operand-1 F) V)
			   (indef-integral(sum-operand-2 F) V)))
	;if difference-operator exists then subtract the arguments
	((difference-p F)(make-difference(indef-integral(difference-operand-1 F) V)
					 (indef-integral(difference-operand-2 F) V)))
	;((product-p F): beyond the scope of the project
	;((quotient-p F): beyond the scope of the project
        ;if negative-operator exists and it was not a difference-operator then make negative.
        ;if F is truly negative (with odd number of negatives) then it makes F positive and
        ;and cons a negative-symbol to the equation (taking the negative symbol out front).
        ;if F is ultimately positive (with even number of negatives) then it takes all negative 
        ;symbols away, and does not cons a negative-symbol onto the equation.
	((negative-p F)(reduce-negative (indef-integral(reduce-negative F nil 0) V) nil 0))
        ;if power-operator exists, then take the power of F
	;power check should only proceed if the exponent is 0 or greater. else it goes to log
	((and (power-p F)(not (equal (power-operand-2 F) -1)))
	 ;convert number to string for concatenation with 1/ then convert back. 
         (make-product (read-from-string (concatenate 'string "1/" (write-to-string (abs (1+ (power-operand-2 F))))))
		       (make-power (power-operand-1 F)(1+ (power-operand-2 F)))))
	;logs can be devised either through the log operator or the power operator so check both
	((or (logarithm-p F)(and (power-p F)(= (power-operand-2 F) -1)))
	 (make-logarithm (power-operand-1 F)(power-operand-2 F)))))

;;def-integral: for the substitution of lo hi arguments and evaluation of the numerical value
(defun def-integral (F V &optional lo hi)
  (if (and (not hi)(not lo))
      ;if no hi lo arguments are supplied then return the equation
      F
      ;else substitute the variable with the hi lo arguments and evaluate the numerical value
      (labels ((sub (e1 e2 L)
	     (cond ((variable-p L) e2)
		   ((endp L) nil)
		   ((equal e1 (first L)) (cons e2 (sub e1 e2 (rest L))))
		   ((listp (first L)) (cons (sub e1 e2 (first L)) (sub e1 e2 (rest L))))
		   (t (cons (first L) (sub e1 e2 (rest L)))))))
	(eval (make-difference (sub V hi F)(sub V lo F))))))
  
;;==========================================================================
;; SYMBOLS
;;--------------------------------------------------------------------------
;;These are the symbols that indicate what a given element of the inputted
;;list is intended to do. If the quotient-symbol is the first symbol of the
;;element, then the intention is that the two arguments that follow are to
;;be divided. The predicate functions use these defined symbols in order to
;;determine where an element should go in the program to be transformed.
;;These particular symbols were chosen in order to be in accordance with the
;;symbols of lisp so that the def-integral operations can proceed unhindered.
;;--------------------------------------------------------------------------
(defconstant variable-symbols       '(U V W X Y Z))
(defconstant sum-symbol             '+)
(defconstant quotient-symbol        '/)
(defconstant power-symbol           'expt)
(defconstant difference-symbol      '-)
(defconstant negative-symbol        '-)
(defconstant logarithm-symbol       'log)
(defconstant product-symbol         '*)

;;==========================================================================
;; OPERATORS
;;--------------------------------------------------------------------------
;;Returns the first element of the list which, in prefix form,  will
;;always be the operator of the element. This will be checked against
;;the defined symbols above in the predicate functions. Although it
;;may seem redundant to have unique functions for each potential
;;operator when they all perform the same task, this is done for the
;;proper abstraction and clarity of the code.
;;--------------------------------------------------------------------------
(defun sum-operator        (F)(first F))
(defun product-operator    (F)(first F))
(defun quotient-operator   (F)(first F))
(defun negative-operator   (F)(first F))
(defun power-operator      (F)(first F))
(defun logarithm-operator  (F)(first F))
(defun difference-operator (F)(first F))

;;==========================================================================
;; OPERANDS
;;--------------------------------------------------------------------------
;;Each operation will come with two operands (except negation) which
;;will be the second and third arguments of the element. These are 
;;the arguments for the particular operator of an element. A division
;;operator requires 2 operands for example, and that is the purpose
;;of these functions - to supply the operands of a given element.
;;--------------------------------------------------------------------------
(defun negative-operand-1   (F)(second F))
(defun sum-operand-1        (F)(second F))
(defun sum-operand-2        (F)(third F))
(defun difference-operand-1 (F)(second F))
(defun difference-operand-2 (F)(third F))
(defun power-operand-1      (F)(second F))
(defun power-operand-2      (F)(third F))
(defun logarithm-operand-1  (F)(second F))
(defun logarithm-operand-2  (F)(third F))
(defun product-operand-1    (F)(second F))
(defun product-operand-2    (F)(third F))
(defun quotient-operand-1   (F)(second F))
(defun quotient-operand-2   (F)(third F))

;;==========================================================================
;; PREDICATES
;;--------------------------------------------------------------------------
;;These are the functions that determine what a particular element of
;;the inputted list is intended to be. It does this by comparing the
;;first element of that list to the symbols defined above. 
;;--------------------------------------------------------------------------
(defun variable-p (F) 
  (member F variable-symbols))
(defun number-p (F G)
  (and (numberp F)(numberp G)))
(defun sum-p (F)
  (eq (sum-operator F) sum-symbol))
(defun product-p (F)
  (eq (product-operator F) product-symbol))
(defun quotient-p (F)
  (eq (quotient-operator F) quotient-symbol))
(defun power-p (F)
  (eq (power-operator F) power-symbol))
(defun logarithm-p (F)
  (eq (logarithm-operator F) logarithm-symbol))  
(defun difference-p (F)
        ;there must be 2 operands for subtraction, else it is negative so return nil
  (cond ((not (= (length F) 3)) nil)
	((eq (difference-operand-1 F) difference-symbol) nil)
	(t (eq (difference-operator F) difference-symbol))))
(defun negative-p (F)
        ;if the argument is a number greater than 0 then return nil
  (cond ((and (numberp F)(>= F 0)) nil)
	;if the argument is a number less than 0 then return true
	((and (numberp F)(< F 0)) T)
	;if the argument is not difference and it is negative, return true
        (t (and (not (difference-p F)) (eq (negative-operator F) negative-symbol)))))
  
;;==========================================================================
;; CONSTRUCTORS
;;--------------------------------------------------------------------------
;;Constructs the equation, or computes the numerical result
;;if both arguments supplied are numbers. There are certain
;;edge cases that must be explicitly tested for in each case.
;;--------------------------------------------------------------------------
(defun make-variable-name (V) V)

(defun make-sum (F G)
        ;if both arguments are numbers then perform the calculation
  (cond ((number-p F G)(+ F G))
	;if either argument is 0, return the other argument
	((equal F 0) G)
	((equal G 0) F)
	;if the negation of the argument equals the other, then F+G=0, so return 0
	((equal G (reduce-negative F nil 0)) 0)
	;else return the equation
	(t (list sum-symbol F G))))

(defun make-difference (F G)
        ;if both arguments are numbers then perform the calculation
  (cond ((number-p F G)(- F G))
	;if F is 0 then G is subtracted from 0, so make it negative
	((equal F 0)(reduce-negative G nil 0))
	;if G is 0 then F-0=F so return F
	((equal G 0) F)
	;if F and G are equal then F-G=0 so return 0
	((equal F G) 0)
	;else return the equation
	(t (list difference-symbol F G))))

;recursively destroy F and build P. When the recursion has finished P should be the initial
;F, and it is used for the return values. Acc is used to check whether F was truly negative
;(odd number of negative-symbols) or ultimately positive (even number of negative-symbols). 
(defun reduce-negative (F P Acc)
        ;if F is a number then return F multiplied by -1
  (cond ((numberp F) (* F -1))
	;if F is a variable by itself, then negate it
	((variable-p F)(list negative-symbol F))
	;end of destruction of F case 1: even number of negative-symbols
	((and (endp F)(equal (first P) negative-symbol)(= (mod Acc 2) 0)) (cons negative-symbol (last P)))
        ;end of dustuction of F case 2: odd number of negative-symbols - make positive
	((and (endp F)(equal (first P) negative-symbol)(= (mod Acc 2) 1)) (first (last P)))
	;recursive call for case that first element is the negative-symbol - add 1 to Acc
	((and (not (endp F))(equal (first F) negative-symbol)) (reduce-negative (rest F)(append P (list (first F)))(1+ Acc)))
	;recursive call for case that first element is not the negative-symbol - do not add 1 to Acc
	((and (not (endp F))(not (equal (first F) negative-symbol))) (reduce-negative (rest F)(append P (list (first F))) Acc))
        ;else return the equation
	(t (list negative-symbol P))))

(defun make-product (F G)
        ;if both arguments are numbers then perform the calculation
  (cond ((number-p F G)(* F G))
	;if either argument is 0 then always return 0
	((or (equal F 0)(equal G 0)) 0)
	;if either argument is 1 then return the other argument
	((equal F 1) G)((equal G 1) F)
	;else return the equation
	(t (list product-symbol F G))))

(defun make-quotient (F G)
  (pprint F)
  (pprint G)
  (pprint "make-quotient")
        ;first check if dividing by zero; division by 0 returns nil
  (cond ((equal G 0) nil)
	;if both arguments are numbers then perform the calculation
	((number-p F G)(/ F G))
	((and (negative-p F)(not (negative-p G)))(reduce-negative (make-quotient (first (last F)) G) nil 0))
	((and (negative-p F)(negative-p G)) (make-quotient (reduce-negative F nil 0)(reduce-negative G nil 0)))
	;else return the equation
	(t (list quotient-symbol F G))))

(defun make-power (B E)
        ;if both arguments are numbers then perform the calculation
  (cond ((< E 0) (reduce-negative (make-quotient 1 (list power-symbol B (abs E))) nil 0))
        ((number-p B E)(expt B E))
	;if the base is a variable and the exponent is 1 then return base
	((equal E 1) B)
	;if the base is a variable and the exponent is 0 then return 1
	((equal E 0) 1)
	;else form the power equation and return it
	(t (list power-symbol B E))))

(defun make-logarithm (B &optional E)
        ;if both arguments are numbers then perform the calculation
  (cond ((number-p B E)(log B))
	;if base and exponent are the same then return 1
	((equal B E) 1)
	;if the base is 1 then return 0
	((equal B 1) 0)
	;else return the equation
	(t (list logarithm-symbol B E))))



;;Test Functions
(defun t1() (integrate '1 'x))
(defun t2() (integrate '1 'y 1 4))
(defun t3() (integrate 'z 'z)) 
(defun t4() (integrate '(+ x 0) 'x))
(defun t5() (integrate '(- x) 'x 1 3))
(defun t6() (integrate '(- - x) 'x 1 4))
(defun t7() (integrate '(- x) 'x))
(defun t8() (integrate '(- - x) 'x))
(defun t9() (integrate '(- - - x) 'x))
(defun t10() (integrate '(+ x (- x)) 'x))
(defun t11() (integrate '(- (+ (- - x) x)) 'x 1 4))
(defun t12() (integrate '(+ (+ (- - x) (+ x 3)) 2) 'x 2 6))
(defun t13() (integrate '(- x (expt x 3)) 'x))
(defun t14() (integrate '(- x (expt x 3)) 'x 2 5))
(defun t15() (integrate '(+ (+ x (- - - x)) (expt x 3)) 'x))
(defun t16() (integrate '(+ (- x (- x)) (expt x 3)) 'x 2 3))
(defun t17() (integrate '(expt x -1) 'x))
(defun t18() (integrate '(expt x -1) 'x 3 45))
(defun t19() (integrate '(+ (+ x (- 5 x)) (expt x -1)) 'x))
(defun t20() (integrate '(+ (+ x (- 5 x)) (expt x -1)) 'x 5 217))

;;Testing Function
(defun integral-test ()
  (print "(integrate '1 'x)") (print (t1))
  (print "(integrate '1 'y 1 4)") (print (t2))
  (print "(integrate 'z 'z)") (print (t3))
  (print "(integrate '(+ x 0) 'x") (print (t4))
  (print "(integrate '(- x) 'x 1 3)") (print (t5))
  (print "(integrate '(- - x) 'x 1 4)") (print (t6))
  (print "(integrate '(- x) 'x)") (print (t7))
  (print "(integrate '(- - x) 'x)") (print (t8))
  (print "(integrate '(- - - x)") (print (t9))
  (print "(integrate '(+ x (- x)) 'x)") (print (t10))
  (print "(integrate '(- (+ (- - x) x)) 'x 1 4)") (print (t11))
  (print "(integrate '(+ (+ (- - x) (+ x 3)) 2) 'x 2 6)") (print (t12))
  (print "(integrate '(- x (expt x 3)) 'x)") (print (t13))
  (print "(integrate '(- x (expt x 3)) 'x 2 5)") (print (t14))
  (print "(integrate '(+ (+ x (- - - x)) (expt x 3)) 'x)") (print (t15))
  (print "(integrate '(+ (- x (- x)) (expt x 3)) 'x 2 3)") (print (t16))
  (print "(integrate '(expt x -1) 'x)") (print (t17))
  (print "(integrate '(expt x -1) 'x 3 45)") (print (t18))
  (print "(integrate '(+ (+ x (- 5 x)) (expt x -1)) 'x)") (print (t19))
  (print "(integrate '(+ (+ x (- 5 x)) (expt x -1)) 'x 5 217)") (t20))

