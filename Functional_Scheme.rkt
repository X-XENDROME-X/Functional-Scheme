#lang r5rs

; below is part 1
; below is the code which creates a function for each called run1, run2, run3, run4 and run5.

; below defines a procedure named "run1"
(define run1
  (lambda()
    ; Below returns the result of adding 7, 3, and 2
    (+ 7 3 2)))

; below defines a procedure named "run2"
(define run2
  (lambda()
    (+(* 7(+ 12 18))3)))

; below defines a procedure named "run3"
(define run3
  (lambda()
    (-(+(/( * 18 8)6) (* 17(- 18 3)))9)))

; below defines a procedure named "run4"
(define run4
  (lambda()
    (-(/(* 7(- 4 9)) (-(+ 11 11) 15))(+ 6 9))))

; below defines a procedure named "run5"
(define run5
  (lambda()
    (/(*(/ 10 (-(+ 2 16)13)) (*(+ 10 4) (- 17 8)))  (*(/(+(- 14 15)10) (+ 3 9)) (/ 72 9)))))



; Below is the Part 2 which is Visiting Final Fantasy.

; Below is the Part A.

; Below defines a function named ffMQ-Damage
(define ffMQ-Damage
  ; Below the function takes four parameters: attack, defense, weakness, and targets
  (lambda(attack defense weakness targets)
    ; Below returns the result of the calculation, rounded down to the nearest integer
    (floor(/(*(-(* attack 4)defense)weakness)targets))))

; Below is the Part B.
; Below defines a function named "ff10-baseDamage" that takes two parameters: "stat" and "dmgValue".
(define ff10-baseDamage
  (lambda(stat dmgValue)
     ; Below returns the result of the calculation
    (*(floor(+(/(expt stat 3)32)32))(floor(/ dmgValue 16)))))

; Below is the Part C.
; Below defines a function named ff10-defenseNum.
(define ff10-defenseNum
  (lambda(defense)
     ; Below returns the result of the calculation
    (floor(+(/(expt(- defense 280.4)2)110)16))))

; Below is the Part D.
; Below defines a function named ff10-reduceDamage
(define ff10-reduceDamage
  (lambda(baseDmg defenseNum)
     ; Below returns the result of the calculation
    (floor(/(* baseDmg defenseNum) 730))))

; Below is the Part E.
; Below defines a function named ff10-finalDamage
(define ff10-finalDamage
  (lambda(reducedDmg defense)
     ; Below returns the result of the calculation
    (floor(/(* reducedDmg(- 730(floor(/(-(* defense 51)(floor(/(expt defense 2)11)))10))))730))))

; Below is the Part F.
; Below defines a function named ff10-calculate-damage
(define ff10-calculate-damage
  (lambda(stat dmgValue defense)
     ; Below returns the result of the calculation
    (ff10-finalDamage(ff10-reduceDamage(ff10-baseDamage stat dmgValue)(ff10-defenseNum defense))defense)))


; Below is the Part 3 which is some recursive math.

; Below is Part A.
(define factorial
  (lambda(x)
    ; below checks if x is not an integer and returns 'NAN (Not a Number) if x is not an integer
    (if(not(integer? x))
        'NAN
        ; Below checks if x is less than 0 and returns 'NAN (Not a Number) if true
        (if(< x 0)
            'NAN
            ; Below checks if x is equal to 0
            (if(= x 0)
                1 ; This returns 1 if x is 0 (base case for factorial)
                ; Below is the recursive case which multiplies x by factorial of (x - 1)
                (* x(factorial(- x 1))))))))

; Below is Part B.
(define(alt-factorial x)
  (cond
    ; Below checks if x is less than 0
    ((< x 0)
     ; If x is less than 0, returns 'NAN' (Not a Number)
     'NAN)
    ; Below checks if x is equal to 0 or 1
    ((or(= x 0)(= x 1))
     ; If x is 0 or 1, returns 1 (base cases for factorial)
     1)
     ; If x is neither less than 0 nor 0 or 1, calculate the factorial of x using a built-in 'factorial' function and recursively subtract the factorial of (x - 1) from the factorial of x.
    (else(-(factorial x)(alt-factorial(- x 1))))))


; Below is the Part 4 which is List manupulation.

; Below is Part A.
; Below defines a function named get-second-item
(define get-second-item
  ; The function takes one argument, denoted as x
  (lambda(x)
    ; Below returns the second element of the input list x
    (car(cdr x))))

; Below is Part B.
; Below defines a new function called get-third-item
(define get-third-item
    ; The function takes one argument, denoted as x
  (lambda(x)
    ; Below returns the third item in the list represented by x
    (car(cdr(cdr x)))))

; Below is Part C.
(define list-length?
  (lambda(item1)
    ; Below checks if the input list is empty and if it's empty, then it returns 0 (base case)
    (if(null? item1)
        0
        ; Below checks if the first element of the list is itself a list
        (if(list?(car item1))
           ; If it's a list, recursively call list-length? on the rest of the list and add 1 to the result
           (+(list-length?(cdr item1))1)
           ; If it's not a list, recursively call list-length? on the rest of the list and add 1 to the result
           (+ 1(list-length?(cdr item1)))))))

; Below is Part D.
(define arbitrary-cdr
  (lambda(x item1)
    ; Below checks if item1 is null (empty list)
    (if(null? item1)
        #f ; If item1 is null, then it returns #f (false)
        ; If x is negative, then it return #f (false)
        (if(< x 0)
           #f
           ; If x is 1, then it returns item1
           (if(= x 1)
              item1
              ; If x is greater than 1, then we recursively call arbitrary-cdr with x decremented by 1 and item1's cdr (rest of the list).
              (arbitrary-cdr(- x 1)(cdr item1)))))))

; Below is Part E.
; Below defines a function named make-list that takes two arguments x and y.
(define make-list
  (lambda(x y)
    ; Below checks if y is a non-negative number.
       ; If y is 0, returns an empty list.
    (if(number? y)
        ; If y is a positive number (greater than 0), executes the below code. 
       (if(> y 0)
          ; Otherwise, it recursively create a list by cons-ing x and a recursive call with y decremented by 1.
          (cons x(make-list x(- y 1)))
          ; If 'y' is not greater than 0, it returns an empty list.
          '())
       ; If 'y' is not a number, it returns an empty list.
       '())))
               
; Below is the Part 5 which is Multiply Number List.

; Below is Part A.
(define number-list?
  (lambda(item)
    (cond
      ;Below if the list is empty,then it returns #f (not a number list)
      ((null? item)#f)
      ;Below if the first element is not a number, then it returns #f (not a number list)
      ((not(number?(car item)))#f)
      ;Below if the list has only one element, then it returns #t (a number list)
      ((null?(cdr item))#t)
      ;Below recursively checks the rest of the list
      (else(number-list?(cdr item)))))) 
    
         
; Below is Part B.
(define multiply-number-list
  (lambda(item)
    ; Check if 'item' is a valid number list using 'multiply-helper' and if it's a valid list and the result of 'multiply-helper' is 1, and 'item' is not equal to '(1)', return '#f'.
        (if(and(eq?(multiply-helper item)1)(not(equal? item '(1))))
            #f
            ; Otherwise, we return the result of 'multiply-helper'.
           (multiply-helper item)) ))

;We define a helper function 'multiply-helper' that calculates the product of numbers in a valid number list 'item'.
(define multiply-helper
  (lambda(item)
    (cond
      ; If 'item' is a number list, recursively multiply its first element with the result of 'multiply-helper' applied to the rest of the list.
      ((number-list? item)              
        (*(car item)(multiply-helper(cdr item))))
      ; If 'item' is not a number list, return 1.
      (else 1))))
   

; Below is the Part 6 which is Functions as parameters.

; Below is Part A.
; Below defines a function called `which-function` with three arguments.
(define which-function
  (lambda(func1 func2 x)
    ; Below checks if the result of calling func1 with x is greater than the result of calling func2 with x.
      (if(>(func1 x)(func2 x))
         ; If the condition is true, then it returns 1.
          1
          ; If the above condition is false, check if the result of calling func1 with x is less than the result of calling func2 with x.
          (if(<(func1 x)(func2 x))
             ; If the above condition is true, returns 2.
              2
              ; If both conditions above are false, returns 0.
              0))))




            


    
            
                              


  
   


  




