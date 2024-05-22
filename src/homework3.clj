(ns homework3)

;; Homework Assignment: Custom Evaluation Function
;; Objective: Create a function named `custom-eval` that evaluates expressions based on a given environment.
;; The function should handle basic arithmetic, function declarations, and function calls.

;; Part 1: Setup the Environment
;; Environment managed as a map where keys are symbols (variable/function names) and values are their corresponding values or definitions.
(def initial-env {:x 10 :y 20})

;; Part 2: Evaluating Expressions
;; Implementing the custom-eval function to evaluate basic arithmetic operations using the environment.

(defn custom-eval [expr env]
  (cond
    (symbol? expr)
      		 (if (get env (keyword expr)) (get env (keyword expr))
         		(throw (Exception. "Undefined variable or function")))
    (list? expr)
   	    (cond
          (= (first expr) 'defn) ;function definition
			(let [funcname (nth expr 1)
					args (nth expr 2)
					body (nth expr 3)]
				(assoc env (keyword funcname)      					 
					(fn [& fn-args]
					(let [local-env (merge env (zipmap args fn-args))]
					(custom-eval body local-env)))))	
          (ifn? (first expr)) ;function call
      		  (if (not (get env (keyword (first expr)))) ;if it's not in env
				(let [op (resolve (first expr)) 
					args (rest expr)]
 					(print (map #(custom-eval % env) args))
				(apply op (map #(custom-eval % env) args)))

      			(let [fn (get env (keyword (first expr))) ;if it's a custom function from env
                args (rest expr)]
                (apply fn (map #(custom-eval % env) args)))
			  )
        :else expr) ;regular list of items
   	:else expr ;everything else: numbers, strings, etc
   	)
)



;; Sample Test Cases for Evaluating Expressions
(custom-eval '(+ x y 2) {:x 5, :y 3})
(ifn? (first '(sum 1 2)))
(println "Test Arithmetic: " (= (custom-eval '(* x y) {:x 10, :y 2}) 20))
(println "Test Nested Expr: " (= (custom-eval '(+ x (* y 2)) {:x 5, :y 3}) 11))

;; Part 3: Declaring and Using Functions
;; Expanded custom-eval function can now handle function definitions and calls within the given environment.
(def updated-env
  (custom-eval '(defn sum [a b] (+ a b)) initial-env))

(println "Test Function Call: " (= (custom-eval '(sum x y) updated-env) 30)) ;fail
(println "Test Function Call with Literals: " (= (custom-eval '(sum 15 25) updated-env) 40))

;; Part 4: Handling Errors
;; Error handling for undefined variables or functions and type mismatches.
(println "Error Handling: "
         (try
           (custom-eval '(sum x z) {:x 1 :y 2})
           (catch Exception e "Undefined variable or function")))

;; Students should implement the `custom-eval` function in a `.clj` file, include tests, and document their code.
