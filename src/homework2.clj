;Problem 58, Function Composition
;https://4clojure.oxal.org/#/problem/58

(defn my-comp 
  ([] identity)
  ([f] f)
  ([f g] (fn [& args] (f (apply g args))))
  ([f g & fns] (reduce my-comp (list* f g fns))))

(= [3 2 1] ((my-comp rest reverse) [1 2 3 4]))
(= 5 ((my-comp  (partial + 3) second) [1 2 3 4]))
(= true ((my-comp zero? #(mod % 8) +) 3 5 7 9))
(= "HELLO" ((my-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world"))

;Problem 59, Juxtaposition
;https://4clojure.oxal.org/#/problem/59

(defn my-juxt 
  ([& fns] 
   (fn [& args] 
     (loop [fns fns res []]
     (if (=(count fns) 0) res
     	(recur (rest fns) (conj res (apply (first fns) args))))))))

(= [21 6 1] ((my-juxt + max min) 2 3 5 1 6 4))
(= ["HELLO" 5] ((my-juxt #(.toUpperCase %) count) "hello"))
(= [2 6 4] ((my-juxt :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))

;Problem 69, Merge with a Function
;https://4clojure.oxal.org/#/problem/69

(defn conj-with [f a b]
  (if (empty? b) a
	(let [b-key (first (first b))
          b-value (second (first b))]
       (if (get a b-key)
     	   (conj-with f 
               	(conj a [b-key (f (get a b-key) b-value)]) (rest b))
     		(conj-with f
               	 (conj a [b-key b-value]) (rest b))))))

(defn my-merge-with [f & maps] 
  (reduce (partial conj-with f) maps))


(= (my-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})
(= (my-merge-with - {1 10, 2 20} {1 3, 2 10, 3 15})
   {1 7, 2 10, 3 15})
(= (my-merge-with concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
   {:a [3 4 5], :b [6 7], :c [8 9]})


;Problem 158, Decurry
;https://4clojure.oxal.org/#/problem/158

(defn decurry [func]
  (fn [& args]
    (let [step (fn step [f args]
                 (if (empty? args)
                   f
                   (recur (apply f [(first args)]) (rest args))))]
      (step func args))))



(= 10 ((decurry (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (+ a b c d)))))) 1 2 3 4))

(= 24 ((decurry (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (* a b c d))))))
       1 2 3 4))

(= 25 ((decurry (fn [a]
             (fn [b]
               (* a b))))
       5 5))