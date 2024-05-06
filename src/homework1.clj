(ns homework1)

(def result (atom {:correct 0 :incorrect 0}))
(defmacro =check [left right]
  `(let [left# ~left
         right# ~right]
     (if (= right# left#)
       (swap! result update :correct inc)
       (do
         (println "mismatch:" ~(str (first left)) left# right#)
         (swap! result update :incorrect inc)))
     left#))

;; reduce examples
(defn my-reduce [f init coll]
  (loop [result init
         remaining coll]
    (if (empty? remaining)
      result
      (recur (f result (first remaining))
             (rest remaining)))))

(= (count []) 0)
(my-reduce + 0 [])
(my-reduce + 0 [1 2 3 4])
(=check (my-reduce + 0 [1 2 3 4]) 10)
(=check (my-reduce str "" ["a" "b" "c"]) "abc")
(=check (my-reduce + 0 (range 10000)) 49995000)


;; filter examples
(defn my-filter [pred coll]
  (loop [coll coll filtered [] checked 0 pred pred]
    (if (empty? coll) (reverse filtered)
       	(recur (rest coll) (if (pred (first coll)) (cons (first coll) filtered) filtered)
            		 (inc checked) pred)
   	))
)

(=check (my-filter even? [1 2 3 4 5 6]) [2 4 6])
(=check (my-filter #(> (count %) 3) ["hi" "hello" "hey" "greetings"]) ["hello" "greetings"])
(=check (my-filter #(and (even? %) (> % 10)) [12 2 13 14 3]) [12 14])

;; concat examples
(defn my-concat [coll1 coll2]
  (loop [result coll1
         remaining coll2]
    (if (empty? remaining)
      result
      (recur (conj result (first remaining)) (rest remaining)))))

(=check (my-concat [1 2] [3 4]) [1 2 3 4])
(=check (count (my-concat (range 5000) (range 5000 10000))) 10000)


;; nth examples
(defn my-nth [coll index]
  (if (= (count coll) (+ index 1)) (last coll) 
  (if (= index 0) (first coll)
      (my-nth (rest coll) (dec index))))
  )

(=check (my-nth [10 20 30 40] 2) 30)
(=check (my-nth [1 2 3 4] 10) nil) ; Assuming nil for out of bounds
(=check (my-nth [1 2 3 4] 3) 4)

;; max/min examples
(defn my-max [coll]
  (loop [min-val (first coll)
         coll (rest coll)]
    (if (empty? coll)
      min-val
      (recur (if ( > (first coll) min-val) (first coll) min-val) ;
             (rest coll))))
)

(defn my-min [coll]
  (loop [min-val (first coll) 
         coll (rest coll)]    
    (if (empty? coll)         
      min-val                 
      (recur (if (< (first coll) min-val) (first coll) min-val) ;
             (rest coll))))
	 
)
(=check (my-max [5 3 9 1]) 9)
(=check (my-min [5 3 9 1]) 1)
(=check (my-max [-5 -3 -9 -1]) -1)
(=check (my-min [-5 -3 -9 -1]) -9)
(=check (my-max []) nil)
(=check (my-min []) nil)

;; count examples
(defn my-count [coll]
(if (empty? coll)
   0
   (inc (my-count (rest coll)))))

(=check (my-count [1 2 3 4 5]) 5)
(=check (my-count [[1 2] [3 4] [5]]) 3)
(=check (my-count []) 0)

;; take examples
(defn my-take [n coll]
  (if (= (count coll) n) 
       coll
	   (my-take n (pop coll))
  )
)

(=check (my-take 3 [5 4 3 2 1]) [5 4 3])

;; merge examples
(defn my-merge [map1 map2]
  (loop [map1 map1 map2 map2] 
    (if (empty? map2) map1
   	(recur (assoc map1 (first (first map2)) (last (first map2))) (rest map2)))))
  
(=check (my-merge {:a 1 :b 2} {:b 3 :c 4}) {:a 1 :b 3 :c 4})
(=check (my-merge {:foo "bar"} {:foo "baz", :hello "world"}) {:foo "baz", :hello "world"})
(=check (my-merge {} {:a 1}) {:a 1})

;; group-by examples
(defn my-group-by [f coll]
  (loop [coll (reverse coll)
         result {}]
    (if (empty? coll)
      result
      (let [x (first coll)
            key (f x)
            updated-result (update result key  #(conj % x))]
        (recur (rest coll) updated-result)))))

(=check (my-group-by :type [{:type :a :value 1} {:type :b :value 2} {:type :a :value 3}])
        {:a [{:type :a :value 1} {:type :a :value 3}], :b [{:type :b :value 2}]})
(=check (my-group-by even? [1 2 3 4 5 6]) {true [2 4 6], false [1 3 5]})
(=check (my-group-by count ["one" "two" "three" "four"]) {3 ["one" "two"], 5 ["three"], 4 ["four"]})

;; keys examples
(defn my-keys [map]
  (loop [keys-coll []
         map (seq map)]
    (if (empty? map) 
      keys-coll
      (recur (conj keys-coll (first (first map))) 
             (rest map))))) 

(=check (my-keys {:a 1 :b 2 :c 3}) [:a :b :c])
(=check (my-keys {:foo "bar" :baz "qux"}) [:foo :baz])
(=check (my-keys {}) [])

;; vals examples
(defn my-vals [map]
  (loop [vals-coll []
         map (seq map)]
    (if (empty? map)
      vals-coll
      (recur (conj vals-coll (last (first map)))
             (rest map)))))
(=check (my-vals {:a 1 :b 2 :c 3}) [1 2 3])
(=check (my-vals {:foo "bar" :baz "qux"}) ["bar" "qux"])
(=check (my-vals {}) [])

;; select-keys examples
(defn my-select-keys [map keys]
  (loop [selected-map {}
      	 keys keys]
    (if (empty? keys)
      selected-map
      (recur (assoc selected-map (first keys) (get map (first keys)))
         	 (rest keys)	 
             )))
)

(=check (my-select-keys {:a 1 :b 2 :c 3} [:a :c]) {:a 1 :c 3})
(=check (my-select-keys {:name "Alice" :age 30 :gender "Female"} [:name :age]) {:name "Alice", :age 30})
(=check (my-select-keys {:foo "bar" :hello "world"} [:foo]) {:foo "bar"})

(println "Test results:" @result)
