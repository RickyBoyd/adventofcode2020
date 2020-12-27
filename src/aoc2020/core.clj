(ns aoc2020.core)

(defn foo
  [x]
  (println x "Hello, World!"))


(defn nums-from-file 
  [file]
  (with-open [rdr (clojure.java.io/reader file)]
  (reduce conj [] (map #(Integer/parseInt %) (line-seq rdr)))))

(defn p1 
  [] 
  (let [input (sort (nums-from-file "input_p1.txt"))]
    (last 
      (for [i (range (count input))
            j (range (count input))
            :let [x (nth input i) y (nth input j) n (* x y)] 
            :when (= 2020 (+ x y))] 
        n))))