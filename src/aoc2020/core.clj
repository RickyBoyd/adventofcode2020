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

(defn p2
  []
  (let [input (sort (nums-from-file "input_p2.txt"))]
    (last
     (for [i (range (count input))
           j (range (count input))
           k (range (count input))
           :let [x (nth input i) y (nth input j) z (nth input k) n (* x (* y z))]
           :when (= 2020 (+ x (+ y z)))]
       n))))

(defn parse-password-policy-line
  [line]
  (let [[first second] (re-seq #"[0-9]+" line)
        letter (re-find #"[a-z]" line)
        pwd (re-find #"[a-z]+$" line)]
    {:first (Integer/parseInt first)
     :second (Integer/parseInt second)
     :letter letter
     :pwd pwd}))

(defn parse-password-policy
  [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (reduce conj [] (map parse-password-policy-line (line-seq rdr)))))

(defn get-count
  [letter word]
  (get (frequencies word) (.charAt letter 0) 0))

(defn valid-pwd?
  [input]
  (let [count (get-count (:letter input) (:pwd input))]
    (and (>= count (:first input))
         (<= count (:second input)))))

(defn d2-1
  []
  (let [input (parse-password-policy "input_d2-1.txt")]
    (reduce (fn [cnt val] (if (valid-pwd? val) (inc cnt) cnt)) 0 input)))

(defn xor
  [a b]
  (and (or a b) (or (not a) (not b))))

(defn valid-pwd2?
  [input]
  (let [letter (.charAt (:letter input) 0) pwd (:pwd input)]
    (xor (= letter (get pwd (dec (:first input))))
         (= letter (get pwd (dec (:second input)))))))

(defn d2-2
  []
  (let [input (parse-password-policy "input_d2-2.txt")]
    (reduce (fn [cnt val] (if (valid-pwd2? val) (inc cnt) cnt)) 0 input)))

(defn read-map
  [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (reduce conj [] (line-seq rdr))))

(defn tree?
  [map x y]
  (let [row (get map y)]
    (= \# (get row (mod x (count row))))))

; The below can be used for day 3 part 2 also by calling the appropriate steps
(defn d3-1
  [down right]
  (let [map (read-map "input_d3-1.txt")]
    (reduce +
            (for [line (range down (count map) down)]
              (if (space? map (* (/ line down) right) line) 0 1)))))