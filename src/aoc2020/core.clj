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

(defn split-file-by-blanks
  [filename]
  (clojure.string/split (slurp filename) #"\n\n"))

(defn valid-passport?
  [passport]
  (some?
   (and
    (re-find #"byr" passport)
    (re-find #"iyr" passport)
    (re-find #"eyr" passport)
    (re-find #"hgt" passport)
    (re-find #"hcl" passport)
    (re-find #"ecl" passport)
    (re-find #"pid" passport))))

(defn d4-1
  []
  (let [passports (split-file-by-blanks "input_d4-1.txt")]
    (reduce (fn [cnt pport] (if (valid-passport? pport) (inc cnt) cnt)) 0 passports)))

(defn valid-passport2?
  [passport-in]
  (let [passport (str passport-in " ")]
  (boolean
   (and
    (re-find #"byr:(19[2-9][0-9]|200[0-2])\s" passport)
    (re-find #"iyr:(201[0-9]|2020)\s" passport)
    (re-find #"eyr:(202[0-9]|2030)\s" passport)
    (let [[_ number-str type] (re-find #"hgt:([0-9]+)(cm|in)\s" passport)]
      (and number-str type
           (let [number (Integer/parseInt number-str)]
             (cond
               (= type "cm") (<= 150 number 193)
               (= type "in") (<= 59 number 76)
               :else nil))))
    (re-find #"hcl:#([a-f]|[0-9]){6}\s" passport)
    (re-find #"ecl:(amb|blu|brn|gry|grn|hzl|oth)\s" passport)
    (re-find #"pid:([0-9]{9})\s" passport)))))

(defn d4-2
  []
  (let [passports (split-file-by-blanks "input_d4-2.txt")]
    (reduce (fn [cnt pport] (if (valid-passport2? pport) (inc cnt) cnt)) 0 passports)))