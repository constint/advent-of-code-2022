(ns day1)

(def example "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000")

(defn parse-input [input]
  (->> input
       clojure.string/split-lines
       (partition-by clojure.string/blank?)
       (filter #(not (clojure.string/blank? (first %))))
       (map #(map (fn [str] (Integer/parseInt str)) %))))

(defn max-calories [input]
  (->> input
       parse-input
       (map #(reduce + %))
       sort
       last
       ))

(max-calories example)

(max-calories (slurp "resources/day1.txt"))

(defn top3-sum-calories [input]
     (->> input
          parse-input
          (map #(reduce + %))
          sort
          reverse
          (take 3)
          (reduce +)
          ))

(top3-sum-calories example)

(top3-sum-calories (slurp "resources/day1.txt"))
