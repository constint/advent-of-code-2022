(ns day5
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]))

;--- Day 5: Supply Stacks ---
;
;The expedition can depart as soon as the final supplies have been unloaded from the ships. Supplies are stored in stacks of marked crates, but because the needed supplies are buried under many other crates, the crates need to be rearranged.
;
;The ship has a giant cargo crane capable of moving crates between stacks. To ensure none of the crates get crushed or fall over, the crane operator will rearrange them in a series of carefully-planned steps. After the crates are rearranged, the desired crates will be at the top of each stack.
;
;The Elves don't want to interrupt the crane operator during this delicate procedure, but they forgot to ask her which crate will end up where, and they want to be ready to unload them as soon as possible so they can embark.
;
;They do, however, have a drawing of the starting stacks of crates and the rearrangement procedure (your puzzle input). For example:
;```
;    [D]
;[N] [C]
;[Z] [M] [P]
; 1   2   3
;
;move 1 from 2 to 1
;move 3 from 1 to 3
;move 2 from 2 to 1
;move 1 from 1 to 2
;```
(def example "    [D]\n[N] [C]\n[Z] [M] [P]\n 1   2   3\n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2")
(defonce day5-input (slurp "resources/day5.txt"))

(defn parse-line [line]
  (->> line
       (partition 3 4 1)
       (map #(nth % 1))
       (map #(if (= \  %) nil %))))

(defn nil-pad [n coll]
  (take n (concat coll (repeat nil))))

(defn square-matrix [stack-lines]
  (let [longest-line-count (apply max (map count stack-lines))]
    (map #(nil-pad longest-line-count %) stack-lines)))

(defn parse-stack [stack-lines]
  (->> stack-lines
       (drop-last)                                          ; drop indices
       (map parse-line)
       (square-matrix)                                      ; create list of characters
       (apply mapv vector)                                  ; reverse matrix
       (mapv #(drop-while nil? %))                          ; remove leading nils
       ))

;> Instead of a vertical view, this created an horizontal view where
;> - the outer list is a list of columns
;> - the inner list are the columns, where the front of the list is the top of the column

(defn parse-moves [move-lines]
  (map (fn [move]
         (let [[_ number source destination] (re-matches #"move (\d+) from (\d+) to (\d+)" move)]
           {:num         (Integer/parseInt number)
            :source      (Integer/parseInt source)
            :destination (Integer/parseInt destination)}))
       move-lines))

(defn parse-input [input]
  (let [[stack-lines _ move-lines] (->> input
                                        (clojure.string/split-lines)
                                        (partition-by empty?))]
    {:stack (parse-stack stack-lines)
     :moves (parse-moves move-lines)}))

(parse-input example)


;In this example, there are three stacks of crates. Stack 1 contains two crates: crate Z is on the bottom, and crate N is on top. Stack 2 contains three crates; from bottom to top, they are crates M, C, and D. Finally, stack 3 contains a single crate, P.
;
;Then, the rearrangement procedure is given. In each step of the procedure, a quantity of crates is moved from one stack to a different stack. In the first step of the above rearrangement procedure, one crate is moved from stack 2 to stack 1, resulting in this configuration:
;```
;[D]
;[N] [C]
;[Z] [M] [P]
; 1   2   3
;```
;In the second step, three crates are moved from stack 1 to stack 3. Crates are moved one at a time, so the first crate to be moved (D) ends up below the second and third crates:
;```
;        [Z]
;        [N]
;    [C] [D]
;    [M] [P]
; 1   2   3
;```
;Then, both crates are moved from stack 2 to stack 1. Again, because crates are moved one at a time, crate C ends up below crate M:
;```
;        [Z]
;        [N]
;[M]     [D]
;[C]     [P]
; 1   2   3
;```
;Finally, one crate is moved from stack 1 to stack 2:
;```
;        [Z]
;        [N]
;        [D]
;[C] [M] [P]
; 1   2   3
;```
;The Elves just need to know which crate will end up on top of each stack; in this example, the top crates are C in stack 1, M in stack 2, and Z in stack 3, so you should combine these together and give the Elves the message CMZ.
;
;After the rearrangement procedure completes, what crate ends up on top of each stack?


(defn apply-moves [{:keys [stack moves]}]
  (reduce
    (fn [current-stack {:keys [source destination num]}]
      ;(prn source destination num)
      ;(prn "current stack" current-stack)
      (let [moved-vals (reverse (take num (nth current-stack (dec source))))]
        ;(prn "moved vals" moved-vals)
        (-> current-stack
            (update-in [(dec source)] #(drop num %))
            (update-in [(dec destination)] #(concat moved-vals %))
            )))
    stack
    moves))

(apply-moves (parse-input example))

(defn stack-tops [input]
  (->> input
       parse-input
       apply-moves
       (map first)
       (reduce str)))

(stack-tops example)
(stack-tops day5-input)

(defn apply-moves2 [{:keys [stack moves]}]
  (reduce
    (fn [current-stack {:keys [source destination num]}]
      ;(prn source destination num)
      ;(prn "current stack" current-stack)
      (let [moved-vals  (take num (nth current-stack (dec source)))] ; <- only line modified
        ;(prn "moved vals" moved-vals)
        (-> current-stack
            (update-in [(dec source)] #(drop num %))
            (update-in [(dec destination)] #(concat moved-vals %))
            )))
    stack
    moves))

(defn stack-tops2 [input]
  (->> input
       parse-input
       apply-moves2
       (map first)
       (reduce str)))

(stack-tops2 example)
(stack-tops2 day5-input)