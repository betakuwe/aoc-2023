(ns aoc.2023.day3
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def filepath "day3.txt")

(def sample "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..")

(defn make-matrix
  [input]
  (->> input
       str/split-lines
       (map vec)
       vec))

(def sample-matrix (make-matrix sample))

;; Part 1

(defn is-symbol?
  [c]
  (and (not (Character/isDigit ^char c)) (not= \. c)))

(defn safe-subvec
  [v start end]
  (let [v-len (count v)
        safe-start (max 0 (min start v-len))
        safe-end (max safe-start (min end v-len))]
    (subvec v safe-start safe-end)))

(defn- get-level
  "level is one of :above, :current, :below"
  [matrix i j num-str level]
  (some-> matrix
          (get (case level
                 :above (dec i)
                 :current i
                 :below (inc i)))
          (safe-subvec (dec j) (+ (count num-str) j 1))))

(defn eval-matrix-num
  [matrix i j]
  (let [num-str (->> (subvec (matrix i) j)
                     (take-while #(Character/isDigit ^char %))
                     (apply str))
        not-part? (->> [:above :current :below]
                       (mapcat #(get-level matrix i j num-str %))
                       (filter is-symbol?)
                       empty?)]
    (if not-part? 0 (Integer/parseInt num-str))))


(defn matrix-value
  [matrix i j]
  (let [c ((matrix i) j)]
    (cond
      (not (Character/isDigit ^char c)) 0
      (and (pos? j) (Character/isDigit ^char ((matrix i) (dec j)))) 0
      :else (eval-matrix-num matrix i j))))

(defn eval-matrix
  [matrix]
  (->> matrix
       (map-indexed (fn [i row]
                      (map-indexed (fn [j _] (matrix-value matrix i j))
                                   row)))))

(defn sum-matrix
  [matrix]
  (->> matrix
       eval-matrix
       (map #(apply + %))
       (apply +)))

(def input (slurp filepath))

(println (sum-matrix (make-matrix input)))

;; Part 2

(defn within-bounds?
  [matrix i j]
  (and (< -1 i (count matrix)) (< -1 j (count (matrix i)))))

(defn pos->num
  [matrix i j]
  (when (within-bounds? matrix i j)
    (let [row (matrix i) elem (row j)]
      (when (Character/isDigit ^char elem)
        (let [leftmost-j (->> (range j -1 -1)
                              (take-while #(Character/isDigit ^char (row %)))
                              last)]
          (->> (drop leftmost-j row)
               (take-while #(Character/isDigit ^char %))
               (apply str)
               Integer/parseInt))))))

(defn pos->nearby-num
  [matrix i j]
  (let [i- (dec i) i+ (inc i) j- (dec j) j+ (inc j)
        pair->num (partial apply pos->num matrix)
        pairs->nums (partial map pair->num)
        left-right (pairs->nums [[i j-] [i j+]])
        upper-lower->nums (fn [[p ps]] (or (pair->num p) (pairs->nums ps)))
        lower (upper-lower->nums [[i+ j] [[i+ j-] [i+ j+]]])
        upper (upper-lower->nums [[i- j] [[i- j-] [i- j+]]])]
    (->> (list left-right lower upper)
         flatten
         (remove nil?))))

(defn pos->ratio
  [matrix i j]
  (let [nums (pos->nearby-num matrix i j)]
    (if (and (= \* ((matrix i) j)) (= 2 (count nums)))
      (apply * nums)
      0)))

(defn eval-matrix2
  [matrix]
  (->> matrix
       (map-indexed (fn [i row]
                      (map-indexed (fn [j _elem] (pos->ratio matrix i j))
                                   row)))))

(defn sum-matrix2
  [matrix]
  (->> matrix
       eval-matrix2
       (map #(apply + %))
       (apply +)))

(println (sum-matrix2 (make-matrix input)))
