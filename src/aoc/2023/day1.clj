(ns aoc.2023.day1)

(set! *warn-on-reflection* true)

(require '[clojure.string :as str])

(def filepath "day1.txt")

(def lines (str/split-lines (slurp filepath)))

;; Part 1

(defn process-line
  [l]
  (let [m (re-seq #"\d" l)
        n (str (first m) (last m))]
    (Integer/parseInt n)))

(defn part1
  [lines]
  (->> lines
       (map process-line)
       (apply +)))

(println (part1 lines))

;; Part 2

(def digit-words
  ["zero" "one" "two" "three" "four"
   "five" "six" "seven" "eight" "nine"])

(def digit-map
  (into {}
        (map-indexed #(vector %2 %1))
        digit-words))

(defn find-first-digits
  [s]
  (into {}
        (map #(vector % (str/index-of s (str %)))
             (take 10 (range)))))

(defn find-first-digit-words
  [s]
  (into {}
        (map #(vector (get digit-map %) (str/index-of s %)))
        digit-words))

(defn- min-or-nil
  [& args]
  (let [valid-args (remove nil? args)]
    (if (empty? valid-args)
      nil
      (apply min valid-args))))

(defn find-first-digit-or-word
  [s]
  (let [merged-maps (merge-with min-or-nil
                                (find-first-digits s)
                                (find-first-digit-words s))]
    ((->> merged-maps
          vec
          (filter #(not (nil? (% 1))))
          (apply min-key #(% 1))) 0)))

(defn find-last-digits
  [s]
  (into {}
        (map #(vector % (str/last-index-of s (str %)))
             (take 10 (range)))))

(defn find-last-digit-words
  [s]
  (into {}
        (map #(vector (get digit-map %) (str/last-index-of s %)))
        digit-words))

(defn- max-or-nil
  [& args]
  (let [valid-args (remove nil? args)]
    (if (empty? valid-args)
      nil
      (apply max valid-args))))

(defn find-last-digit-or-word
  [s]
  (let [merged-maps (merge-with max-or-nil
                                (find-last-digits s)
                                (find-last-digit-words s))]
    ((->> merged-maps
          vec
          (filter #(not (nil? (% 1))))
          (apply max-key #(% 1)))
     0)))

(defn process-line2
  [line]
  (let [a (find-first-digit-or-word line)
        b (find-last-digit-or-word line)]
    (Integer/parseInt (str a b))))

(defn part2
  [lines]
  (->> lines
       (map process-line2)
       (apply +)))

(println (part2 lines))