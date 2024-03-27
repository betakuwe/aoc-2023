(ns aoc.2023.day2
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

(def filepath "day2.txt")

(def lines (str/split-lines (slurp filepath)))

(def sample-line "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")

(def limits {"red" 12 "green" 13 "blue" 14})

(def empty-colors {"red" 0 "green" 0 "blue" 0})

(defn- over-limit?
  [m]
  (some (fn [[k v]] (> v (get limits k))) m))

(defn- game->reveals
  [game]
  (->> (str/split game #" ")
       (drop 2)
       (partition 2)
       (map (fn [[n c]]
              [(Integer/parseInt n)
               (str/replace c #",|;" "")
               (= \; (last c))]))))

(defn- game-is-possible
  [game]
  (->> game
       game->reveals
       (reduce (fn [m [n c ending]]
                 (let [updated-m (update m c (partial + n))]
                   (cond
                     (over-limit? updated-m) (reduced nil)
                     ending empty-colors
                     :else updated-m)))
               empty-colors)
       nil?
       not))

(defn part1
  [lines]
  (->> lines
       (map-indexed (fn [i g] [(inc i) g]))
       (filter (fn [[_ g]] (game-is-possible g)))
       (map first)
       (apply +)))

(println (part1 lines))

(defn- game->power
  [game]
  (->> game
       game->reveals
       (reduce (fn [m [n c _]]
                 (update m c (partial max n)))
               empty-colors)
       vals
       (apply *)))

(defn part2
  [lines]
  (->> lines
       (map game->power)
       (apply +)))

(println (part2 lines))

       


