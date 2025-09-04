(ns powerball-shuffle.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- csv-data->maps 
  [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (repeat))
       (rest csv-data)))

(defn- massage-record
  [record]
  (let [winning-numbers (-> record 
                            (get "Winning Numbers")
                            (str/split #" "))]
    (mapv #(Integer/parseInt %) winning-numbers)))

(defn- massage-records
  "Takes sets of records and returns the winning
   numbers as a set."
  [records]
  (->> records 
       (map massage-record)
       (set)))

(defn- read-previous-powerball-numbers
  "Reads csv file from PAST_POWERBALL env variable
   and creates a set of previous powerball numbers"
  []
  (with-open [r (io/reader (System/getenv "PAST_POWERBALL"))]
    (let [results (-> r
                      (csv/read-csv)
                      (csv-data->maps))]
      (massage-records results))))

(def previous-powerball-numbers (read-previous-powerball-numbers))

(defn- powerball-numbers
  []
  (let [main-numbers (->> (range 1 70)
                          (shuffle)
                          (take 5)
                          (sort)
                          (vec))
        powerball (->> (range 1 27)
                       (shuffle)
                       (first))
        numbers (conj main-numbers powerball)]
    (if-not (contains? previous-powerball-numbers numbers)
      numbers
      (println "Generated existing numbers" numbers))))

(defn- lotto-numbers
  []
  (->> (range 1 60)
       (shuffle)
       (take 6)
       (sort)))

(defn generate-numbers
  [number-fn n]
  (loop [numbers []]
    (if (= n (count numbers))
      numbers
      (if-let [chosen-numbers (number-fn)]
        (recur (conj numbers chosen-numbers))
        (recur numbers)))))

(defn -main
  [& args]
  (let [n (Integer/parseInt (first args)) ;; amount of sets of powerball numbers to generate
        type (second args)
        number-fn (if (= "lotto" type)
                    lotto-numbers
                    powerball-numbers)
        numbers (generate-numbers number-fn n)]
    (doseq [number-set numbers]
      (->> number-set
           (str/join " ")
           (println)))))