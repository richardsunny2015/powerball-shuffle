(ns powerball-shuffle.core
  (:require [cheshire.core :as json]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- csv-data->maps 
  [csv-data]
  (map zipmap
       (->> (first csv-data) ;; First row is the header
            (repeat))
       (rest csv-data)))

(defn- get-winning-numbers
  [record]
  (-> record
      (get "Winning Numbers")
      (str/split #" ")))

(defn- massage-record
  [record]
  (let [winning-numbers (get-winning-numbers record)]
    (mapv #(Integer/parseInt %) winning-numbers)))

(defn- massage-records
  "Takes sets of records and returns the winning
   numbers as a set."
  [records]
  (map massage-record records))

(defn- read-previous-powerball-numbers
  "Reads csv file from PAST_POWERBALL env variable
   and creates a set of previous powerball numbers"
  []
  (when-let [past-powerball-file (System/getenv "PAST_POWERBALL")]
    (with-open [r (io/reader past-powerball-file)]
      (let [results (-> r
                        (csv/read-csv)
                        (csv-data->maps))]
        (-> (massage-records results)
            (doall))))))

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
        numbers (conj main-numbers powerball)
        previous-number-set (set previous-powerball-numbers)]
    (if-not (contains? previous-number-set numbers)
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

(defn- count-frequencies
  [numbers-to-count]
  (->> (reduce
        (fn [acc numbers]
          (reduce
           (fn [acc number]
             (update acc number (fnil inc 0)))
           acc
           numbers))
        {}
        numbers-to-count)
       (into [])
       (sort-by second)))

(defn- count-powerball-frequencies
  []
  (let [main-numbers (map drop-last previous-powerball-numbers)
        powerball (map (fn [xs] [(last xs)]) previous-powerball-numbers)
        main-number-count (count-frequencies main-numbers)
        powerball-count (count-frequencies powerball)]
    (with-open [w1 (io/writer "target/main-number-counts.json")
                w2 (io/writer "target/powerball-counts.json")]
      (json/generate-stream main-number-count w1 {:pretty true})
      (json/generate-stream powerball-count w2 {:pretty true}))))

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

(comment 
  
  (count-powerball-frequencies)
  previous-powerball-numbers
  )