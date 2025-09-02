(ns powerball-shuffle.core
  (:require [clojure.string :as str]))

(defn- powerball-numbers
  []
  (let [main-numbers (->> (range 1 70)
                          (shuffle)
                          (take 5)
                          (sort)
                          (vec))
        powerball (->> (range 1 27)
                       (shuffle)
                       (first))]
    (conj main-numbers powerball)))

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
      (let [chosen-numbers (number-fn)]
        (recur (conj numbers chosen-numbers))))))

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