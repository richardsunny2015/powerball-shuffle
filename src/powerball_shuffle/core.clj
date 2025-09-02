(ns powerball-shuffle.core)

(defn- choose-numbers
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

(defn powerball-numbers
  "Takes n and returns a list of n set of powerball
   numbers."
  [n]
  (loop [numbers []]
    (if (= n (count numbers))
      numbers
      (let [chosen-numbers (choose-numbers)]
        (recur (conj numbers chosen-numbers))))))
