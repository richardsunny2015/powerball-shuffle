(ns powerball-shuffle.check-numbers)

(defn- setup
  [winning-numbers]
  (fn [numbers-to-check]
    (let [winning-powerball (last winning-numbers)
          rest-of-winning-numbers (-> (drop-last winning-numbers)
                                      (set))
          powerball-check (last numbers-to-check)
          rest-of-check (drop-last numbers-to-check)
          winning-numbers (filter #(contains? rest-of-winning-numbers %)
                                  rest-of-check)
          powerball-match? (= winning-powerball powerball-check)
          results [numbers-to-check winning-numbers]]
      (cond-> results
        powerball-match? (conj winning-powerball)))))

(defn check
  [winning-numbers numbers-to-check]
  (->> numbers-to-check
       (map (setup winning-numbers))
       (remove (comp empty? second))))

