
(def super-set (atom #{}))

(defn get-superset [column]
    (swap! super-set conj 
    (-> bulk-trans-data
        (tc/select-rows #(valid-score? (get % "q01")))
        (tc/group-by column)
        (tc/aggregate {"count" tc/row-count})
        (tc/rename-columns {column "score"})
        (tc/order-by [:score])
        (tc/rows :as-maps))))


(defn get-LGBT-stats [column]
  (get-superset column))

(defn get-trans-stats [column]
  (-> column
      (get-superset)
      (tc/select-rows #(= (get % "trans_sample") 1))))

(defn get-LGBT-percentages [column]
  (let [total-count (reduce + (map #(get % "count") column-data))]
    
    (map #(assoc % "percentage" (/ (get % "count") total-count)) column-data)))

(defn convert-to-percentage [column-data]
  (let [total-count (reduce + (map #(get % "count") column-data))]
     (map #(assoc % "percentage" (/ (get % "count") total-count)) column-data)))

(defn get-trans-percentages [column]
  (-> (get-superset column)
      (tc/select-rows #(= (get % "trans_sample") 1))
      (convert-to-percentage)))


(defn get-LGBT-percentages [column]
  (-> (get-superset column)
      (convert-to-percentage)))