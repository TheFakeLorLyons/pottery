^:kindly/hide-code
(ns pottery.clj.home
  (:require [scicloj.clay.v2.api :as clay]
            [scicloj.kindly.v4.kind :as kind]
            [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]
            [tablecloth.api :as tc]
            [clojure.data.csv :as csv]
            [tech.v3.dataset :as ds]))

["# Beginning"]

(def table
  ^:kind/table
  {:column-names (range 1 13)
   :row-vectors (for [i (range 1 13)]
                  (for [j (range 1 13)]
                    (* i j)))})
; a map keeps the metadata because it evaluates to itself

(-> "resources/assets/apples/apple_quality.csv"
    slurp
    csv/read-csv
    first)

(def apples
  (tc/dataset "resources/assets/apples/apple_quality.csv"))

(def bulk-trans-data
  (tc/dataset "resources/assets/trans-health/final-trans-data.csv"))

(println "Number of rows:" (count (tc/rows bulk-trans-data)))
(println "Number of columns:" (count (tc/columns bulk-trans-data)))

(def dsApples
  (ds/->dataset "resources/assets/apples/apple_quality.csv"))

`(-> apples
    tc/select-columns [:Crunchiness :Ripeness :Quality])

(def color-mapping {"Good" "blue" "Bad" "red"})

(def size-weight
  (kind/vega     ;fn call
   (hc/xform ht/point-chart
             :DATA (-> apples
                       (tc/select-columns ["Size" "Weight" "Quality"])
                       (tc/rows :as-maps))
             :X :Size
             :COLOR "Quality"
             :Y :Weight)))
;make a form here so I can drop down what I want to chart against 
(defn valid-score? [x]
  (when (number? x)
    (and (>= x 0) (<= x 10))))

(def q01-frequencies
  (-> bulk-trans-data
      (tc/select-rows #(valid-score? (get % "q01")))
      (tc/group-by "q01")
      (tc/aggregate {"count" tc/row-count})
      (tc/rename-columns {"q01" "score"})
      (tc/order-by [:score])
      (tc/rows :as-maps)))

(defn convert-to-individual-maps [data]
  (->> data
       (map (fn [item]
              {"score" (:$group-name item)
               "count" (get item "count")}))
       (filter (fn [item] (valid-score? (get item "score"))))
       (sort-by #(get % "score"))))

(def separate-items (convert-to-individual-maps q01-frequencies))

(def q01-bar-chart-data
  (kind/vega
   (hc/xform
    ht/bar-chart
    :DATA separate-items
    :X "score"
    :XSCALE {:title "Q01 Responses (0-10)"
             :format "d"}
    :Y "count"
    :XTYPE "ordinal"
    :YTITLE "Count"
    :TITLE "Frequencies of q01 Scores")))

(defn prepare-data [dataset column]
  (let [valid-range? (fn [x] (and (number? x) (<= 0 x 10)))]
    (->> dataset
         (tc/select-columns [column])
         (tc/rows)
         (map #(get % column))
         (filter valid-range?)
         (frequencies)
         (map (fn [[k v]] {:q01 k :count v})))))

(def q01-chart
  (kind/vega
   (hc/xform
    {:data (-> bulk-trans-data
               (tc/select-columns ["q01"])
               (tc/rows [prepare-data bulk-trans-data :q01]))
     :mark "bar"
     :encoding {:x {:field "q01"
                    :type "quantitative"
                    :axis {:title "Q01 Responses (0-10)"
                           :format "d"}
                    :scale {:domain [0 10]}}
                :y {:field "count"
                    :type "quantitative"
                    :axis {:title "Count"}}
                :tooltip [{:field "q01" :type "quantitative" :title "Q01 Response"}
                          {:field "count" :type "quantitative" :title "Count"}]}
     :width 600
     :height 400
     :title "Distribution of Q01 Responses"})))

(def create-3d-scatter-plot
  (kind/plotly
  (let [data dsApples]
    {:data [{:x (ds/column data "Sweetness")
             :y (ds/column data "Ripeness")
             :z (ds/column data "Juiciness")
             :mode "markers"
             :type "scatter3d"
             :marker {:size 5
                      :color (map #(if (= % "good") 0 1) (ds/column data "Quality"))
                      :colorscale [[0 "blue"] [1 "red"]]
                      :colorbar {:title "Quality"}
                      :showscale true}}]
     :layout {:title "Apples 3D Scatter Plot"
              :scene {:xaxis {:title "Sweetness"}
                      :yaxis {:title "Ripeness"}
                      :zaxis {:title "Juiciness"}}
              :width 800
              :height 600}})))

(kind/hiccup
 [:div
  [:div size-weight]
  [:div q01-bar-chart-data]
  [:div table]
  [:div create-3d-scatter-plot]
  [:div ^:kind/dataset (tc/head apples 10)]])

(println (tc/head apples 5))

(comment
  (clay/make! {:format [:html] 
               :source-path "src/pottery/clj/home.clj"}))
