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

(+ 3 3)

(def table
  ^:kind/table
  {:column-names (range 1 13)
   :row-vectors (for [i (range 1 13)]
                  (for [j (range 1 13)]
                    (* i j)))})

(-> "resources/assets/apples/apple_quality.csv"
    slurp
    csv/read-csv
    first)

(def apples
  (tc/dataset "resources/assets/apples/apple_quality.csv"))

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
;what

(def crunchiness-juiciness
  (kind/vega     ;fn call
   (hc/xform ht/point-chart
             :DATA (-> apples
                       (tc/select-columns ["Crunchiness" "Weight" "Juiciness"])
                       (tc/rows :as-maps))
             :X :Size
             :COLOR "Quality"
             :Y :Weight)))

(def simple-data
  ^:kind/table
  {:column-names ["A" "B" "C"]
   :row-vectors (for [i (range 1 4)]
                  (for [j (range 1 4)]
                    (* i j)))})
; a map keeps the metadata because it evaluates to itself

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
  [:div crunchiness-juiciness]
  [:div simple-data]
  [:div table]
  [:div create-3d-scatter-plot]
  [:div ^:kind/dataset (tc/head apples 10)]])

(println (tc/head apples 5))

(comment
  (clay/make! {:format [:html] 
               :source-path "src/pottery/clj/home.clj"}))
