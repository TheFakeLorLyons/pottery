^:kindly/hide-code
(ns pottery.clj.home
  (:require [scicloj.clay.v2.api :as clay]
            [scicloj.kindly.v4.kind :as kind]
            [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]
            [tablecloth.api :as tc]
            [clojure.data.csv :as csv]
            [tech.v3.dataset :as ds]
            [clojure.string :as str]))

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

(defn get-frequencies [column]
  (let [column-data
        (-> bulk-trans-data
        (tc/select-rows #(valid-score? (get % "q01")))
        (tc/group-by column)
        (tc/aggregate {"count" tc/row-count})
        (tc/rename-columns {column "score"})
        (tc/order-by [:score])
        (tc/rows :as-maps))] 
       column-data))

(def q01-frequencies2
  (-> bulk-trans-data
      (tc/select-rows #(= (get % "trans_sample") 1))
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

(def q1-text ["Please imagine a ladder with steps numbered from zero at the bottom to ten at the top."
              "The top of the ladder represents the best possible life for you, and the bottom of the ladder represents the worst possible life for you."
              "On which step of the ladder would you say you personally feel you stand at this time?"])

(def q01a-items (convert-to-individual-maps q01-frequencies))
(def q01b-items (convert-to-individual-maps q01-frequencies2))

(def q02-items (convert-to-individual-maps (get-frequencies "q02")))

(def layered-q01-bar-chart-data
  {:layer [{:data {:values q01a-items}
            :mark {:type "bar", :color "blue"} ;; Base data in blue
            :encoding
            {:x {:field "score", :type "ordinal", :axis {:title "Q01 Responses (0-10)"}}
             :y {:field "count", :type "quantitative", :axis {:title "Count"}}}}
           {:data {:values q01b-items}
            :mark {:type "bar", :color "red"}  ;; Overlay data in red
            :encoding
            {:x {:field "score", :type "ordinal", :axis {:title "Q01 Responses (0-10)"}}
             :y {:field "count", :type "quantitative", :axis {:title "Count"}}}}]
   :width 800
   :height 600
   :title {:text q1-text
           :fontSize 18
           :align "center"
           :anchor "middle"}})

(def q01-bar-chart-data
  (kind/vega
   (hc/xform
    ht/bar-chart
    :DATA q01a-items
    :X "score"
    :XSCALE {:title "Q01 Responses (0-10)"
             :format "d"}
    :Y "count"
    :XTYPE "ordinal"
    :YTITLE "Count"
    :TITLE {:text q1-text
            :anchor "middle"
            :frame "group"
            :align "center"
            :wrap "enabled"})))

(def q01-bar-chart-data2
  (kind/vega
   (hc/xform
    ht/bar-chart
    :DATA q01b-items
    :X "score"
    :XSCALE {:title "Q01 Responses (0-10)"
             :format "d"}
    :XTYPE "ordinal"
    :Y "count" 
    :YTITLE "Count"
    :TITLE {:text q1-text
            :anchor "middle"
            :frame "group"
            :align "center"
            :wrap "enabled"})))

(def layered-q01-bar-chart
  (kind/vega-lite
   {:data {:values (concat 
                    (map #(assoc % "group" "LGBT Population") q01a-items)
                    (map #(assoc % "group" "Trans Sample") q01b-items))}
    :width 600
    :height 400
    :mark {:type "bar"
           :opacity 0.7}
    :encoding {:x {:field "score"
                   :type "ordinal"
                   :axis {:title "Q01 Responses (0-10)"}
                   :scale {:format "d"}}
               :y {:field "count"
                   :type "quantitative"
                   :axis {:title "Count"}
                   :stack nil}
               :color {:field "group"
                       :type "nominal"
                       :scale {:range ["lightblue" "pink"]}}}
    :title {:text q1-text
            :fontSize 18
            :align "center"
            :anchor "middle"}}))


(def q02-bar-chart-data
  (kind/vega
   (hc/xform
    ht/bar-chart
    :DATA q02-items
    :X "score"
    :XSCALE {:title "Q01 Responses (0-10)"
             :format "d"}
    :Y "count"
    :XTYPE "ordinal"
    :YTITLE "Count"
    :TITLE "On which step do you think you will stand about five years from now?")))

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
  [:div
   {:style {:display "flex"
            :flex-direction "column"
            :justify-content "center"
            :text-align "center"}}
   [:div q01-bar-chart-data]
   [:div q01-bar-chart-data2]
   [:div layered-q01-bar-chart]
   [:div q02-bar-chart-data]]
  [:div table]
  [:div create-3d-scatter-plot]
  [:div ^:kind/dataset (tc/head apples 10)]])

(println (tc/head apples 5))

(comment
  (clay/make! {:format [:html] 
               :source-path "src/pottery/clj/home.clj"}))
