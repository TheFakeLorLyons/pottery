^:kindly/hide-code
(ns pottery.clj.home
  (:require [scicloj.clay.v2.api :as clay]
            [scicloj.kindly.v4.kind :as kind]
            [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]
            [tablecloth.api :as tc]
            [clojure.data.csv :as csv]))

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

(kind/hiccup
 [:div
  [:div size-weight]
  [:div crunchiness-juiciness]
  [:div simple-data]
  [:div table]
  [:div ^:kind/dataset (tc/head apples 10)]])

(println (tc/head apples 5))

(comment
  (clay/make! {:format [:html] 
               :source-path "src/pottery/clj/home.clj"}))
