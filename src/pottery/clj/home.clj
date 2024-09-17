^:kindly/hide-code
(ns pottery.clj.home
  (:require [scicloj.clay.v2.api :as clay]
            [scicloj.kindly.v4.kind :as kind]
            [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]
            [tablecloth.api :as tc]
            [clojure.data.csv :as csv]
            [tech.v3.dataset :as ds]
            [pottery.clj.questions :as ques]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                               
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def apples
  (tc/dataset "resources/assets/apples/apple_quality.csv"))

(def dsApples
  (ds/->dataset "resources/assets/apples/apple_quality.csv"))

(def size-weight
  (kind/vega     ;fn call
   (hc/xform ht/point-chart
             :DATA (-> apples
                       (tc/select-columns ["Size" "Weight" "Quality"])
                       (tc/rows :as-maps))
             :X :Size
             :COLOR "Quality"
             :Y :Weight)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def bulk-trans-data
  (tc/dataset "resources/assets/trans-health/final-trans-data.csv"))

(defn valid-score? [x]
  (when (number? x)
    (and (>= x 0) (<= x 10))))

(defn get-LGBT-stats [column]
  (let [column-data
        (-> bulk-trans-data
        (tc/select-rows #(valid-score? (get % "q01")))
        (tc/group-by column)
        (tc/aggregate {"count" tc/row-count})
        (tc/rename-columns {column "score"})
        (tc/order-by [:score])
        (tc/rows :as-maps))] 
       column-data))

(defn get-trans-stats [column]
  (-> bulk-trans-data
      (tc/select-rows #(= (get % "trans_sample") 1))
      (tc/select-rows #(valid-score? (get % "q01")))
      (tc/group-by column)
      (tc/aggregate {"count" tc/row-count})
      (tc/rename-columns {column "score"})
      (tc/order-by [:score])
      (tc/rows :as-maps)))

(defn convert-to-individual-maps [data]
  (->> data
       (map (fn [item]
              {"score" (:$group-name item)
               "count" (get item "count")}))
       (filter (fn [item] (valid-score? (get item "score"))))
       (sort-by #(get % "score"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generate-graphs-for-column [column-name question-text]
  (let [lgbt-items (convert-to-individual-maps (get-LGBT-stats column-name))
        trans-items (convert-to-individual-maps (get-trans-stats column-name))

        lgbt-chart (kind/vega
                    (hc/xform
                     ht/bar-chart
                     :DATA lgbt-items
                     :X "score"
                     :XSCALE {:title (str column-name " Responses")
                              :format "d"}
                     :XTYPE "ordinal"
                     :COLOR {:value "lightblue"} 
                     :Y "count" 
                     :YTITLE "Count"
                     :TITLE {:text (str column-name " - LGBT Population") 
                             :frame "group"
                             :align "center"
                             :wrap "enabled"}))

        trans-chart (kind/vega
                     (hc/xform
                      ht/bar-chart
                      :DATA trans-items
                      :X "score"
                      :XSCALE {:title (str column-name " Responses")
                               :format "d"}
                      :XTYPE "ordinal"
                      :COLOR {:value "pink"} 
                      :Y "count"
                      :YTITLE "Count"
                      :TITLE {:text (str column-name " - Transgender Sample") 
                              :frame "group"
                              :align "center"
                              :wrap "enabled"}))

        layered-chart (kind/vega-lite
                       {:data {:values (concat
                                        (map #(assoc % "group" "LGBT Population") lgbt-items)
                                        (map #(assoc % "group" "Trans Sample") trans-items))}
                        :width 600
                        :height 400
                        :mark {:type "bar"
                               :opacity 0.7}
                        :encoding {:x {:field "score"
                                       :type "ordinal"
                                       :axis {:title (str column-name " Responses")}
                                       :scale {:format "d"}}
                                   :y {:field "count"
                                       :type "quantitative"
                                       :axis {:title "Count"}
                                       :stack nil}
                                   :color {:field "group"
                                           :type "nominal"
                                           :scale {:range ["lightblue" "pink"]}}}
                        :title {:text (str column-name " - Compared")
                                :fontSize 18}})]
    {:subheading (str column-name " - " question-text)
     :lgbt-chart lgbt-chart
     :trans-chart trans-chart
     :layered-chart layered-chart}))

(defn generate-all-graphs [dataset]
  (let [columns-to-process (filter #(re-matches #"q\d+(_.*)?" %) (ds/column-names dataset))]
    (into (sorted-map) (map (fn [column]
                              [column (generate-graphs-for-column column (get ques/question-texts column column))])
                            columns-to-process))))

(def all-graphs (generate-all-graphs bulk-trans-data))

(defn display-all-graphs [graphs]
  [:div
   (for [[column graphs] graphs]
     [:div {:key column}
      [:h3 (str "Graphs for " column)]
      [:h5 (:subheading graphs)]
      [:div (:lgbt-chart graphs)
       (:trans-chart graphs)]
      [:div (:layered-chart graphs)]])])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(kind/hiccup
 [:div {:style {:display "flex"
                :flex-direction "column"
                :justify-content "center"
                :text-align "center"
                :margin-bottom "5vh"
                :gap "10vh"}}
  
  [:h2 "Practice Set 1 - Apples"
   [:div size-weight]
   [:div create-3d-scatter-plot]]

  [:h2 "Practice Set 2 - LGB/T Survey Data"
   (display-all-graphs all-graphs)
   [:div "Thanks for checking this out. More of my work can be seen on my website - lorelailyons.me"]]])

(comment
  (clay/make! {:format [:html] 
               :source-path "src/pottery/clj/home.clj"}))
