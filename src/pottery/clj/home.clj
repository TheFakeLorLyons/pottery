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

(defn safe-parse-int [x]
  (try
    (Integer/parseInt (str x))
    (catch Exception _
      nil)))

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

(defn valid-trans-sample? [row]
  (and #(valid-score? (get % "q01"))
       (= (get row "trans_sample") 1)))

(def get-superset
  (memoize (fn [column pred]
             (-> bulk-trans-data
                 (tc/select-rows pred)
                 (tc/group-by column)
                 (tc/aggregate {"count" tc/row-count})
                 (tc/rename-columns {column "score"})
                 (tc/order-by [:score])
                 (tc/rows :as-maps)))))

(defn get-LGBT-stats [column]
  (get-superset column #(valid-score? (get % "q01"))))

(defn get-trans-stats [column]
  (get-superset column valid-trans-sample?))

(defn convert-to-percentage [processed-columns]
  (let [total-count (reduce + (map #(get % "count") processed-columns))]
    (map #(assoc % "percentage" (/ (get % "count") total-count)) processed-columns)))

(defn get-LGBT-percentages [column]
  (let [column-data (get-LGBT-stats column)]
    (convert-to-percentage column-data)))

(defn get-trans-percentages [column]
  (let [column-data (get-trans-stats column)]
    (convert-to-percentage column-data)))

(defn get-trans-stats-and-percentages [column]
  (let [trans-stats (get-trans-stats column)
        trans-percentages (get-trans-percentages column)]
    {:stats trans-stats
     :percentages trans-percentages}))

(get-trans-stats-and-percentages "q01")


(defn convert-to-individual-maps [data]
  (->> data
       (map (fn [item]
              {"score" (:$group-name item)
               "count" (get item "count")}))
       (filter (fn [item] (valid-score? (get item "score"))))
       (sort-by #(get % "score"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn individual-chart [data color title responses column-name is-percent]
  (let [total-count (reduce + (map #(get % "count") data))
        data-to-use (if is-percent
                      (map #(assoc % "count" (* 100 (/ (get % "count") total-count))) data)
                      data)]
    (kind/vega
     (hc/xform
      ht/bar-chart
      :DATA data-to-use
      :X "response"
      :XSCALE {:title (str column-name " Responses")}
      :XTYPE "ordinal"
      :COLOR {:value color}
      :Y "count"
      :YTITLE (if is-percent "Percentage" "Count")
      :TITLE {:text title
              :frame "group"
              :align "center"
              :wrap "enabled"}
      :XSORT responses
      :AXIS {:labelAngle 45}))))

(defn layered-chart [lgbt-items-mapped trans-items-mapped responses column-name is-percent]
  (let [combined-data (concat
                       (map #(assoc % "group" "LGBT Population") lgbt-items-mapped)
                       (map #(assoc % "group" "Trans Sample") trans-items-mapped))
        total-count (if is-percent
                      (reduce + (map #(get % "count") lgbt-items-mapped))  ; Calculate separate totals
                      1)
        trans-total (if is-percent
                      (reduce + (map #(get % "count") trans-items-mapped))
                      1)
        data-to-use (if is-percent
                      (concat
                       (map #(assoc % "count" (* 100 (/ (get % "count") total-count)))
                            (filter #(= (get % "group") "LGBT Population") combined-data))
                       (map #(assoc % "count" (* 100 (/ (get % "count") trans-total)))
                            (filter #(= (get % "group") "Trans Sample") combined-data)))
                      combined-data)]
    (kind/vega-lite
     {:data {:values data-to-use}
      :width 600
      :height 400
      :encoding {:x {:field "response"
                     :type "ordinal"
                     :axis {:title (str column-name " Responses")
                            :labelAngle 45}
                     :sort responses}
                 :y {:field "count"
                     :type "quantitative"
                     :axis {:title (if is-percent "Percentage" "Count")}
                     :stack nil}
                 :color {:field "group"
                         :type "nominal"
                         :scale {:range ["lightblue" "pink"]}}}
      :title {:text (str column-name " - Compared" (if is-percent " (%)" ""))
              :fontSize 18}
      :layer [{:mark {:type "bar" :opacity 0.7}
               :encoding {:x {:field "response"
                              :type "ordinal"}
                          :y {:field "count"
                              :type "quantitative"}
                          :color {:field "group"
                                  :type "nominal"
                                  :scale {:range ["lightblue" "pink"]}}
                          :text {:field "count"
                                 :format ".1f"}}}
              {:mark {:type "text"
                      :align "center"
                      :baseline "middle"
                      :dy -5}}]})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generate-graphs-for-column [column-name question-text]
  (let [question-info (some #(when (= (:qid %) column-name) %) ques/question-texts)
        responses (get question-info :responses)
        response-index (zipmap (range 1 (inc (count responses))) responses)
        lgbt-items (convert-to-individual-maps (get-LGBT-stats column-name))
        trans-items (convert-to-individual-maps (get-trans-stats column-name))
        lgbt-percent (convert-to-individual-maps (get-LGBT-percentages column-name))
        trans-percent (convert-to-individual-maps (get-trans-percentages column-name))
        map-response (fn [item]
                       (let [score (get item "score")
                             score-int (if (number? score) score (Integer/parseInt (str score)))
                             response (get response-index score-int)]
                         (assoc item
                                "response" (or response "Unknown")
                                "order" score-int)))
        lgbt-items-mapped (map map-response lgbt-items)
        trans-items-mapped (map map-response trans-items)
        lgbt-percent-mapped (map map-response lgbt-percent)
        trans-percent-mapped (map map-response trans-percent)

        lgbt-chart (individual-chart lgbt-items-mapped "lightblue" (str column-name " - LGBT Population") responses lgbt-items-mapped false)
        trans-chart (individual-chart trans-items-mapped "pink" (str column-name " - Transgender Sample") responses trans-items-mapped false)
        layered-stats-chart (layered-chart lgbt-items-mapped trans-items-mapped responses (str column-name " - Compared") false)
        lgbt-per-chart (individual-chart lgbt-percent-mapped "lightblue" (str column-name " - LGBT Population (%)") responses lgbt-percent-mapped true)
        trans-per-chart (individual-chart trans-percent-mapped "pink" (str column-name " - Transgender Sample (%)") responses trans-percent-mapped true)
        percent-layered (layered-chart lgbt-percent-mapped trans-percent-mapped responses (str column-name " - Compared (%)") true)]
    {:subheading (str column-name " - " (get question-info :label))
     :lgbt-chart lgbt-chart
     :trans-chart trans-chart
     :layered-stats-chart layered-stats-chart
     :lgbt-percent-chart lgbt-per-chart
     :trans-percent-chart trans-per-chart
     :percent-layered percent-layered}))

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
      [:div (:layered-stats-chart graphs)]
      [:div (:lgbt-percent-chart graphs)
       (:trans-percent-chart graphs)]
      [:div (:percent-layered graphs)]])])

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
