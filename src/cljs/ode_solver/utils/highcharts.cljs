(ns ode-solver.utils.highcharts
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljsjs.jquery]
            [cljsjs.highcharts]))

(defn gen-data-series [dataset]
  {:data dataset
   :lineWidth 2
   :marker {:enabled false}})

(defn label-dataset [[y-label x-label] dataset]
  (let [x-label (keyword x-label)
        y-label (keyword y-label)]
    (map
      (fn [data-point]
        {:x (x-label data-point)
         :y (y-label data-point)})
      dataset)))

(defn label-all [datasets labels]
  (map
    (partial label-dataset labels)
    datasets))

(defn create-chart [container datasets y-axis-type title]
  (js/Highcharts.Chart.
    (clj->js
      {:chart {:renderTo container
               :type "scatter"}
       :plotOptions {:series {:turboThreshold 0}}
       :title {:text title}
       :xAxis {:type "linear"}
       :yAxis {:type y-axis-type}
       :series (map gen-data-series datasets)})))

(defn highcharts [[labels datasets y-axis-type title] owner]
  (reify
    om/IRender
    (render [this]
      (dom/div #js {:className "ui segment"}
               (dom/div #js {:className "container"})))

    om/IInitState
    (init-state [_]
      {:chart nil})

    om/IDidMount
    (did-mount [_]
      (let [container (-> (om/get-node owner) js/$. (.find ".container") (.get 0))
            datasets (label-all datasets labels)]
        (om/set-state! owner :chart (create-chart container datasets y-axis-type title))))))
