(ns ode-solver.components.comparison
  (:require [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [ode-solver.utils.http :as http]
            [ode-solver.utils.inputs :as inputs]
            [ode-solver.utils.highcharts :as highcharts]))

(defn format-as-numbers [data]
  (vec (map #(into {} (map (fn [[k v]] [k (js/parseFloat v)]) %)) data)))

(defn update-data-options! [app-state]
  (http/get-data-files
    (fn [data-files]
      (om/update! app-state :options (:data_files data-files)))))

(defn graph-file! [app-state data-file-name]
  (http/get-data
    data-file-name
    (fn [data]
      (js/console.log (-> data :data format-as-numbers))
      (om/update! app-state :graph-data (-> data :data format-as-numbers)))))

(defn graph-view [app-state owner]
  (reify
    om/IRender
    (render [_]
      (html [:div {:class "ui segment"}
             "Your graph"
             (om/build highcharts/highcharts [["y1" "t"]
                                              [(:graph-data app-state)]
                                              "linear"
                                              (:data-file app-state)])]))))

(defn select-file-view [app-state owner]
  (reify
    om/IRender
    (render [this]
      (html [:div {:class "ui segment"}
             [:h2 {:class "header"} "Select file"]
             (om/build inputs/option-input [app-state
                                            {:edit-key :data-file
                                             :placeholder-text "Filename"
                                             :values (:options app-state)}])
             [:button {:class "ui blue basic button"
                       :on-click #(graph-file! app-state (:data-file app-state))}
              "Graph me"]
             [:button {:class "ui green basic icon button"
                       :on-click #(update-data-options! app-state)}
              [:i {:class "refresh icon"}]] ]))))

(defn comparison-view[app-state owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (update-data-options! app-state))

    om/IRender
    (render [this]
      (html [:div {:class "dashboard-panel"}
             [:div {:class "dashboard-welcome"}
              [:h1 "ODE Solver in Clojurescript"] ]
             [:div {:class "dashboard-content"}
              (om/build select-file-view app-state)
              (om/build graph-view app-state)]]))))
