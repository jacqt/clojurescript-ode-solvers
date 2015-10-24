(ns ode-solver.components.dashboard
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! chan <!]]
            [cljsjs.plottable]
            [cljsjs.jquery]
            [ode-solver.utils.inputs :as inputs]))

(defn forward-euler-integrate [diff-equations start-values start-time end timestep]
  (:all-previous-values 
    (reduce
      (fn [{:keys [all-previous-values times previous-values]} next-time]
        (let [dy-dts (vec (map (fn [diff-equation]
                                 (apply diff-equation (conj (vec previous-values) next-time))) diff-equations))]
          (let [next-values (map (fn [[previous-value dy-dt]] 
                                   (+ previous-value (* dy-dt timestep))) (vec (map vector previous-values dy-dts)))] 
            {:all-previous-values (conj all-previous-values (merge (zipmap (keys start-values) next-values) {:t next-time}))
             :times (conj times next-time)
             :previous-values next-values})))
      {:all-previous-values [(merge start-values {:t start-time})]
       :previous-values (vals start-values)}
      (range (+ start-time timestep) end timestep))))


(defn dy-dt-1 [_ _]
  1)

(defn dy-dt-t [_ t d]
  t)

(defn dy-dt-y [y _]
  y)


; Circle
(defn circle-dy1-dt [y1 y2 t]
  y2)

(defn circle-dy2-dt [y1 y2 t]
  (- y1))

(defn plotter [[labels dataset] owner]
  (reify
    om/IInitState
    (init-state [_]
      {:subscriptions nil :chart nil})

    om/IDidMount
    (did-mount [_]
      (.renderTo (om/get-state owner :chart) (-> (om/get-node owner) js/$. (.find "svg.ode-graph") (.get 0))))

    om/IWillMount
    (will-mount [_]
      (let [plot (js/Plottable.Plots.Line. )
            xScale (js/Plottable.Scales.Linear.)
            yScale (js/Plottable.Scales.Linear.)]
        (let [xAxis (js/Plottable.Axes.Numeric. xScale "bottom")
              yAxis (js/Plottable.Axes.Numeric. yScale "left")
              panZoomInteraction (js/Plottable.Interactions.PanZoom. xScale nil)]
          (.x plot #(aget %1 (get labels 1)) xScale)
          (.y plot #(aget %1 (get labels 0)) yScale)
          (.attachTo panZoomInteraction plot)
          (.addDataset
            plot
            (js/Plottable.Dataset. (clj->js dataset)))     
          (om/set-state! owner :plot plot)
          (om/set-state! owner :chart (js/Plottable.Components.Table. (clj->js [[yAxis plot] [nil xAxis]]))))))

    om/IRenderState
    (render-state [this state]
      (dom/div
        #js {:className "ui segment"}
        (dom/h2 nil "Bro.")
        (dom/div
          #js {}
          (dom/h3 nil "A graph of your ODE solver!")
          (dom/svg #js {:className "ode-graph"}))))

    om/IWillUpdate
    (will-update [_ next-props _]
      (.datasets
        (om/get-state owner :plot)
        #js [(js/Plottable.Dataset. (clj->js dataset))]))))

(defn dashboard-view[app-state owner]
  (reify
    om/IRenderState
    (render-state [this _]
      (dom/div
        #js {:className "dashboard-panel"}
        (dom/div
          #js {:className "dashboard-welcome"}
          (dom/h1
            #js {}
            "ODE Solver in Clojurescript"))
        (dom/div
          #js {:className "dashboard-content"}
          (om/build plotter [["y" "t"] (forward-euler-integrate [dy-dt-1] {:y 0} 0 10 0.01)])
          (om/build plotter [["y" "t"] (forward-euler-integrate [dy-dt-t] {:y 0} 0 10 0.01)])
          (om/build plotter [["y" "t"] (forward-euler-integrate [dy-dt-y] {:y 1} 0 10 0.01)])
          (om/build plotter [["y1" "y2"] (forward-euler-integrate [circle-dy1-dt circle-dy2-dt] {:y1 1 :y2 0} 0 10 0.01)])
          (om/build plotter [["y1" "y2"] (forward-euler-integrate [circle-dy1-dt circle-dy2-dt] {:y1 1 :y2 0} 0 10 0.1)])
          (om/build plotter [["y1" "y2"] (forward-euler-integrate [circle-dy1-dt circle-dy2-dt] {:y1 1 :y2 0} 0 100 0.1)])
          (with-out-str (print (forward-euler-integrate [circle-dy1-dt circle-dy2-dt] {:y1 1 :y2 0} 0 10 1)))
          )))))


