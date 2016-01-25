(ns ode-solver.components.dashboard
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljsjs.jquery]
            [cljsjs.highcharts]
            [ode-solver.utils.inputs :as inputs]))

(defn abs [n]
  (max n (- n)))

;; Forward Euler and Euler Predict-Corrector method

(defn get-dy-dt [previous-values next-time diff-equation]
  (apply diff-equation (conj (vec previous-values) next-time)))

(defn get-next-value [timestep [previous-value dy-dt]]
  (+ previous-value (* dy-dt timestep)))

(defn correct-dy-dt [[dy-dt corrector-val]]
  (* 0.5 (+ dy-dt corrector-val)))

(defn euler-predict-next-value [diff-equations timestep previous-values next-time]
  (let [dy-dts (vec (map (partial get-dy-dt previous-values next-time) diff-equations))]
    (map (partial get-next-value timestep) (map vector previous-values dy-dts))))

(defn euler-predict-correct-next-value [diff-equations timestep previous-values next-time]
  (let [dy-dts (vec (map (partial get-dy-dt previous-values next-time) diff-equations))]
    (let [predicted-values (map (partial get-next-value timestep) (map vector previous-values dy-dts))]
      (let [corrector-vals (vec (map (partial get-dy-dt predicted-values (+ next-time timestep)) diff-equations))]
        (let [corrected-dy-dts (map correct-dy-dt (map vector dy-dts corrector-vals))]
          (map (partial get-next-value timestep) (map vector previous-values corrected-dy-dts)))))))

(defn solve-iteration [next-value-predictor diff-equations value-labels timestep
                       {:keys [all-previous-values previous-values times]} next-time]
  (let [next-values (next-value-predictor diff-equations timestep previous-values next-time)]
    {:all-previous-values (conj all-previous-values (merge (zipmap value-labels next-values) {:t next-time}))
     :previous-values next-values}))

(defn forward-euler-integrate [diff-equations start-values start-time end timestep]
  (:all-previous-values
    (reduce
      (partial solve-iteration euler-predict-next-value diff-equations (keys start-values) timestep)
      {:all-previous-values [(merge start-values {:t start-time})]
       :previous-values (vals start-values)}
      (range (+ start-time timestep) end timestep))))

(defn euler-pc [diff-equations start-values start-time end timestep]
  (:all-previous-values
    (reduce
      (partial solve-iteration euler-predict-correct-next-value diff-equations (keys start-values) timestep)
      {:all-previous-values [(merge start-values {:t start-time})]
       :previous-values (vals start-values)}
      (range (+ start-time timestep) end timestep))))

;; Now for an adaptive step-wise solution

(defn repeat-predictor [diff-equations timestep previous-values current-time iterations]
  (let [timestep (/ timestep iterations)]
    (:previous-values
      (reduce
        (fn [{:keys [previous-values current-time]} _]
          (let [next-values (euler-predict-correct-next-value diff-equations timestep previous-values current-time)]
            {:previous-values next-values
             :current-time (+ timestep current-time) }))
        {:previous-values previous-values
         :current-time current-time }
        (range iterations)))))

(defn estimate-error [diff-equations current-timestep previous-values current-time]
  (let [err0 (repeat-predictor diff-equations current-timestep previous-values current-time 1)]
    (let [err1 (repeat-predictor diff-equations current-timestep previous-values current-time 2)]
      ;;(js/console.log (clj->js err0) (clj->js err1) current-timestep)
      (reduce (fn [old [a b]] (+ old (abs (- a b)))) 0 (map vector err0 err1)))))

;; define it recursively
(defn euler-adaptive [diff-equations previous-values current-time end current-timestep tolerance]
  (if (<= end current-time )
    []
    (let [error-estimate (estimate-error diff-equations current-timestep (vals previous-values) current-time)]
      (let [new-timestep (min (* 0.9 current-timestep (/ tolerance error-estimate)) 1)]
        ;;(js/console.log error-estimate)
        ;;(js/console.log new-timestep)
        (let [next-values (euler-predict-correct-next-value diff-equations new-timestep (vals previous-values) current-time)]
          (let [labeled-values (zipmap (keys previous-values) next-values)]
            (conj
              (euler-adaptive diff-equations labeled-values (+ current-time new-timestep) end new-timestep tolerance)
              (merge labeled-values {:t (+ current-time new-timestep)}))))))))

;; Boring plots
(defn dy-dt-1 [_ _]
  1)

(defn dy-dt-t [_ t d]
  t)

(defn dy-dt-y [y _]
  y)


;; Circle
(defn circle-dy1-dt [y1 y2 t]
  y2)

(defn circle-dy2-dt [y1 y2 t]
  (- y1))

;; FitzHugh-Nagumo Model

(defn fhn-dv [alpha epsilon Iapp]
  (fn [v w t]
    (/
      (+
        (-
          (*
            (- v)
            (- v alpha)
            (- v 1))
          w)
        Iapp)
      epsilon)))

(defn fhn-dw [gamma]
  (fn [v w t]
    (- v (* gamma w))))

(defn gen-data-series [dataset]
  {:data dataset
   :lineWidth 2
   :marker {:enabled false}})

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

(defn fitzhugh-nagumo [solver {:keys [alpha epsilon gamma Iapp]}]
  (solver [(fhn-dv alpha epsilon Iapp) (fhn-dw gamma)] {:v 0.64 :w 0} 0 1 0.005))

(defn get-euclidian-distance [coord1 coord2]
  {:pre [(= (-> coord1 keys sort) (-> coord2 keys sort))]}
  (reduce
    (fn [dist-sq coord-key]
      (js/Math.pow (- (coord-key coord1) (coord-key coord2)) 2))
    0
    (keys coord1)))

(defn get-error [solver {:keys [diff-equations start-values start-time end timestep expected-values-equations]}]
  (let [data (solver diff-equations start-values start-time end timestep)]
    (js/Math.sqrt
      (/
        (reduce
          (fn [total-error next-data]
            (+ total-error
               (get-euclidian-distance
                 (dissoc next-data :t)
                 (reduce
                   (fn [expected-values k]
                     (assoc expected-values k ((k expected-values-equations) (:t next-data))))
                   {}
                   (keys expected-values-equations)))))
          0
          data)
        (/ (- end start-time) timestep)))))

(defn get-all-errors [solver {:keys [diff-equations start-values start-time end min-timestep max-timestep expected-values-equations]}]
  (reduce
    (fn [previous-data-points next-timestep]
      (conj previous-data-points {:timestep next-timestep
                                  :error (get-error
                                           solver
                                           {:diff-equations diff-equations
                                            :start-values start-values
                                            :start-time start-time
                                            :end end
                                            :timestep next-timestep
                                            :expected-values-equations expected-values-equations})}))
    []
    (range min-timestep max-timestep (/ (- max-timestep min-timestep) 50))))

(defn compare-solvers [[solver-1 solver-2] options]
  (let [errors-1 (get-all-errors solver-1 options)
        errors-2 (get-all-errors solver-2 options)]
    [errors-1 errors-2]))

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
          ;(with-out-str (print (compare-solvers
          ;[forward-euler-integrate euler-pc]
          ;{:diff-equations [circle-dy1-dt circle-dy2-dt]
          ;:start-values {:y1 1 :y2 0}
          ;:start-time 0
          ;:end 6.0
          ;:min-timestep 0.01
          ;:max-timestep 0.1
          ;:expected-values-equations {:y1 js/Math.cos
          ;:y2 js/Math.sin}})))
          (om/build highcharts [["error" "timestep"]
                                (compare-solvers
                                  [forward-euler-integrate euler-pc]
                                  {:diff-equations [circle-dy1-dt circle-dy2-dt]
                                   :start-values {:y1 1 :y2 0}
                                   :start-time 0
                                   :end 6.0
                                   :min-timestep 0.001
                                   :max-timestep 1.0
                                   :expected-values-equations {:y1 #(js/Math.cos (- %))
                                                               :y2 #(js/Math.sin (- %))}})
                                "logarithmic"
                                "Error vs Timestep; solving circles with different solvers"])

          (om/build highcharts [["y" "t"] [(forward-euler-integrate [dy-dt-1] {:y 0} 0 10 0.1)] "linear" "dy/dt = 1"])
          ;(om/build highcharts [["y" "t"] (forward-euler-integrate [dy-dt-t] {:y 0} 0 10 0.01) "dy/dt = t"])
          ;(om/build highcharts [["y" "t"] (forward-euler-integrate [dy-dt-y] {:y 1} 0 10 0.01) "dy/dt = y"])
          (om/build highcharts [["y1" "y2"]
                                [(forward-euler-integrate [circle-dy1-dt circle-dy2-dt] {:y1 1 :y2 0} 0 10 0.5)]
                                "linear"
                                "Circle, timestep of 0.5"])
          (om/build highcharts [["y1" "y2"]
                                [(forward-euler-integrate [circle-dy1-dt circle-dy2-dt] {:y1 1 :y2 0} 0 100 0.1)]
                                "linear"
                                "Cricle, timestep of 0.1"])
          (om/build highcharts [["y1" "y2"]
                                [(forward-euler-integrate [circle-dy1-dt circle-dy2-dt] {:y1 5 :y2 0} 0 6.3 0.01)]
                                "linear"
                                "Circle, timestep of 0.01"])
          (om/build highcharts [["y1" "y2"]
                                [(forward-euler-integrate [circle-dy1-dt circle-dy2-dt] {:y1 10 :y2 0} 0 6.3 0.1)]
                                "linear"
                                "Forward Euler: 0.1"])
          (om/build highcharts [["y1" "y2"]
                                [(euler-pc [circle-dy1-dt circle-dy2-dt] {:y1 10 :y2 0} 0 6.3 0.1)]
                                "linear"
                                "Euler predictor-corrector timestep of 0.1"])
          (om/build highcharts [["y1" "y2"]
                                [(euler-pc [circle-dy1-dt circle-dy2-dt] {:y1 10 :y2 0} 0 6.3 0.5)]
                                "linear"
                                "Euler predictor-corrector timestep of 0.5"])
          (om/build highcharts [["y1" "y2"] [(euler-pc [circle-dy1-dt circle-dy2-dt] {:y1 10 :y2 0} 0 100 0.1)]
                                "linear"
                                "Euler predictor-corrector timestep of 0.1 t 0 - 100"])
          (om/build highcharts [["y1" "y2"] [(euler-adaptive [circle-dy1-dt circle-dy2-dt] {:y1 10 :y2 0} 0 6.3 0.2 0.5)]
                                "linear"
                                "Euler adaptive init-timestep of 0.1, tolerance = 0.5 t 0 - 100"])
          (om/build highcharts [["v" "t"]
                                [(fitzhugh-nagumo forward-euler-integrate {:alpha 0.2 :epsilon 0.01 :gamma 0.5 :Iapp 0.0})]
                                "linear"
                                "Forward Euler FitzHugh-Nagumo model timestep 0.1"])
          (om/build highcharts [["v" "t"]
                                [(fitzhugh-nagumo euler-pc {:alpha 0.2 :epsilon 0.01 :gamma 0.5 :Iapp 0.0})]
                                "linear"
                                "Euler-pc FitzHugh-Nagumo model timestep 0.1"]))))))
