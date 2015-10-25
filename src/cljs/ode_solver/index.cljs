(ns ode-solver.index
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljsjs.jquery]
            [ode-solver.components.dashboard :as dashboard]))

(defn index-view [state owner]
  (reify
    om/IRenderState
    (render-state [this _]
      (case (@state :route)
        "home" (om/build dashboard/dashboard-view (:ode state))
        (om/build dashboard/dashboard-view (:ode state))))))
