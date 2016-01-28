(ns ode-solver.router
  (:require [secretary.core :as secretary :include-macros true :refer-macros [defroute]]
            [goog.events :as events]
            [goog.history.EventType :as EventType])
  (:import goog.History))

(defn route-app [app-state]
  (defroute
    "/" []
    (swap! app-state assoc :route "home"))

  (defroute
    "/comparison" []
    (swap! app-state assoc :route "comparison")))

; enable fallback that don't have HTML 5 History
(secretary/set-config! :prefix "#")

; Quick and dirty history configuration.
(let [h (History.)]
  (goog.events/listen h EventType/NAVIGATE #(secretary/dispatch! (.-token %)))
  (doto h (.setEnabled true)))
