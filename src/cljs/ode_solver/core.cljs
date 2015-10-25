(ns ode-solver.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljsjs.jquery]
            [secretary.core :as secretary :include-macros true :refer-macros [defroute]]
            [ode-solver.router :as router]
            [ode-solver.index :as index]))

(enable-console-print!)

(defonce app-state (atom {:route nil
                          :ode {:equations [1,2,3,4]}
                          }))

(defn main []
  (router/route-app app-state)
  (secretary/dispatch!
    (.substring (.. js/window -location -hash) 1))
  (om/root
    index/index-view
    app-state
    {:target (js/document.getElementById "app")}))
