(ns ode-solver.utils.inputs
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljsjs.jquery]
            [exicon.semantic-ui]))

;;; These components require jquery, jquery-ui, bootstrap, and bootstrap-timepicker

;; Generic component for an input box with a two-way databinding
;; between a property of the state and the value of the input
;;
;; Works by passing into the component a vector of length two
;; with the first being a cursor to the parent of the input, and the second
;; being a map with the keys
;;  :className        (maps to the classname on the input created)
;;  :edit-key         (the key in the parent-state to the text)
;;  :placeholder-text (the placeholder text in the creatd input)

(defn handle-change [e data edit-key]
  (om/transact! data edit-key (fn [_] (.. e -target -value))))

(defn editable-input [[parent-state {:keys [className edit-key placeholder-text]}]  owner]
  {:pre [(some? parent-state)]}
  (reify
    om/IRenderState
    (render-state [_ _]
      (dom/input
        #js {:className className
             :placeholder placeholder-text
             :onChange #(handle-change % parent-state edit-key)
             :value (edit-key parent-state)
             :type "text" }))))

(defn option-input [[parent-state {:keys [className edit-key values placeholder-text]}]  owner]
  {:pre [(some? parent-state)]}
  (reify
    om/IDidMount
    (did-mount [_]
      (js/console.log (edit-key parent-state))
      (->
        (js/$. (om/get-node owner))
        (.dropdown
          #js {:onChange (fn [value text]
                           (om/transact!
                             parent-state
                             edit-key
                             (fn [_] value)))})
        (.dropdown
          "set selected" (edit-key parent-state))))

    om/IRenderState
    (render-state [_ _]
      (dom/div
        #js {:className "ui selection dropdown"}
        (dom/input #js {:type "hidden"})
        (dom/i #js {:className "dropdown icon"})
        (dom/div #js {:className "default text"} placeholder-text)
        (dom/div #js {:className "menu"}
                 (map #(dom/div #js {:className "item"
                                     :data-value %} %) values))))))
