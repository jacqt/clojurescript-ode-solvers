(ns ode-solver.utils.http
  (:require [cljs.core.async :refer [put! chan <!]]
            [goog.uri.utils :as uri-utils]
            [goog.net.XhrIo :as net-xhrio]))

(enable-console-print!)

(def BASE_URL "http://localhost:5000/")
(def DATA_URL (str BASE_URL "data_files"))

; parses goog.net.XhrIo response to a json
(defn parse-xhrio-response [response-channel success-callback fail-callback]
  (fn [response]
    (let [target (aget response "target")]
      (if (.isSuccess target)
        (let [json (.getResponseJson target)]
          (put! response-channel (js->clj json :keywordize-keys true))
          (success-callback (js->clj json :keywordize-keys true)))
        (let [error (.getLastError target)]
          (put! response-channel (js->clj error :keywordize-keys true))
          (fail-callback (js->clj error :keywordize-keys true)))))))

; wraps goog.net.XhrIo library in a simpler function xhr
(defn xhr [{:keys [method base-url url-params on-complete on-error]}]
  (let [response-channel (chan)]
    (.send
      goog.net.XhrIo
      (reduce
        (fn [partial-url param-key]
          (.appendParams
            goog.uri.utils
            partial-url
            (name param-key)
            (url-params param-key)))
        base-url
        (keys url-params))
      (parse-xhrio-response response-channel on-complete on-error)
      method)
    response-channel))

(defn get-data-files [on-complete]
  (xhr {:method "GET"
        :base-url DATA_URL
        :url-params {}
        :on-complete on-complete
        :on-error #()}))

(defn get-data [filename on-complete]
  (xhr {:method "GET"
        :base-url (str DATA_URL "/" filename)
        :url-params {}
        :on-complete on-complete
        :on-error #()}))
