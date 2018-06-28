(ns fox-say.main
    (:require [reagent.core :as reagent]
              [fox-say.fox :as fox]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (reagent/atom {}))

(defn deal! []
  (let [{:keys [position action-to-you hand]} (fox/deal)]
    (swap! app-state assoc :position position :action-to-you action-to-you :hand hand :result nil)))

(defn check-proper-action [state action]
  (let [action-description (fox/action-with-description @app-state)
        result (if (= action (:action action-description)) :correct :incorrect)]
    (assoc state :result result :description (:description action-description))))

(defn raise! []
  (swap! app-state check-proper-action :raise))

(defn call! []
  (swap! app-state check-proper-action :call))

(defn fold! []
  (swap! app-state check-proper-action :fold))

(defn display []
  [:div
   [:button {:on-click #(deal!)} "Deal"]
   (let [{:keys [position action-to-you hand result description]} @app-state]
     [:div
      [:div (str "Position: " position)]
      [:div (str "Action to you: " action-to-you)]
      [:div (str "Hand: " hand)]
      [:button {:on-click #(raise!)} "Raise"]
      [:button {:on-click #(call!)} "Call"]
      [:button {:on-click #(fold!)} "Fold"]
      [:div (str "Result: " result)]
      [:div description]])
   ]
  )

(reagent/render-component
  [display]
  (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
