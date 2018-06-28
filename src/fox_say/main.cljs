(ns fox-say.main
    (:require [reagent.core :as reagent]
              [fox-say.fox :as fox]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (reagent/atom {}))

(defn deal! []
  (let [{:keys [position action-to-you hand]} (fox/deal)]
    (swap! app-state assoc :position position :action-to-you action-to-you :hand hand
           :chosen-action nil :correct-action nil :result nil :description nil)))

(defn check-proper-action [state action]
  (let [{:keys [correct-action description]} (fox/action-with-description @app-state)
        result (if (= action correct-action) :correct :incorrect)]
    (assoc state :chosen-action action :correct-action correct-action :result result :description description)))

(defn raise! []
  (swap! app-state check-proper-action :raise))

(defn call! []
  (swap! app-state check-proper-action :call))

(defn fold! []
  (swap! app-state check-proper-action :fold))

(defn display []
  [:div
   [:button {:on-click #(deal!)} "Deal"]
   (let [{:keys [position action-to-you hand chosen-action correct-action result description]} @app-state]
     [:div
      [:div (str "Position: " position)]
      [:div (str "Action to you: " action-to-you)]
      [:div (str "Hand: " hand)]
      [:button {:on-click #(raise!)} "Raise"]
      [:button {:on-click #(call!)} "Call"]
      [:button {:on-click #(fold!)} "Fold"]
      [:div (str "Chosen action: " chosen-action)]
      [:div (str "Proper action: " correct-action)]
      [:div (str "Result: " result)]
      (for [d description]
        (do
          ^{:key (rand-int 1000000)}
          [:div d]))])
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
