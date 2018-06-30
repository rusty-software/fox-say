(ns fox-say.main
    (:require [reagent.core :as reagent]
              [fox-say.fox :as fox]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (reagent/atom {:position nil :action-to-you nil :hand nil
                                  :chosen-action nil :correct-action nil :result nil :description nil
                                  :stats {:hand-count 0 :correct-count 0 :incorrect-count 0 :correct-hands [] :incorrect-hands []}}))

(defn deal! []
  (let [{:keys [position action-to-you hand]} (fox/deal)]
    (swap! app-state assoc :position position :action-to-you action-to-you :hand hand
           :chosen-action nil :correct-action nil :result nil :description nil)))

(defn update-stats [{:keys [position action-to-you hand stats]} action correct-action]
  (if (= action correct-action)
    (-> stats
        (update :hand-count inc)
        (update :correct-count inc)
        (assoc :correct-hands (conj (:correct-hands stats) {:position position :action-to-you action-to-you :hand hand :chosen-action action})))
    (-> stats
        (update :hand-count inc)
        (update :incorrect-count inc)
        (assoc :incorrect-hands (conj (:incorrect-hands stats) {:position position :action-to-you action-to-you :hand hand :chosen-action action :correct-action correct-action})))))

(defn check-proper-action [state action]
  (let [{:keys [correct-action description]} (fox/action-with-description @app-state)
        result (if (= action correct-action) :correct :incorrect)
        updated-stats (update-stats state action correct-action)]
    (assoc state :chosen-action action :correct-action correct-action :result result :description description :stats updated-stats)))

(defn raise! []
  (swap! app-state check-proper-action :raise))

(defn call! []
  (swap! app-state check-proper-action :call))

(defn fold! []
  (swap! app-state check-proper-action :fold))

(defn display []
  [:div
   [:button {:class "myButton"
             :on-click #(deal!)} "Deal"]
   (let [{:keys [position action-to-you hand chosen-action correct-action result description stats]} @app-state]
     [:div
      [:div (str "Position: " position)]
      [:div (str "Action to you: " action-to-you)]
      [:div "Hand: "
       [:div {:class (str "card card" (first hand))}]
       [:div {:class (str "card card" (second hand))}]
       ]
      [:button {:class "myButton" :on-click #(raise!)} "Raise"]
      [:button {:class "myButton" :on-click #(call!)} "Call"]
      [:button {:class "myButton" :on-click #(fold!)} "Fold"]
      [:hr]
      [:div (str "Chosen action: " chosen-action)]
      [:div (str "Proper action: " correct-action)]
      [:div (str "Result: " result)]
      (for [d description]
        (do
          ^{:key (rand-int 1000000)}
          [:div d]))
      [:pre (with-out-str (cljs.pprint/pprint (dissoc stats :correct-hands :incorrect-hands)))]])
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
