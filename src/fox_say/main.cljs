(ns fox-say.main
    (:require [reagent.core :as reagent]
              [fox-say.fox :as fox]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (reagent/atom {:game-type :no-limit :position nil :action-to-you nil :action-count nil :hand nil
                                  :chosen-action nil :correct-action nil :result nil :description nil
                                  :stats {:hand-count 0 :correct-count 0 :incorrect-count 0 :fold 0 :call 0 :raise 0}}))

(defn set-game-type! [game-type]
  (swap! app-state assoc :game-type game-type))

(defn deal-hole! []
  (let [{:keys [position action-to-you action-count hand]} (fox/deal-hole)]
    (swap! app-state assoc :position position :action-to-you action-to-you :action-count action-count :hand hand
           :chosen-action nil :correct-action nil :result nil :description nil)))

(defn toggle [app-state key]
  (assoc app-state key (not (get app-state key))))

(defn show-pre-flop-results! []
  (let [toggled (toggle @app-state :showing-pre-flop-results)]
    (reset! app-state toggled)))

(defn show-stats! []
  (let [toggled (toggle @app-state :showing-stats)]
    (reset! app-state toggled)))

(defn update-stats [{:keys [stats]} action correct-action]
  (if (= action correct-action)
    (-> stats
        (update :hand-count inc)
        (update correct-action inc)
        (update :correct-count inc))
    (-> stats
        (update :hand-count inc)
        (update correct-action inc)
        (update :incorrect-count inc))))

(defn check-proper-action [state action]
  (let [{:keys [correct-action description] :as r} (fox/action-with-description @app-state)
        result (if (= action correct-action) :correct :incorrect)
        updated-stats (update-stats state action correct-action)]
    (assoc state :chosen-action action :correct-action correct-action :result result :description description :stats updated-stats)))

(defn raise! []
  (swap! app-state check-proper-action :raise))

(defn call! []
  (swap! app-state check-proper-action :call))

(defn fold! []
  (swap! app-state check-proper-action :fold))

(defn pre-flop-results-display [chosen-action correct-action result description]
  [:div
   [:hr]
   [:div (str "Chosen action: " (when chosen-action (name chosen-action)))]
   [:div (str "Proper action: " (when correct-action (name correct-action)))]

   [:div "Result: " (when result
                      (if (not= :correct result)
                        [:span {:class "incorrect"} (name result)]
                        [:span {:class "correct"} (name result)]))]
   [:div description]])

(defn stats-display [stats]
  [:div
   [:hr]
   [:code
    (for [[k v] stats]
      (str k ": " v "; "))]])

(defn pre-flop-display []
  (let [{:keys [game-type position action-to-you action-count hand
                chosen-action correct-action result description stats
                showing-pre-flop-results showing-stats]} @app-state]
    [:div
     [:h2 "Pre Flop Trainer"]
     [:form
      {:on-submit (fn [e]
                    (.preventDefault e))
       :on-key-press (fn [e]
                       (case (.-key e)
                         "d" (deal-hole!)
                         "r" (raise!)
                         "c" (call!)
                         "f" (fold!)
                         "pressed something i don't care about"))}
      [:label
       [:input {:type "radio"
                :name "game-type"
                :value :no-limit
                :checked (= :no-limit game-type)
                :on-change #(set-game-type! :no-limit)}]
       "No Limit"]
      [:label
       [:input {:type "radio"
                :name "game-type"
                :value :low-limit
                :checked (= :low-limit game-type)
                :disabled true
                :on-change #(set-game-type! :low-limit)}]
       "Low Limit"]
      [:br]
      [:button {:class "myButton"
                :auto-focus true
                :on-click #(deal-hole!)} "Deal"]
      [:label
       [:input {:type "checkbox"
                :on-click #(show-pre-flop-results!)}]
       "Show Pre-Flop Results"]
      [:label
       [:input {:type "checkbox"
                :on-click #(show-stats!)}]
       "Show Stats"]

      [:div "Position: "
       [:strong (when position (name position))]]
      [:div "Action to you: "
       [:strong (when action-to-you (name action-to-you))]]
      [:div (str "Action count: " action-count)]
      [:div "Hand: "
       [:div {:class (str "card card" (first hand))}]
       [:div {:class (str "card card" (second hand))}]
       (when result
         (if (= :correct result)
           [:div {:class "check-mark"}]
           [:div {:class "x-mark"}]))]
      [:button {:class "myButton" :on-click #(raise!)} "Raise"]
      [:button {:class "myButton" :on-click #(call!)} "Call"]
      [:button {:class "myButton" :on-click #(fold!)} "Fold"]
      (when showing-pre-flop-results
        (pre-flop-results-display chosen-action correct-action result description))
      (when showing-stats
        (stats-display stats))]]))

(defn display []
  [:div
   (pre-flop-display)])

(reagent/render-component
  [display]
  (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
