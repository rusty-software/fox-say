(ns fox-say.main
    (:require [reagent.core :as reagent]
              [fox-say.fox :as fox]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (reagent/atom {:game-type :no-limit :position nil :street :pre-flop :action-to-you nil :action-count nil :hand nil :deck nil
                                  :chosen-action nil :correct-action nil :result nil :hand-category nil :description nil
                                  :stats {:hand-count 0 :correct-count 0 :incorrect-count 0 :fold 0 :call 0 :raise 0
                                          :flop-correct-count 0 :flop-incorrect-count 0 :aggressive 0 :passive 0}}))

(defn set-game-type! [game-type]
  (swap! app-state assoc :game-type game-type))

(defn deal-hole! []
  (let [{:keys [position action-to-you action-count hand deck]} (fox/deal-hole)]
    (swap! app-state assoc :position position :street :pre-flop :action-to-you action-to-you :action-count action-count :hand hand :deck deck
           :chosen-action nil :correct-action nil :result nil :hand-category nil :description nil
           :showing-flop? false)))

(defn toggle [app-state key]
  (assoc app-state key (not (get app-state key))))

(defn show-action-results! []
  (let [toggled (toggle @app-state :showing-action-results?)]
    (reset! app-state toggled)))

(defn show-stats! []
  (let [toggled (toggle @app-state :showing-stats?)]
    (reset! app-state toggled)))

(defn update-stats [{:keys [stats street]} action correct-action]
  (let [inc-key (cond
                  (and (= :pre-flop street) (= action correct-action)) :correct-count
                  (and (= :pre-flop street) (not (= action correct-action))) :incorrect-count
                  (and (= :flop street) (= action correct-action)) :flop-correct-count
                  (and (= :flop street) (not (= action correct-action))) :flop-incorrect-count)]
    (-> stats
        (update :hand-count inc)
        (update correct-action inc)
        (update inc-key inc))))

(defmulti check-proper-action (fn [state action] (:street state)))

(defmethod check-proper-action :pre-flop [state action]
  (let [{:keys [correct-action description]} (fox/action-with-description state)
        flop (fox/deal-flop state)
        result (if (= action correct-action) :correct :incorrect)
        flopping? (and (= :correct result) (not (= :fold correct-action)))
        street (if flopping? :flop :pre-flop)
        updated-stats (update-stats state action correct-action)]
    (assoc state :chosen-action action :correct-action correct-action :result result
                 :description description :stats updated-stats
                 :street street :flop flop :showing-flop? flopping?)))

(defmethod check-proper-action :flop [state action]
  (let [{:keys [correct-action hand-category description]} (fox/action-with-description state)
        result (if (= action correct-action) :correct :incorrect)
        updated-stats (update-stats state action correct-action)]
    (assoc state :chosen-action action :correct-action correct-action :hand-category hand-category :result result
                 :description description :stats updated-stats)))

(defn raise! []
  (swap! app-state check-proper-action :raise))

(defn call! []
  (swap! app-state check-proper-action :call))

(defn fold! []
  (swap! app-state check-proper-action :fold))

(defn flop-aggressive! []
  (swap! app-state check-proper-action :aggressive))

(defn flop-passive! []
  (swap! app-state check-proper-action :passive))

(defn action-results-display [chosen-action correct-action result hand-category description]
  [:div
   [:hr]
   [:div (str "Chosen action: " (when chosen-action (name chosen-action)))]
   [:div (str "Proper action: " (when correct-action (name correct-action)))]
   [:div "Result: " (when result
                      (if (not= :correct result)
                        [:span {:class "incorrect"} (name result)]
                        [:span {:class "correct"} (name result)]))]
   (when hand-category
     [:div "Hand Category: " hand-category])
   [:div (if (coll? description)
           (for [d description]
             ^{:key (rand-int 1000000)}
             [:div d])
           description)]
   [:hr]])

(defn stats-display [stats]
  [:div
   [:hr]
   [:code
    (for [[k v] (into (sorted-map) stats)]
      (str k ": " v "; "))]])

(defn pre-flop-buttons []
  [:span
   [:button {:class "myButton" :on-click #(raise!)} "Raise"]
   [:button {:class "myButton" :on-click #(call!)} "Call"]
   [:button {:class "myButton" :on-click #(fold!)} "Fold"]])

(defn flop-buttons []
  [:span
   [:button {:class "myButton" :on-click #(flop-aggressive!)} "Aggressive"]
   [:button {:class "myButton" :on-click #(flop-passive!)} "Passive"]])

(defn deal-display []
  (let [{:keys [game-type showing-flop?]} @app-state]
    [:div
     [:form
      {:on-submit (fn [e]
                    (.preventDefault e))
       :on-key-press (fn [e]
                       (case (.-key e)
                         "d" (deal-hole!)
                         "r" (raise!)
                         "c" (call!)
                         "f" (fold!)
                         "a" (flop-aggressive!)
                         "p" (flop-passive!)
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
      [:label
       [:input {:type "checkbox"
                :on-click #(show-action-results!)}]
       "Show Action Results"]
      [:label
       [:input {:type "checkbox"
                :on-click #(show-stats!)}]
       "Show Stats"]
      [:br]
      [:button {:class "myButton"
                :auto-focus true
                :on-click #(deal-hole!)} "Deal"]
      (if showing-flop?
        (flop-buttons)
        (pre-flop-buttons))]]))

(defn pre-flop-display []
  (let [{:keys [position action-to-you action-count hand result]} @app-state]
    [:div
     [:div "Position: "
      [:strong (when position (name position))]]
     [:div "Action to you: "
      [:strong (when action-to-you (name action-to-you))]]
     [:div (str "Action count: " action-count)]
     [:div
      {:style {:display "flex" :align-items "center"}}
      "Hand: "
      [:div {:class (str "card card" (first hand))}]
      [:div {:class (str "card card" (second hand))}]
      (when result
        (if (= :correct result)
          [:div {:class "check-mark"}]
          [:div {:class "x-mark"}]))]]))

(defn flop-display []
  (let [{:keys [flop]} @app-state
        [f1 f2 f3] flop]
    [:div
     [:div
      {:style {:display "flex" :align-items "center"}}
      "Flop: "
      [:div {:class (str "card card" f1)}]
      [:div {:class (str "card card" f2)}]
      [:div {:class (str "card card" f3)}]
      ]]))

(defn display []
  (let [{:keys [showing-flop? showing-action-results? showing-stats?
                chosen-action correct-action result hand-category description stats]} @app-state]
    [:div
     (deal-display)
     (pre-flop-display)
     (when showing-flop?
       (flop-display))
     (when showing-action-results?
       (action-results-display chosen-action correct-action result hand-category description))
     (when showing-stats?
       (stats-display stats))]))

(reagent/render-component
  [display]
  (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
