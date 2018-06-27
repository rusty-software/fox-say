(ns fox-say.fox)

(defn rank [card]
  (let [[r _] card
        upper-ranks {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get upper-ranks r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(def proper-action
  {:utg {:called {:raise [{:pair 14} {:pair 13} {:pair 12} {:pair 11} {:pair 10}
                          {:suited [14 13]} {:suited [14 12]}
                          {:unsuited [14 13]} {:unsuited [14 12]}]
                  :call []}
         :raised {:raise [{:pair 14} {:pair 13} {:pair 12} {:pair 11}
                          {:suited [14 13]}
                          {:unsuited [14 13]}]
                  :call []}}
   :blind {:called {:raise [{:pair 14} {:pair 13} {:pair 12} {:pair 11} {:pair 10} {:pair 9} {:pair 8} {:pair 7}
                            {:suited [14 13]} {:suited [14 12]} {:suited [14 11]}
                            {:unsuited [14 13]} {:unsuited [14 12]} {:unsuited [14 11]}]
                    :call [{:suited-connector :any}
                           {:suited [14 10]} {:suited [13 12]} {:suited [13 11]} {:suited [13 10]} {:suited [12 11]} {:suited [12 10]} {:suited [11 10]}]}
           :raised {:raise [{:pair 14} {:pair 13} {:pair 12}
                            {:suited [14 13]}
                            {:unsuited [14 13]}]
                    :call [{:pair 11} {:pair 10} {:pair 9} {:pair 8} {:pair 7} {:pair 6}
                           {:suited-connector :any}
                           {:suited-one-gap :any}]}}})

(defn pair? [hand]
  (apply = (map rank hand)))

(defn suited? [hand]
  (apply = (map suit hand)))

(defn actionable-pair? [action-with hand]
  (let [actionable-pair-ranks (map :pair action-with)]
    (and (pair? hand)
         (some #(= (rank (first hand)) %) actionable-pair-ranks))))

(defn hand-ranks [hand]
  (sort > (map rank hand)))

(defn suited-connector? [hand ranks]
  (and (suited? hand)
       (= (first ranks) (inc (second ranks)))))

(defn suited-rank-match [hand ranks suited-ranks]
  (and (suited? hand) (some #(= ranks %) suited-ranks)))

(defn unsuited-rank-match [ranks unsuited-ranks]
  (some #(= ranks %) unsuited-ranks))

(defn suited-connector-match [hand ranks suited-connectors]
  (and (suited-connector? hand ranks)
       (or (= :any (first suited-connectors))
           (some #(= ranks %) suited-connectors))))

(defn actionable-non-pair? [action-with hand]
  (let [suited-ranks (map :suited action-with)
        unsuited-ranks (map :unsuited action-with)
        suited-connectors (filter #(not (nil? %)) (map :suited-connector action-with))
        ranks (hand-ranks hand)]
    (or
      (suited-rank-match hand ranks suited-ranks)
      (unsuited-rank-match ranks unsuited-ranks)
      (suited-connector-match hand ranks suited-connectors))))

(defn action [{:keys [position action-to-you hand]}]
  (let [raise-with (get-in proper-action [position action-to-you :raise])
        call-with (get-in proper-action [position action-to-you :call])]
    (cond
      (actionable-pair? raise-with hand) :raise
      (actionable-non-pair? raise-with hand) :raise

      (actionable-pair? call-with hand) :call
      (actionable-non-pair? call-with hand) :call

      :else :fold)))
