(ns fox-say.fox)

(def positions #{:utg :blind :middle :late})
(def actions-to-you #{:folded :called :raised})
(def fresh-deck
  (for [rank ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"]
        suit ["S" "H" "C" "D"]]
    (str rank suit)))

(defn rank [[r _]]
  (let [upper-ranks {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (get upper-ranks r))))

(defn suit [[_ s]]
  (str s))

(def proper-action
  {:utg {:called {:raise [{:pair 14} {:pair 13} {:pair 12} {:pair 11} {:pair 10}
                          {:suited [14 13]} {:suited [14 12]}
                          {:unsuited [14 13]} {:unsuited [14 12]}]}
         :raised {:raise [{:pair 14} {:pair 13} {:pair 12} {:pair 11}
                          {:suited [14 13]}
                          {:unsuited [14 13]}]}}

   :blind {:called {:raise [{:pair 14} {:pair 13} {:pair 12} {:pair 11} {:pair 10} {:pair 9} {:pair 8}
                            {:suited [14 13]} {:suited [14 12]} {:suited [14 11]}
                            {:unsuited [14 13]} {:unsuited [14 12]} {:unsuited [14 11]}]
                    :call [{:suited-connector :any}
                           {:suited [14 10]} {:suited [13 12]} {:suited [13 11]} {:suited [13 10]} {:suited [12 11]} {:suited [12 10]} {:suited [11 10]}]}
           :raised {:raise [{:pair 14} {:pair 13} {:pair 12}
                            {:suited [14 13]}
                            {:unsuited [14 13]}]
                    :call [{:pair 11} {:pair 10} {:pair 9} {:pair 8} {:pair 7} {:pair 6}
                           {:suited-connector :any}
                           {:suited-one-gap :any}]}}

   :middle {:folded {:raise [{:pair 14} {:pair 13} {:pair 12} {:pair 11} {:pair 10} {:pair 9} {:pair 8} {:pair 7}
                             {:suited [14 13]} {:suited [14 12]} {:suited [14 11]} {:unsuited [14 13]} {:unsuited [14 12]} {:unsuited [14 11]}
                             {:suited [14 10]} {:suited [13 12]} {:suited [13 11]} {:suited [13 10]} {:suited [12 11]} {:suited [12 10]}]}
            :called {:call [{:pair 14} {:pair 13} {:pair 12} {:pair 11} {:pair 10} {:pair 9}
                            {:suited [14 13]} {:suited [14 12]} {:suited [14 11]} {:suited [14 10]}
                            {:suited [13 12]} {:suited [13 11]} {:suited [13 10]} {:suited [12 11]} {:suited [12 10]}
                            {:suited-connector :any}]}
            :raised {:raise [{:pair 14} {:pair 13} {:pair 12} {:pair 11} {:pair 10}
                             {:suited [14 13]} {:suited [14 12]}
                             {:unsuited [14 13]} {:unsuited [14 12]}]
                     :call [{:pair 9} {:pair 8} {:pair 7} {:pair 6} {:pair 5} {:pair 4} {:pair 3} {:pair 2}
                            {:suited-connector :any}]}}

   :late {:folded {:raise [{:pair :any}
                           {:suited-connector :any}
                           {:suited [14 13]} {:suited [14 12]} {:suited [14 11]} {:suited [14 10]} {:suited [14 9]} {:suited [14 8]}
                           {:suited [14 7]} {:suited [14 6]} {:suited [14 5]} {:suited [14 4]} {:suited [14 3]} {:suited [14 2]} ;;AXs
                           {:unsuited [14 13]} {:unsuited [14 12]} {:unsuited [14 11]} {:unsuited [14 10]} {:unsuited [14 9]} {:unsuited [14 8]} {:unsuited [14 7]} ;;A7o
                           {:suited [13 12]} {:suited [13 11]} {:suited [13 10]} {:suited [12 11]} {:suited [12 10]} {:suited [11 10]} {:unsuited [13 12]} {:unsuited [13 11]}
                           {:unsuited [13 10]} {:unsuited [12 11]} {:unsuited [12 10]} {:unsuited [11 10]} ;;big cards >= 10
                           ]}
          :called {:raise [{:pair 14} {:pair 13} {:pair 12} {:pair 11} {:pair 10}
                           {:suited [14 13]} {:suited [14 12]} {:suited [14 11]}
                           {:unsuited [14 13]} {:unsuited [14 12]} {:unsuited [14 11]}]
                   :call [{:pair 9} {:pair 8} {:pair 7} {:pair 6} {:pair 5} {:pair 4} {:pair 3} {:pair 2}
                          {:suited-connector :any}
                          {:suited [14 10]} {:suited [14 9]} {:suited [14 8]} {:suited [14 7]} {:suited [14 6]} {:suited [14 5]} {:suited [14 4]} {:suited [14 3]} {:suited [14 2]} ;;AXs
                          {:suited [13 12]} {:suited [13 11]} {:suited [13 10]} {:suited [13 9]}
                          {:suited [12 11]} {:suited [12 10]} {:suited [12 9]}
                          {:suited [11 10]} {:suited [11 9]}
                          {:suited [10 9]}]}
          :raised {:raise [{:pair 14} {:pair 13} {:pair 12} {:pair 11} {:pair 10}
                           {:suited [14 13]} {:suited [14 12]} {:unsuited [14 13]} {:unsuited [14 12]}]
                   :call [{:pair 9} {:pair 8} {:pair 7}
                          {:suited-connector :any}]}}})

(defn pair? [hand]
  (apply = (map rank hand)))

(defn suited? [hand]
  (apply = (map suit hand)))

(defn actionable-pair? [action-with hand]
  (let [actionable-pair-ranks (map :pair action-with)]
    (and (pair? hand)
         (or (= :any (first actionable-pair-ranks))
             (some #(= (rank (first hand)) %) actionable-pair-ranks)))))

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

(defn suited-one-gap? [hand ranks]
  (and (suited? hand)
       (= (first ranks) (+ 2 (second ranks)))))

(defn suited-one-gap-match [hand ranks suited-one-gaps]
  (and (suited-one-gap? hand ranks)
       (or (= :any (first suited-one-gaps))
           (some #(= ranks %) suited-one-gaps))))

(defn not-nil-by-key [key coll]
  (filter #(not (nil? %)) (map key coll)))

(defn actionable-non-pair? [action-with hand]
  (let [suited-ranks (not-nil-by-key :suited action-with)
        unsuited-ranks (not-nil-by-key :unsuited action-with)
        suited-connectors (not-nil-by-key :suited-connector action-with)
        suited-one-gaps (not-nil-by-key :suited-one-gap action-with)
        ranks (hand-ranks hand)]
    (or
      (suited-rank-match hand ranks suited-ranks)
      (unsuited-rank-match ranks unsuited-ranks)
      (suited-connector-match hand ranks suited-connectors)
      (suited-one-gap-match hand ranks suited-one-gaps))))

(defn action [{:keys [position action-to-you hand]}]
  (let [raise-with (get-in proper-action [position action-to-you :raise])
        call-with (get-in proper-action [position action-to-you :call])]
    (cond
      (actionable-pair? raise-with hand) :raise
      (actionable-non-pair? raise-with hand) :raise

      (actionable-pair? call-with hand) :call
      (actionable-non-pair? call-with hand) :call

      :else :fold)))

(defn deal []
  {:position (rand-nth (seq positions))
   :action-to-you (rand-nth (seq actions-to-you))
   :hand (take 2 (shuffle fresh-deck))})

(comment
  (let [deck (shuffle fresh-deck)]
    (loop [deck deck]
      (let [hand (take 2 deck)]
        (when (seq hand)
          (println "hand" hand)
          (recur (drop 2 deck)))))))
