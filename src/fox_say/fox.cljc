(ns fox-say.fox)

(defn str->int [s]
  #?(:clj  (Integer/parseInt s)
     :cljs (js/parseInt s)))

(def positions #{:early :blind :middle :late})
(def actions-to-you #{:folded :called :raised})
(def ranks ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"])
(def suits ["S" "H" "C" "D"])

(def fresh-deck
  (for [rank ranks
        suit suits]
    (str rank suit)))

(defn rank
  ([[r _]]
   (let [r (str r)
         upper-ranks {"A" 14 "K" 13 "Q" 12 "J" 11 "T" 10}
         v (get upper-ranks r)]
     (if v
       v
       (str->int r)))))

(defn suit [[_ s]]
  (str s))

(def player-count-na 100)

(defn pairs-for-ranks [upper lower]
  (let [v (subvec ranks (.indexOf ranks upper) (inc (.indexOf ranks lower)))]
    (for [r v]
      {:pair (rank r)})))

(defn styled-cards-from-rank [style upper lower]
  (let [upper-rank (rank upper)
        v (subvec ranks (inc (.indexOf ranks upper)) (inc (.indexOf ranks lower)))]
    (for [r v]
      {style [upper-rank (rank r)]})))

(def proper-action
  {:no-limit
   {:early {:called {:raise {player-count-na (concat (pairs-for-ranks "A" "T")
                                                     (styled-cards-from-rank :suited "A" "Q")
                                                     (styled-cards-from-rank :unsuited "A" "Q"))}}
            :raised {:raise {player-count-na (concat (pairs-for-ranks "A" "J")
                                                     [{:suited [14 13]} {:unsuited [14 13]}])}}}

    :blind {:called {:raise {player-count-na (concat (pairs-for-ranks "A" "8")
                                                     (styled-cards-from-rank :suited "A" "J")
                                                     (styled-cards-from-rank :unsuited "A" "J"))}
                     :call {player-count-na [{:suited-connector :any}
                                             {:suited [14 10]} {:suited [13 11]} {:suited [13 10]} {:suited [12 11]} {:suited [12 10]}]}}
            :raised {:raise {player-count-na (concat (pairs-for-ranks "A" "Q")
                                                     [{:suited [14 13]} {:unsuited [14 13]}])}
                     :call {player-count-na (concat (pairs-for-ranks "J" "6")
                                                    [{:suited-connector :any}
                                                     {:suited-one-gap :any}])}}}

    :middle {:folded {:raise {player-count-na (concat (pairs-for-ranks "A" "7")
                                                      (styled-cards-from-rank :suited "A" "T")
                                                      (styled-cards-from-rank :unsuited "A" "J")
                                                      (styled-cards-from-rank :suited "K" "T")
                                                      (styled-cards-from-rank :suited "Q" "T"))}}
             :called {:call {player-count-na (concat (pairs-for-ranks "A" "9")
                                                     (styled-cards-from-rank :suited "A" "T")
                                                     (styled-cards-from-rank :suited "K" "T")
                                                     (styled-cards-from-rank :suited "Q" "T")
                                                     [{:suited-connector :any}])}}
             :raised {:raise {player-count-na (concat (pairs-for-ranks "A" "T")
                                                      (styled-cards-from-rank :suited "A" "Q")
                                                      (styled-cards-from-rank :unsuited "A" "Q"))}
                      :call {player-count-na (concat (pairs-for-ranks "9" "2")
                                                     [{:suited-connector :any}])}}}

    :late {:folded {:raise {player-count-na (concat (styled-cards-from-rank :suited "A" "2")
                                                    (styled-cards-from-rank :unsuited "A" "7")
                                                    (styled-cards-from-rank :suited "K" "T")
                                                    (styled-cards-from-rank :unsuited "K" "T")
                                                    (styled-cards-from-rank :suited "Q" "T")
                                                    (styled-cards-from-rank :unsuited "Q" "T")
                                                    (styled-cards-from-rank :suited "J" "T")
                                                    (styled-cards-from-rank :unsuited "J" "T")
                                                    [{:pair :any}
                                                     {:suited-connector :any}])}}
           :called {:raise {player-count-na (concat (pairs-for-ranks "A" "T")
                                                    (styled-cards-from-rank :suited "A" "J")
                                                    (styled-cards-from-rank :unsuited "A" "J"))}
                    :call {player-count-na (concat (pairs-for-ranks "9" "2")
                                                   (styled-cards-from-rank :suited "K" "9")
                                                   (styled-cards-from-rank :suited "Q" "9")
                                                   (styled-cards-from-rank :suited "J" "9")
                                                   [{:suited-connector :any}
                                                    {:suited [14 10]} {:suited [14 9]} {:suited [14 8]} {:suited [14 7]}
                                                    {:suited [14 6]} {:suited [14 5]} {:suited [14 4]} {:suited [14 3]} {:suited [14 2]} ;;A->T-Xs
                                                    ])}}
           :raised {:raise {player-count-na (concat (pairs-for-ranks "A" "T")
                                                    (styled-cards-from-rank :suited "A" "Q")
                                                    (styled-cards-from-rank :unsuited "A" "Q"))}
                    :call {player-count-na (concat (pairs-for-ranks "9" "7")
                                                   [{:suited-connector :any}])}}}}

   :low-limit {:early {:called {:raise {player-count-na (concat (pairs-for-ranks "A" "J")
                                                                (styled-cards-from-rank :suited "A" "J")
                                                                (styled-cards-from-rank :unsuited "A" "Q")
                                                                [{:suited [13 12]}])}}}
               :middle {:called {:raise {3 (concat (pairs-for-ranks "A" "8")
                                                   (styled-cards-from-rank :suited "A" "T")
                                                   (styled-cards-from-rank :unsuited "A" "T")
                                                   (styled-cards-from-rank :suited "K" "J")
                                                   [{:suited [12 11]} {:unsuited [13 12]}])
                                         player-count-na (concat (pairs-for-ranks "A" "8")
                                                                 (styled-cards-from-rank :suited "A" "T")
                                                                 (styled-cards-from-rank :unsuited "A" "T")
                                                                 (styled-cards-from-rank :suited "K" "J")
                                                                 [{:suited [12 11]} {:unsuited [13 12]}])}
                                 :call {3 (concat (pairs-for-ranks "7" "5")
                                                  (styled-cards-from-rank :suited "A" "2")
                                                  (styled-cards-from-rank :suited "K" "8")
                                                  [{:suited [10 9]} {:suited [9 8]}
                                                   {:unsuited [13 11]} {:unsuited [12 11]} {:unsuited [11 10]}])
                                        player-count-na (concat (pairs-for-ranks "7" "2")
                                                                (styled-cards-from-rank :suited "A" "2")
                                                                (styled-cards-from-rank :suited "K" "7")
                                                                [{:suited [10 9]} {:suited [9 8]} {:suited [8 7]} {:suited [7 6]}
                                                                 {:suited [10 8]}
                                                                 {:unsuited [13 11]} {:unsuited [12 11]} {:unsuited [11 10]}])}}
                        :raised {:raise {player-count-na (concat (pairs-for-ranks "A" "J")
                                                                 (styled-cards-from-rank :suited "A" "Q")
                                                                 (styled-cards-from-rank :unsuited "A" "Q"))}
                                 :call {player-count-na (concat (pairs-for-ranks "T" "9")
                                                                (styled-cards-from-rank :suited "K" "J")
                                                                [{:suited [14 11]} {:suited [12 11]}])}}}}})

(def proper-action-description
  {:early {:called {:raise "You should raise UTG when called to you with AA - TT, AK, AQ."}
         :raised {:raise "You should raise UTG when raised to you with AA - JJ, AK."}}
   :blind {:called {:raise "You should raise in the blind when called to you with AA - 88, or AJ or better."
                    :call "You should call in the blind when called to you with suited connectors or suited cards T or better."}
           :raised {:raise "You should raise in the blind when raised to you with AA - QQ, AK."
                    :call "You should call in the blind when raised to you with suited connectors or suited one gaps, provided you have lots of chips."}}
   :middle {:folded {:raise "You should raise in the middle when folded to you with AA - 77, AK - AJ, or suited cards QT or better."}
            :called {:call "You should call in the middle when called to you with AA - 99, suited cards QT or better, or suited connectors." }
            :raised {:raise "You should raise in the middle when raised to you with AA - TT, AK, AQ."
                     :call "You should call in the middle when raised to you with 99 - 22 or suited connectors."}}
   :late {:folded {:raise "You should raise in late position when folded to with AA - 22, AX suited, A7 or better, big cards (T or better), or suited connectors."}
          :called {:raise "You should raise in late position when called to with AA - TT, AK - AJ."
                   :call "You should call in late position when called to with 99 - 22, AX suited, suited cards 9 or better, or suited connectors."}
          :raised {:raise "You should raise in late position when raised to with AA - TT, AK, AQ."
                   :call "You should call in late position when raised to with 99 - 77 or suited connectors."}}})

(defn pair? [hand]
  (apply = (map rank hand)))

(defn suited? [hand]
  (apply = (map suit hand)))

(defn not-nil-by-key [key coll]
  (filter #(not (nil? %)) (map key coll)))

(defn actionable-pair? [action-with hand]
  (let [actionable-pair-ranks (not-nil-by-key :pair action-with)]
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

(defn action-with [game-type position action-to-you action-count action]
  (let [action-hands (get-in proper-action [game-type position action-to-you action])
        action-count (first (filter #(<= action-count %) (keys action-hands)))]
    (get action-hands action-count)))

(defn action [{:keys [game-type position action-to-you action-count hand] :or {game-type :no-limit action-count player-count-na}}]
  (let [raise-with (action-with game-type position action-to-you action-count :raise)
        call-with (action-with game-type position action-to-you action-count :call)]
    (cond
      (actionable-pair? raise-with hand) :raise
      (actionable-non-pair? raise-with hand) :raise

      (actionable-pair? call-with hand) :call
      (actionable-non-pair? call-with hand) :call

      :else :fold)))

(defn action-with-description [{:keys [game-type position action-to-you] :or {game-type :no-limit} :as hand-state}]
  (let [action (action hand-state)
        description (get-in proper-action-description [position action-to-you action])
        description (if (not description)
                      (for [a [:raise :call]]
                        (get-in proper-action-description [position action-to-you a]))
                      [description])]
    {:correct-action action
     :description description}))

(defn deal []
  (let [position (rand-nth (seq positions))
        action-to-you (rand-nth (seq actions-to-you))]
    (if (and (= :folded action-to-you)
             (#{:early :blind} position))
      (deal)
      {:position position
       :action-to-you action-to-you
       :hand (take 2 (shuffle fresh-deck))})))

(comment
  (let [deck (shuffle fresh-deck)]
    (loop [deck deck]
      (let [hand (take 2 deck)]
        (when (seq hand)
          (println "hand" hand)
          (recur (drop 2 deck)))))))
