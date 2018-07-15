(ns fox-say.fox)

(defn str->int [s]
  #?(:clj  (Integer/parseInt s)
     :cljs (js/parseInt s)))

(def positions #{:early :blind :middle :late})
(def actions-to-you #{:folded :called :raised})
(def ranks ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"])
(def suits ["S" "H" "C" "D"])
(def player-count-na 100)

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

(defn frequencies-of [f hand]
  (frequencies (map f hand)))

(defn rank-frequencies [hand]
  (frequencies-of rank hand))

(defn suit-frequencies [hand]
  (frequencies-of suit hand))

(defn proper-size-and-rank?
  "Given a hand, returns whether or not the number of distinct ranks is the right and the number of items in a rank is
  correct."
  [hand size rank-count]
  (let [rank-counts (vals (rank-frequencies hand))]
    (and (= size (count rank-counts))
         (not (nil? (some #{rank-count} rank-counts))))))

(defn pair?
  "For a full hand, a pair should have 4 distinct ranks, and 2 of one rank.
  For hole cards, a pair should have 1 rank and two cards in that rank."
  [hand]
  (or (proper-size-and-rank? hand 4 2)
      (proper-size-and-rank? hand 1 2)))

(defn two-pair?
  "Two pair should have 3 distinct ranks, and 2 of two ranks."
  [hand]
  (proper-size-and-rank? hand 3 2))

(defn trips?
  "Trips should have 3 ranks, and 3 in one rank."
  [hand]
  (proper-size-and-rank? hand 3 3))

(defn quads?
  "Quads should have 2 ranks, and 4 in one rank."
  [hand]
  (proper-size-and-rank? hand 2 4))

(defn full-house?
  "Full house should have 2 ranks, with 3 in one and 2 in the other."
  [hand]
  (and (proper-size-and-rank? hand 2 3)
       (proper-size-and-rank? hand 2 2)))

(defn flush?
  "Flush should have 5 of the same suit. Note that the suited? function isn't granular enough, as it would report
  straight flush as a normal flush."
  [hand]
  (= 5 (first (vals (suit-frequencies hand)))))

(defn straight? [hand]
  (let [ordered-hand-ranks (sort (map rank hand))
        low-rank (apply min ordered-hand-ranks)
        expected-straight (range low-rank (+ low-rank 5))
        low-straight (range 1 6)
        switch-to-low-ace (fn [ranks] (sort (replace {14 1} ranks)))]
    (or (= ordered-hand-ranks expected-straight)
        (= (switch-to-low-ace ordered-hand-ranks) low-straight))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn suited? [hand]
  (apply = (map suit hand)))

(defn pairs-for-ranks [upper lower]
  (let [v (subvec ranks (.indexOf ranks upper) (inc (.indexOf ranks lower)))]
    (for [r v]
      {:pair (rank r)})))

(defn styled-cards-from-rank [style upper lower]
  (let [upper-rank (rank upper)
        v (subvec ranks (inc (.indexOf ranks upper)) (inc (.indexOf ranks lower)))]
    (for [r v]
      {style [upper-rank (rank r)]})))

(defn styled-connectors-from-rank [style upper lower]
  (let [upper-rank (rank upper)
        lower-rank (max 3 (rank lower))
        connector (keyword (str (name style) "-connector"))]
    (for [r (reverse (range lower-rank (inc upper-rank)))]
      {connector [r (dec r)]})))

(def no-limit-pre-flop-proper-actions
  {:early {:called {:raise {player-count-na (concat (pairs-for-ranks "A" "T")
                                                     (styled-cards-from-rank :suited "A" "Q")
                                                     (styled-cards-from-rank :unsuited "A" "Q"))}}
            :raised {:raise {player-count-na (concat (pairs-for-ranks "A" "T")
                                                     (styled-cards-from-rank :suited "A" "Q")
                                                     (styled-cards-from-rank :unsuited "A" "Q"))}}}

    :blind {:called {:raise {player-count-na (concat (pairs-for-ranks "A" "8")
                                                     (styled-cards-from-rank :suited "A" "J")
                                                     (styled-cards-from-rank :unsuited "A" "J"))}
                     :call {player-count-na [{:suited-connector :any}
                                             {:suited [14 10]} {:suited [13 11]} {:suited [13 10]} {:suited [12 10]}]}}
            :raised {:raise {player-count-na (concat (pairs-for-ranks "A" "Q")
                                                     [{:suited [14 13]} {:unsuited [14 13]}])}
                     :call {player-count-na (concat (pairs-for-ranks "J" "7")
                                                    [{:suited-connector :any}
                                                     {:suited-one-gap :any}])}}}

    :middle {:folded {:raise {player-count-na (concat (pairs-for-ranks "A" "7")
                                                      (styled-cards-from-rank :suited "A" "T")
                                                      (styled-cards-from-rank :unsuited "A" "J")
                                                      (styled-cards-from-rank :suited "K" "T")
                                                      (styled-cards-from-rank :suited "Q" "T"))}}
             :called {:raise {player-count-na (concat (pairs-for-ranks "A" "T")
                                                      (styled-cards-from-rank :unsuited "A" "J"))}
                      :call {player-count-na (concat (pairs-for-ranks "9" "2")
                                                     [{:suited-connector :any}
                                                      {:suited [14 10]} {:suited [13 11]} {:suited [13 10]} {:suited [12 10]}])}}
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
                                                   [{:suited-connector :any}])}}}})

(def low-limit-pre-flop-proper-actions
  {:early {:called {:raise {player-count-na (concat (pairs-for-ranks "A" "J")
                                                    (styled-cards-from-rank :suited "A" "J")
                                                    (styled-cards-from-rank :unsuited "A" "Q")
                                                    [{:suited [13 12]}])}
                    :call {player-count-na (concat (pairs-for-ranks "T" "7")
                                                   (styled-cards-from-rank :suited "K" "J")
                                                   (styled-connectors-from-rank :suited "Q" "J")
                                                   [{:suited [14 10]} {:suited [14 9]}
                                                    {:unsuited [13 12]}])}}
           :raised {:raise {player-count-na (concat (pairs-for-ranks "A" "J")
                                                    (styled-cards-from-rank :suited "A" "Q")
                                                    (styled-cards-from-rank :unsuited "A" "Q"))}
                    :call {player-count-na (concat (pairs-for-ranks "T" "7")
                                                   (styled-cards-from-rank :suited "K" "J")
                                                   (styled-connectors-from-rank :suited "Q" "J")
                                                   [{:suited [14 11]} {:suited [14 10]} {:suited [14 9]}
                                                    {:unsuited [13 12]}])}}}
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
                                      (styled-connectors-from-rank :suited "J" "9")
                                      [{:suited [12 10]} {:suited [11 9]}
                                       {:unsuited [13 11]} {:unsuited [12 11]} {:unsuited [11 10]}])
                            player-count-na (concat (pairs-for-ranks "7" "2")
                                                    (styled-cards-from-rank :suited "A" "2")
                                                    (styled-cards-from-rank :suited "K" "7")
                                                    (styled-connectors-from-rank :suited "T" "7")
                                                    [{:suited [10 8]}
                                                     {:unsuited [13 11]} {:unsuited [12 11]} {:unsuited [11 10]}])}}
            :raised {:raise {player-count-na (concat (pairs-for-ranks "A" "J")
                                                     (styled-cards-from-rank :suited "A" "Q")
                                                     (styled-cards-from-rank :unsuited "A" "Q"))}
                     :call {player-count-na (concat (pairs-for-ranks "T" "9")
                                                    (styled-cards-from-rank :suited "K" "J")
                                                    [{:suited [14 11]} {:suited [12 11]}])}}}
   :late {:called {:raise {4 (concat (pairs-for-ranks "A" "8")
                                     (styled-cards-from-rank :suited "A" "8")
                                     (styled-cards-from-rank :unsuited "A" "9")
                                     (styled-cards-from-rank :suited "K" "J")
                                     [{:suited [12 11]} {:unsuited [13 12]}])
                           player-count-na (concat (pairs-for-ranks "A" "8")
                                                   (styled-cards-from-rank :suited "A" "8")
                                                   (styled-cards-from-rank :unsuited "A" "9")
                                                   (styled-cards-from-rank :suited "K" "J")
                                                   [{:suited [12 11]} {:unsuited [13 12]}])}
                   :call {4 (concat (pairs-for-ranks "7" "5")
                                    (styled-cards-from-rank :suited "A" "2")
                                    (styled-cards-from-rank :suited "K" "2")
                                    (styled-connectors-from-rank :suited "T" "5")
                                    [{:suited [12 10]} {:suited [11 10]} {:suited [10 8]} {:suited [9 7]} {:suited [8 6]} {:suited [7 5]}
                                     {:unsuited [12 11]} {:unsuited [11 10]}
                                     {:unsuited [13 11]} {:unsuited [12 10]} {:unsuited [13 10]}])
                          player-count-na (concat (pairs-for-ranks "7" "2")
                                                  (styled-cards-from-rank :suited "A" "2")
                                                  (styled-cards-from-rank :suited "K" "2")
                                                  (styled-cards-from-rank :suited "Q" "2")
                                                  (styled-connectors-from-rank :suited "T" "5")
                                                  [{:suited [11 10]} {:suited [10 8]} {:suited [9 7]} {:suited [8 6]} {:suited [7 5]} {:suited [6 4]} {:suited [5 3]}
                                                   {:unsuited [12 11]} {:unsuited [11 10]}
                                                   {:unsuited [13 11]} {:unsuited [12 10]} {:unsuited [13 10]}])}}
          :raised {:raise {player-count-na (concat (pairs-for-ranks "A" "J")
                                                   (styled-cards-from-rank :suited "A" "Q")
                                                   (styled-cards-from-rank :unsuited "A" "Q"))}
                   :call {player-count-na (concat (pairs-for-ranks "T" "9")
                                                  (styled-cards-from-rank :suited "K" "J")
                                                  [{:suited [14 11]} {:suited [12 11]}])}}}
   :blind {:raised {:raise {player-count-na (concat (pairs-for-ranks "A" "8")
                                                    (styled-cards-from-rank :suited "A" "Q")
                                                    (styled-cards-from-rank :unsuited "A" "Q")
                                                    [{:suited [13 11]} {:suited [12 11]} {:unsuited [13 12]}])}}
           :called {:raise {player-count-na (concat (pairs-for-ranks "A" "8")
                                                    (styled-cards-from-rank :suited "A" "Q")
                                                    (styled-cards-from-rank :unsuited "A" "Q")
                                                    [{:suited [13 11]} {:suited [12 11]} {:unsuited [13 12]}])}
                    :call {player-count-na [:any]}}}})

(def pre-flop-proper-action
  {:no-limit no-limit-pre-flop-proper-actions
   :low-limit low-limit-pre-flop-proper-actions})

(def pre-flop-proper-action-description
  {:no-limit
   {:early {:called "In early position, when called or raised to you, you should raise with AA - TT, AK, AQ."
            :raised "In early position, when called or raised to you, you should raise with AA - TT, AK, AQ."}
    :blind {:called "In the blind, when called to you, you should raise with AA - 88, AK - AJ. You should call with suited connectors or suited big cards (T or better)."
            :raised "In the blind, when raised to you, you should raise with AA - QQ, AK. You should call with JJ - 77, suited connectors, or suited one gaps, provided you have a lot of chips."}
    :middle {:folded "In middle position, when folded to you, you should raise with AA - 77, AK - AJ, or suited cards QT or better. DON’T LIMP!"
             :called "In middle position, when called to you, you should raise with AA - TT, AK - AJ. You should call with 99 - 22, suited cards QT or better, or suited connectors."
             :raised "In middle position, when raised to you, you should raise with AA - TT, AK, AQ. You should call with 99 - 22 or suited connectors."}
    :late {:folded "In late position, when folded to you, you should raise with AA - 22, AX suited, A7 or better, big cards (T or better), or suited connectors."
           :called "In late position, when called to you, you should raise with AA - TT, AK - AJ. You should call with 99 - 22, AX suited, suited cards 9 or better, or suited connectors."
           :raised "In late position, when raised to you, you should raise with AA - TT, AK, AQ. You should call with 99 - 77 or suited connectors."}}

   :low-limit
   {:early {:called {:raise {player-count-na "You should raise early when called to you with AA - JJ, AKs - AJs, AKo - AQo, KQs."}
                     :call {player-count-na "You should call early when called to you with TT - 77, KJs, QJs, JTs, ATs, A9s, KQo."}}
            :raised {:raise {player-count-na "You should raise early when raised to you with AA - JJ, AKs - AQs, AKo - AQo, KQs."}
                     :call {player-count-na "You should call in early position when raised to you with TT - 77, AJs, ATs, A9s, KQs - JTs, KJs, KQo."}}}
    :middle {:called {:raise {3 "You should raise in middle position when 3 or fewer callers to you with AA - 88, AK - AT, KQs - KJs, QJs, KQo."
                              player-count-na "You should raise in middle position when 4 or more callers to you with AA - 88, AK - AT, KQs - KJs, QJs, KQo."}
                      :call {3 "You should call in middle position when 3 or fewer callers to you with 77 - 55, AXs, KQs - K8s, T9s, 98s, KJo, QJo, JTo."
                             player-count-na "You should call in middle position when 4 or more callers to you with 77 - 22, AXs, KQs - K7s, T9s - 76s, T8s, KJo, QJo, JTo."}}
             :raised {:raise {player-count-na "You should raise in middle position when raised to you with AA - JJ, AK, AQ."}
                      :call {player-count-na "You should call in middle position when raised to you with TT - 99, AJs, ATs, KQs - QJs, KJs."}}}
    :late {:called {:raise {4 "You should raise in late position when called to with AA - 88, AK - A9, KQ, A8s, KJs, QJs, KQo."
                            player-count-na "You should raise in late position when called to with AA - 88, AK - A9, KQ, A8s, KJs, QJs, KQo."}
                    :call {4 "You should call in late position when 4 or fewer callers to you with 77 - 55, JTs - 54s, QTs - 75s, AXs, KXs, QJo - JTo, KJo - QTo, KTo."
                           player-count-na "You should call in late position when 5 or more callers to you with 77 - 22, JTs - 54s, QTs - 53s, AXs, KXs, QXs, QJo - JTo, KJo - QTo, KTo."}}
           :raised {:raise {player-count-na "You should raise in late position when raised to with AA - JJ, AK, AQ."}
                    :call {player-count-na "You should call in late position when raised to with TT - 77, KQs - QJs, KJs, AJs - A9s."}}}
    :blind {:called {:raise {player-count-na "You should raise in the blind when called to you with AA - 88, AKs - QJs, AQs - KJs, AKo - KQo, AQo."}
                     :call {player-count-na "You should call in the blinds when called to you with anything that costs half a bet."}}
            :raised {:raise {player-count-na "You should raise in the blind when called to you with AA - 88, AKs - QJs, AQs - KJs, AKo - KQo, AQo."}
                     :call {player-count-na "You should NOT call in the blind when raised to you. If you can't raise, you should fold."}}}
    }})


(defn not-nil-by-key [key coll]
  (filter #(not (nil? %)) (map key coll)))

(defn actionable-pair? [action-with hand]
  (let [actionable-pair-ranks (not-nil-by-key :pair action-with)]
    (and (pair? hand)
         (or (= :any (first actionable-pair-ranks))
             (some #(= (rank (first hand)) %) actionable-pair-ranks)))))

(defn hand-ranks [hand]
  (sort > (map rank hand)))

(defn suited-connector? [hand [r1 r2]]
  (and (suited? hand)
       (or
         (= r1 (inc r2))
         (and (= 14 r1) (= 2 r2)))))

(defn suited-rank-match [hand ranks suited-ranks]
  (and (suited? hand) (some #(= ranks %) suited-ranks)))

(defn unsuited-rank-match [ranks unsuited-ranks]
  (some #(= ranks %) unsuited-ranks))

(defn suited-connector-match [hand ranks suited-connectors]
  (and (suited-connector? hand ranks)
       (or (= :any (first suited-connectors))
           (some #(= ranks %) suited-connectors))))

(defn suited-one-gap? [hand [r1 r2]]
  (and (suited? hand)
       (or
         (= r1 (+ 2 r2))
         (and (= 14 r1) (= 3 r2)))))

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

(defn top-two-pair? [hand]
  (let [rank-frequencies (rank-frequencies hand)
        top-two (take 2 (into (sorted-map-by >) rank-frequencies))]
    (= 2 (val (first top-two)) (val (second top-two)))))

(defn overpair? [hole flop]
  (let [top-hole-rank (apply max (map rank hole))
        top-flop-rank (apply max (map rank flop))]
    (and (pair? hole)
         (> top-hole-rank top-flop-rank))))

(defn top-pair-and-kicker? [hole flop]
  (let [hole-ranks (map rank hole)
        top-hole-rank (apply max hole-ranks)
        top-flop-rank (apply max (map rank flop))]
    (and (not (pair? hole))
         (pair? (concat hole flop))
         (some #(= top-flop-rank %) hole-ranks)
         (or (= 14 top-hole-rank)
             (= [14 13] hole-ranks)))))

(defn very-strong-made-hand? [hand]
  (or
    (top-two-pair? hand)
    (trips? hand)
    (straight? hand)
    (flush? hand)
    (full-house? hand)
    (quads? hand)))

(defn strong-made-hand? [hole flop]
  (or (overpair? hole flop) (top-pair-and-kicker? hole flop)))

(defn made-hand? [hole flop]
  (let [hand (concat hole flop)]
    (or
      (very-strong-made-hand? hand)
      (pair? hand)
      (two-pair? hand))))

(defn hand-category [hole flop]
  (let [hand (concat hole flop)]
    (cond
      (very-strong-made-hand? hand) :very-strong
      (strong-made-hand? hole flop) :strong
      (pair? (concat hole flop)) :mediocre
      :else :trash)))

(defn action-with [_ game-type position action-to-you action-count action]
  (let [action-hands (get-in pre-flop-proper-action [game-type position action-to-you action])
        action-count (first (filter #(<= action-count %) (keys action-hands)))]
    (get action-hands action-count)))

(defn action [{:keys [game-type position street action-to-you action-count hand]
               :or {game-type :no-limit street :pre-flop action-count player-count-na}}]
  (let [raise-with (action-with street game-type position action-to-you action-count :raise)
        call-with (action-with street game-type position action-to-you action-count :call)]
    (cond
      (or (= :any (first raise-with)) (actionable-pair? raise-with hand)) :raise
      (or (= :any (first raise-with)) (actionable-non-pair? raise-with hand)) :raise

      (or (= :any (first call-with)) (actionable-pair? call-with hand)) :call
      (or (= :any (first call-with)) (actionable-non-pair? call-with hand)) :call

      :else :fold)))

(defmulti action-with-description (fn [state] (:street state)))

(defmethod action-with-description :pre-flop [{:keys [game-type position action-to-you action-count]
                                               :or {game-type :no-limit action-count player-count-na}
                                               :as hand-state}]
  (let [correct-action (action hand-state)
        description (get-in pre-flop-proper-action-description [game-type position action-to-you])]
    {:correct-action correct-action
     :description description}))


(defmethod action-with-description :flop [{:keys [hand flop] :as state}]
  (let [category (hand-category hand flop)
        proper-action-category {:very-strong :aggressive
                                :strong :aggressive
                                :mediocre :passive
                                :trash :passive}]
    {:correct-action (get proper-action-category category)
     :description "Very strong hands are almost always ahead, so should be played very aggressively.
     Strong hands are usually ahead, so aggressive play is appropriate.
     For mediocre hands, it depends on what you're holding, the community cards, and the number of other players.
     For trash, an occasional bluff is OK, but you should usually fold with any action."}))

(defn deal-hole []
  (let [position (rand-nth (seq positions))
        action-to-you (rand-nth (seq actions-to-you))
        deck (shuffle fresh-deck)
        hand (take 2 deck)
        deck (drop 2 deck)]
    (if (and (= :folded action-to-you)
             (#{:early :blind} position))
      (deal-hole)
      {:position position
       :action-to-you action-to-you
       :action-count (rand-nth (range 3 6))
       :hand hand
       :deck deck})))

(defn deal-flop [{:keys [deck]}]
  (take 3 deck))

(comment
  (let [deck (shuffle fresh-deck)]
    (loop [deck deck]
      (let [hand (take 2 deck)]
        (when (seq hand)
          (println "hand" hand)
          (recur (drop 2 deck)))))))
