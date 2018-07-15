(ns fox-say.fox-test
  (:require [clojure.test :refer [deftest testing is]]
            [fox-say.fox :as fox]))

(deftest test-pairs-for-ranks
  (is (= [{:pair 10} {:pair 9} {:pair 8}] (fox/pairs-for-ranks "T" "8")))
  (is (= [{:pair 4} {:pair 3} {:pair 2}] (fox/pairs-for-ranks "4" "2")))
  (is (= [{:pair 14}] (fox/pairs-for-ranks "A" "A"))))

(deftest test-styled-cards-from-rank
  (is (= [{:suited [14 13]} {:suited [14 12]} {:suited [14 11]}] (fox/styled-cards-from-rank :suited "A" "J")))
  (is (= [{:suited [13 12]}] (fox/styled-cards-from-rank :suited "K" "Q")))
  (is (= [{:unsuited [11 10]}{:unsuited [11 9]}  {:unsuited [11 8]} {:unsuited [11 7]}] (fox/styled-cards-from-rank :unsuited "J" "7"))))

(deftest test-styled-connectors-from-rank
  (is (= [{:suited-connector [10 9]} {:suited-connector [9 8]} {:suited-connector [8 7]}] (fox/styled-connectors-from-rank :suited "T" "8")))
  (is (= [{:unsuited-connector [10 9]}] (fox/styled-connectors-from-rank :unsuited "T" "T")))
  (is (= [{:suited-connector [3 2]}] (fox/styled-connectors-from-rank :suited "3" "2"))))

(deftest test-pair?
  (is (fox/pair? ["AH" "AD"]))
  (is (fox/pair? ["AH" "AD" "KS" "QD" "JC"]))
  (is (not (fox/pair? ["AH" "KH"])))
  (is (not (fox/pair? ["AH" "AD" "KS" "KD" "JC"])))
  (is (not (fox/pair? ["AH" "AD" "KS" "KD" "KC"]))))

(deftest test-two-pair?
  (is (fox/two-pair? ["AH" "AD" "KS" "KD" "JC"]))
  (is (not (fox/two-pair? ["AH" "AD" "KS" "QD" "JC"])))
  (is (not (fox/two-pair? ["AH" "AD" "KS" "KD" "KC"]))))

(deftest test-trips?
  (is (fox/trips? ["AH" "AD" "AC" "QH" "JH"]))
  (is (not (fox/trips? ["AH" "AD" "AS" "AC" "QD"])))
  (is (not (fox/trips? ["AH" "AD" "QC" "QH" "QD"])))
  (is (not (fox/trips? ["AH" "AD" "QC" "QH" "JH"]))))

(deftest test-quads?
  (is (fox/quads? ["AH" "AD" "AC" "AS" "JH"]))
  (is (not (fox/quads? ["AH" "AD" "AC" "QH" "JH"]))))

(deftest test-flush?
  (is (fox/flush? ["AH" "QH" "TH" "8H" "JH"]))
  (is (not (fox/flush? ["AH" "KH" "TH" "QH" "JC"]))))

(deftest test-full-house?
  (is (fox/full-house? ["AH" "AD" "AC" "QH" "QD"]))
  (is (not (fox/full-house? ["AH" "AD" "AC" "AS" "JH"]))))

(deftest test-straight?
  (is (fox/straight? ["AD" "KS" "QC" "JH" "TD"]))
  (is (fox/straight? ["KD" "QD" "JD" "TD" "9D"]) "Straight flush is also a straight")
  (is (fox/straight? ["AD" "KD" "QD" "JD" "TD"]) "Royal flush is also a straight")
  (is (not (fox/straight? ["AD" "KS" "QC" "JH" "9D"]))))

(deftest test-straight-flush?
  (is (fox/straight-flush? ["KD" "QD" "JD" "TD" "9D"]))
  (is (fox/straight-flush? ["AD" "KD" "QD" "JD" "TD"]) "Royal flush is also a straight flush")
  (is (not (fox/straight-flush? ["AD" "KS" "QC" "JH" "TD"]))))

(deftest test-suited-connector?
  (is (fox/suited-connector? ["AH" "KH"] [14 13]))
  (is (fox/suited-connector? ["KH" "QH"] [13 12]))
  (is (fox/suited-connector? ["3H" "2H"] [3 2]))
  (is (fox/suited-connector? ["AH" "2H"] [14 2]))
  (is (not (fox/suited-connector? ["AH" "3H"] [14 3])))
  (is (not (fox/suited-connector? ["KH" "JH"] [13 11])))
  (is (not (fox/suited-connector? ["AH" "KS"] [14 13]))))

(deftest test-suited-one-gap?
  (is (fox/suited-one-gap? ["AH" "QH"] [14 12]))
  (is (fox/suited-one-gap? ["4H" "2H"] [4 2]))
  (is (fox/suited-one-gap? ["AH" "3H"] [14 3]))
  (is (not (fox/suited-connector? ["AH" "4H"] [14 4])))
  (is (not (fox/suited-connector? ["AH" "JH"] [14 11])))
  (is (not (fox/suited-connector? ["AH" "QS"] [14 12]))))

(deftest test-deal
  (let [{:keys [position action-to-you hand]} (fox/deal-hole)]
    (is (fox/positions position))
    (is (fox/actions-to-you action-to-you))
    (is (= 2 (count hand)))))

(deftest test-early
  (let [raising-hands [["AS" "AD"] ["KS" "KH"] ["QC" "QD"] ["JS" "JH"] ["TS" "TH"] ["AC" "KC"] ["AD" "KS"] ["AS" "QD"]]
        folding-hands [["AH" "JH"]]]
    (doseq [hand raising-hands
            action-to-you [:called :raised]]
      (is (= :raise (fox/action {:position :early :action-to-you action-to-you :hand hand}))
          (str "should raise early when " (name action-to-you) " to you with " hand)))
    (doseq [hand folding-hands
            action-to-you [:called :raised]]
      (is (= :fold (fox/action {:position :early :action-to-you action-to-you :hand hand}))
          (str "should fold early when " (name action-to-you) " to you with " hand)))))

(deftest test-blinds
  (testing "called to you"
    (let [raising-hands [["AS" "AD"] ["KS" "KH"] ["QC" "QD"] ["JS" "JH"] ["TS" "TC"] ["9S" "9H"] ["8C" "8D"]
                         ["AD" "KS"] ["AH" "QC"] ["AD" "JC"]]
          calling-hands [["AD" "TD"] ["KH" "JH"] ["KS" "TS"] ["QD" "TD"] ["KC" "QC"] ["QH" "JH"] ["JH" "TH"] ["9H" "8H"] ["7S" "6S"] ["5D" "4D"] ["3C" "2C"]]
          folding-hands [["7H" "7D"] ["6S" "6C"]
                         ["AD" "TC"] ["KS" "QC"] ["KD" "JH"] ["KH" "TS"] ["QS" "JH"] ["QC" "TD"] ["JS" "TH"]
                         ["9C" "8H"] ["7H" "6S"] ["5H" "4D"] ["3D" "2C"]]]
      (doseq [hand raising-hands]
        (is (= :raise (fox/action {:position :blind :action-to-you :called :hand hand}))
            (str "should raise in the blind when called to you with " hand)))
      (doseq [hand calling-hands]
        (is (= :call (fox/action {:position :blind :action-to-you :called :hand hand}))
            (str "should call in the blind when called to you with " hand)))
      (doseq [hand folding-hands]
        (is (= :fold (fox/action {:position :blind :action-to-you :called :hand hand}))
            (str "should fold in the blind when called to you with " hand)))))
  (testing "raised to you"
    (let [raising-hands [["AS" "AD"] ["KS" "KH"] ["QC" "QD"]
                         ["AD" "KS"]]
          calling-hands [["JS" "JH"] ["TS" "TC"] ["9S" "9H"] ["8C" "8D"] ["7H" "7D"]
                         ["KC" "QC"] ["QH" "JH"] ["JH" "TH"] ["9H" "8H"] ["7S" "6S"] ["5D" "4D"] ["3C" "2C"]
                         ["KH" "JH"] ["JS" "9S"] ["9C" "7C"] ["7D" "5D"]]
          folding-hands [["6S" "6C"]
                         ["AH" "QC"] ["AD" "JC"] ["AD" "TC"] ["KS" "QC"] ["KD" "JH"] ["KH" "TS"] ["QS" "JH"] ["QC" "TD"] ["JS" "TH"]
                         ["9C" "8H"] ["7H" "6S"] ["5H" "4D"] ["3D" "2C"]]]
      (doseq [hand raising-hands]
        (is (= :raise (fox/action {:position :blind :action-to-you :raised :hand hand}))
            (str "should raise in the blind when raised to you with " hand)))
      (doseq [hand calling-hands]
        (is (= :call (fox/action {:position :blind :action-to-you :raised :hand hand}))
            (str "should call in the blind when raised to you with " hand)))
      (doseq [hand folding-hands]
        (is (= :fold (fox/action {:position :blind :action-to-you :raised :hand hand}))
            (str "should fold in the blind when raised to you with " hand))))))

(deftest test-middle
  (testing "folded to you"
    (let [raising-hands [["AS" "AD"] ["KS" "KH"] ["QC" "QD"] ["JS" "JH"] ["TS" "TC"] ["9S" "9H"] ["8C" "8D"] ["7H" "7D"]
                         ["AD" "KS"] ["AH" "QC"] ["AD" "JC"]
                         ["AC" "KC"] ["AC" "QC"] ["AC" "JC"] ["AC" "TC"] ["AC" "QC"] ["AC" "JC"] ["AC" "TC"] ["QH" "JH"] ["QH" "TH"]]
          folding-hands [["AD" "TC"] ["KS" "QC"] ["KD" "JH"] ["KH" "TS"] ["QS" "JH"] ["QC" "TD"] ["JS" "TH"]
                         ["9C" "8H"] ["7H" "6S"] ["5H" "4D"] ["3D" "2C"]]]
      (doseq [hand raising-hands]
        (is (= :raise (fox/action {:position :middle :action-to-you :folded :hand hand}))
            (str "should raise in middle when folded to you with " hand)))
      (doseq [hand folding-hands]
        (is (= :fold (fox/action {:position :middle :action-to-you :folded :hand hand}))
            (str "should fold in middle when folded to you with " hand)))))
  (testing "called to you"
    (let [raising-hands [["AS" "AD"] ["KS" "KH"] ["QC" "QD"] ["JS" "JH"] ["TS" "TC"]
                         ["AD" "KS"] ["AH" "QC"] ["AD" "JC"]
                         ["AC" "KC"] ["AC" "QC"] ["AC" "JC"] ["AC" "QD"] ["AC" "JD"]]
          calling-hands [["9S" "9H"] ["2C" "2D"] ["QH" "TH"]
                         ["KC" "QC"] ["QH" "JH"] ["JH" "TH"] ["9H" "8H"] ["7S" "6S"] ["5D" "4D"] ["3C" "2C"]]]
      (doseq [hand raising-hands]
        (is (= :raise (fox/action {:position :middle :action-to-you :called :hand hand}))
            (str "should raise in middle when called to you with " hand)))
      (doseq [hand calling-hands]
        (is (= :call (fox/action {:position :middle :action-to-you :called :hand hand}))
            (str "should call in middle when called to you with " hand)))))
  (testing "raised to you"
    (let [raising-hands [["AS" "AD"] ["KS" "KH"] ["QC" "QD"] ["JS" "JH"] ["TS" "TC"]
                         ["AD" "KS"]]
          calling-hands [["9S" "9H"] ["8C" "8D"] ["7H" "7D"] ["6S" "6C"] ["5H" "5D"] ["4S" "4C"] ["3S" "3D"] ["2H" "2C"]
                         ["KC" "QC"] ["QH" "JH"] ["JH" "TH"] ["9H" "8H"] ["7S" "6S"] ["5D" "4D"] ["3C" "2C"]]
          folding-hands [["AD" "JC"] ["AD" "TC"] ["KS" "QC"] ["KD" "JH"] ["KH" "TS"] ["QS" "JH"] ["QC" "TD"] ["JS" "TH"]
                         ["9C" "8H"] ["7H" "6S"] ["5H" "4D"] ["3D" "2C"]]]
      (doseq [hand raising-hands]
        (is (= :raise (fox/action {:position :middle :action-to-you :raised :hand hand}))
            (str "should raise in middle position when raised to you with " hand)))
      (doseq [hand calling-hands]
        (is (= :call (fox/action {:position :middle :action-to-you :raised :hand hand}))
            (str "should call in middle position when raised to you with " hand)))
      (doseq [hand folding-hands]
        (is (= :fold (fox/action {:position :middle :action-to-you :raised :hand hand}))
            (str "should fold in middle position when raised to you with " hand))))))

(deftest test-late
  (testing "folded to you"
    (let [raising-hands [["AH" "AS"] ["TC" "TD"] ["7H" "7D"] ["2S" "2C"]
                         ["AH" "JH"] ["AS" "6S"] ["AC" "2C"]
                         ["AH" "KS"] ["AD" "7C"]
                         ["AC" "TH"] ["KS" "TH"] ["QH" "TD"] ["JH" "TC"]
                         ["KC" "QC"] ["QH" "JH"] ["JH" "TH"] ["9H" "8H"] ["7S" "6S"] ["5D" "4D"] ["3C" "2C"]]]
      (doseq [hand raising-hands]
        (is (= :raise (fox/action {:position :late :action-to-you :folded :hand hand}))
            (str "should raise in late position when folded to you with " hand)))))
  (testing "called to you"
    (let [raising-hands [["AH" "AS"] ["TC" "TD"] ["AH" "KS"] ["AD" "JC"]]
          calling-hands [["9S" "9H"] ["8C" "8D"] ["7H" "7D"] ["6S" "6C"] ["5H" "5D"] ["4S" "4C"] ["3S" "3D"] ["2H" "2C"]
                         ["KH" "9H"] ["QH" "9H"] ["JH" "9H"]
                         ["AH" "TH"] ["AS" "6S"] ["AC" "2C"]
                         ["KC" "QC"] ["QH" "JH"] ["JH" "TH"] ["9H" "8H"] ["7S" "6S"] ["5D" "4D"] ["3C" "2C"]]]
      (doseq [hand raising-hands]
        (is (= :raise (fox/action {:position :late :action-to-you :called :hand hand}))
            (str "should raise in late position when called to you with " hand)))
      (doseq [hand calling-hands]
        (is (= :call (fox/action {:position :late :action-to-you :called :hand hand}))
            (str "should call in late position when called to you with " hand)))))
  (testing "raised to you"
    (let [raising-hands [["AH" "AS"] ["TC" "TD"] ["AH" "KS"] ["AD" "QC"]
                         ]
          calling-hands [["9S" "9H"] ["8C" "8D"] ["7H" "7D"]
                         ["KC" "QC"] ["QH" "JH"] ["JH" "TH"] ["9H" "8H"] ["7S" "6S"] ["5D" "4D"] ["3C" "2C"]]]
      (doseq [hand raising-hands]
        (is (= :raise (fox/action {:position :late :action-to-you :raised :hand hand}))
            (str "should raise in late position when called to you with " hand)))
      (doseq [hand calling-hands]
        (is (= :call (fox/action {:position :late :action-to-you :raised :hand hand}))
            (str "should call in late position when raised to you with " hand))))))

(deftest test-low-limit-strategy
  (testing "early"
    (testing "called to you"
      (let [raising-hands [["AS" "AH"] ["JC" "JD"]
                           ["AS" "KS"] ["AH" "JH"]
                           ["AS" "KH"] ["AD" "QC"] ["KH" "QH"]]
            calling-hands [["TS" "TH"] ["7D" "7C"]
                           ["KS" "JS"] ["QH" "JH"] ["JD" "TD"]
                           ["AC" "TC"] ["AS" "9S"]
                           ["KS" "QH"]]
            folding-hands [["6S" "6H"] ["KS" "JH"] ["AD" "8D"]]]
        (doseq [hand raising-hands]
          (is (= :raise (fox/action {:game-type :low-limit :position :early :action-to-you :called :hand hand}))
              (str "in low-limit, should raise in early position when called to you with " hand)))
        (doseq [hand calling-hands]
          (is (= :call (fox/action {:game-type :low-limit :position :early :action-to-you :called :hand hand}))
              (str "in low-limit, should call in early position when called to you with " hand)))
        (doseq [hand folding-hands]
          (is (= :fold (fox/action {:game-type :low-limit :position :early :action-to-you :called :hand hand}))
              (str "in low-limit, should fold in early position when called to you with " hand)))))
    (testing "raised to you"
      (let [raising-hands [["AS" "AH"] ["JC" "JD"]
                           ["AS" "KS"] ["AS" "KH"] ["AD" "QC"]]
            calling-hands [["AH" "JH"] ["KH" "QH"]
                           ["TS" "TH"] ["7D" "7C"]
                           ["KS" "JS"] ["QH" "JH"] ["JD" "TD"]
                           ["AC" "TC"] ["AS" "9S"]
                           ["KS" "QH"]]
            folding-hands [["6S" "6H"] ["KS" "JH"] ["AD" "8D"]]]
        (doseq [hand raising-hands]
          (is (= :raise (fox/action {:game-type :low-limit :position :early :action-to-you :raised :hand hand}))
              (str "in low-limit, should raise in early position when raised to you with " hand)))
        (doseq [hand calling-hands]
          (is (= :call (fox/action {:game-type :low-limit :position :early :action-to-you :raised :hand hand}))
              (str "in low-limit, should call in early position when raised to you with " hand)))
        (doseq [hand folding-hands]
          (is (= :fold (fox/action {:game-type :low-limit :position :early :action-to-you :raised :hand hand}))
              (str "in low-limit, should fold in early position when raised to you with " hand))))))
  (testing "middle"
    (testing "called to you"
      (testing "3 or fewer callers"
        (let [raising-hands [["AS" "AH"] ["8C" "8D"]
                             ["AS" "KH"] ["AC" "TD"]
                             ["KS" "QH"] ["KC" "JC"]
                             ["QS" "JS"]]
              calling-hands [["7S" "7H"] ["5D" "5C"]
                             ["JS" "TS"] ["9H" "8H"]
                             ["AD" "2D"] ["KC" "8C"]
                             ["KS" "JH"] ["QD" "JC"] ["JH" "TD"]]
              folding-hands [["4S" "4H"]]]
          (doseq [hand raising-hands]
            (is (= :raise (fox/action {:game-type :low-limit :position :middle :action-to-you :called :action-count 3 :hand hand}))
                (str "in low-limit, should raise in middle position when 3 or fewer callers with " hand)))
          (doseq [hand calling-hands]
            (is (= :call (fox/action {:game-type :low-limit :position :middle :action-to-you :called :action-count 3 :hand hand}))
                (str "in low-limit, should call in middle position when 3 or fewer callers with " hand)))
          (doseq [hand folding-hands]
            (is (= :fold (fox/action {:game-type :low-limit :position :middle :action-to-you :called :action-count 3 :hand hand}))
                (str "in low-limit, should fold in middle position when 3 or fewer callers with " hand)))))
      (testing "4 or more callers"
        (let [raising-hands [["AS" "AH"] ["8C" "8D"]
                             ["AS" "KH"] ["AC" "TD"]
                             ["KS" "QH"] ["KC" "JC"]
                             ["QS" "JS"]]
              calling-hands [["7S" "7H"] ["2D" "2C"]
                             ["JS" "TS"] ["7H" "6H"]
                             ["AD" "2D"] ["KC" "7C"] ["TH" "8H"]
                             ["KS" "JH"] ["QD" "JC"] ["JH" "TD"]]
              folding-hands [["KS" "7H"] ["KD" "6D"]]]
          (doseq [hand raising-hands]
            (is (= :raise (fox/action {:game-type :low-limit :position :middle :action-to-you :called :action-count 4 :hand hand}))
                (str "in low-limit, should raise in middle position when 4 or more callers with " hand)))
          (doseq [hand calling-hands]
            (is (= :call (fox/action {:game-type :low-limit :position :middle :action-to-you :called :action-count 4 :hand hand}))
                (str "in low-limit, should call in middle position when 4 or more callers with " hand)))
          (doseq [hand folding-hands]
            (is (= :fold (fox/action {:game-type :low-limit :position :middle :action-to-you :called :action-count 4 :hand hand}))
                (str "in low-limit, should fold in middle position when 4 or more callers with " hand))))))
    (testing "raised to you"
      (let [raising-hands [["AS" "AH"] ["JC" "JD"]
                           ["AS" "KS"] ["AD" "KC"] ["AD" "QC"]]
            calling-hands [["TS" "TH"] ["9D" "9C"]
                           ["KS" "QS"] ["QH" "JH"]
                           ["AD" "JD"] ["KC" "JC"]]
            folding-hands [["8S" "8H"]]]
        (doseq [hand raising-hands]
          (is (= :raise (fox/action {:game-type :low-limit :position :middle :action-to-you :raised :hand hand}))
              (str "in low-limit, should raise in middle position when raised with " hand)))
        (doseq [hand calling-hands]
          (is (= :call (fox/action {:game-type :low-limit :position :middle :action-to-you :raised :hand hand}))
              (str "in low-limit, should call in middle position when raised with " hand)))
        (doseq [hand folding-hands]
          (is (= :fold (fox/action {:game-type :low-limit :position :middle :action-to-you :raised :hand hand}))
              (str "in low-limit, should fold in middle position when raised with " hand))))))
  (testing "late"
    (testing "called to you"
      (testing "4 or fewer callers"
        (let [raising-hands [["AS" "AH"] ["8C" "8D"]
                             ["AD" "KD"] ["AC" "8C"]
                             ["AS" "KH"] ["AC" "9D"]
                             ["KS" "QH"] ["KC" "JC"]
                             ["QS" "JS"]]
              calling-hands [["7S" "7H"] ["5D" "5C"]
                             ["JS" "TS"] ["5H" "4H"]
                             ["QD" "TD"] ["7C" "5C"]
                             ["AD" "2D"] ["KC" "2C"]
                             ["QD" "JC"] ["JH" "TD"] ["KS" "JH"] ["QD" "TC"] ["KS" "TH"]]
              folding-hands [["KS" "9H"] ["QS" "9S"]]]
          (doseq [hand raising-hands]
            (is (= :raise (fox/action {:game-type :low-limit :position :late :action-to-you :called :action-count 4 :hand hand}))
                (str "in low-limit, should raise in late position when 4 or fewer callers with " hand)))
          (doseq [hand calling-hands]
            (is (= :call (fox/action {:game-type :low-limit :position :late :action-to-you :called :action-count 4 :hand hand}))
                (str "in low-limit, should call in late position when 4 or fewer callers with " hand)))
          (doseq [hand folding-hands]
            (is (= :fold (fox/action {:game-type :low-limit :position :late :action-to-you :called :action-count 4 :hand hand}))
                (str "in low-limit, should fold in late position when 4 or fewer callers with " hand)))))
      (testing "5 or more callers"
        (let [raising-hands [["AS" "AH"] ["8C" "8D"]
                             ["AD" "KD"] ["AC" "8C"]
                             ["AS" "KH"] ["AC" "9D"]
                             ["KS" "QH"] ["KC" "JC"]
                             ["QS" "JS"]]
              calling-hands [["7S" "7H"] ["2D" "2C"]
                             ["JS" "TS"] ["5H" "4H"]
                             ["QH" "TH"] ["5C" "3C"]
                             ["AD" "2D"] ["KC" "2C"] ["QH" "2H"]
                             ["QD" "JC"] ["JH" "TD"] ["KS" "JH"] ["QD" "TC"] ["KS" "TH"]]
              folding-hands [["KS" "9H"] ["4S" "3S"] ]]
          (doseq [hand raising-hands]
            (is (= :raise (fox/action {:game-type :low-limit :position :late :action-to-you :called :action-count 5 :hand hand}))
                (str "in low-limit, should raise in late position when 5 or more callers with " hand)))
          (doseq [hand calling-hands]
            (is (= :call (fox/action {:game-type :low-limit :position :late :action-to-you :called :action-count 5 :hand hand}))
                (str "in low-limit, should call in late position when 5 or more callers with " hand)))
          (doseq [hand folding-hands]
            (is (= :fold (fox/action {:game-type :low-limit :position :late :action-to-you :called :action-count 5 :hand hand}))
                (str "in low-limit, should fold in late position when 5 or more callers with " hand))))))
    (testing "raised to you"
      (let [raising-hands [["AS" "AH"] ["JC" "JD"]
                           ["AS" "KS"] ["AD" "KC"] ["AD" "QC"]]
            calling-hands [["TS" "TH"] ["9D" "9C"]
                           ["KS" "QS"] ["QH" "JH"]
                           ["AD" "JD"] ["KC" "JC"]]
            folding-hands [["8S" "8H"]]]
        (doseq [hand raising-hands]
          (is (= :raise (fox/action {:game-type :low-limit :position :late :action-to-you :raised :hand hand}))
              (str "in low-limit, should raise in late position when raised with " hand)))
        (doseq [hand calling-hands]
          (is (= :call (fox/action {:game-type :low-limit :position :late :action-to-you :raised :hand hand}))
              (str "in low-limit, should call in late position when raised with " hand)))
        (doseq [hand folding-hands]
          (is (= :fold (fox/action {:game-type :low-limit :position :late :action-to-you :raised :hand hand}))
              (str "in low-limit, should fold in late position when raised with " hand))))))

  (testing "blinds"
    (testing "called to you"
      (let [raising-hands [["AS" "AH"] ["8D" "8C"]
                          ["AH" "KH"] ["QD" "JD"]
                          ["AH" "QH"] ["KC" "JC"]
                          ["AS" "KH"] ["KD" "QC"] ["AS" "QH"]]
            calling-hands [["8C" "4D"] ["7S" "2H"]]]
        (doseq [hand raising-hands]
          (is (= :raise (fox/action {:game-type :low-limit :position :blind :action-to-you :called :hand hand}))
              (str "in low-limit, should raise in the blind when called with " hand)))
        (doseq [hand calling-hands]
          (is (= :call (fox/action {:game-type :low-limit :position :blind :action-to-you :called :hand hand}))
              (str "in low-limit, should call in late position when raised with " hand)))))
    (testing "raised to you"
      (let [raising-hands [["AS" "AH"] ["8D" "8C"]
                          ["AH" "KH"] ["QD" "JD"]
                          ["AH" "QH"] ["KC" "JC"]
                          ["AS" "KH"] ["KD" "QC"] ["AS" "QH"]]
            folding-hands [["7C" "7D"] ["7S" "2H"] ["AS" "JS"]]]
        (doseq [hand raising-hands]
          (is (= :raise (fox/action {:game-type :low-limit :position :blind :action-to-you :raised :hand hand}))
              (str "in low-limit, should raise in the blind when raised with " hand)))
        (doseq [hand folding-hands]
          (is (= :fold (fox/action {:game-type :low-limit :position :blind :action-to-you :raised :hand hand}))
              (str "in low-limit, should fold in late position when raised with " hand)))))))

(deftest test-action-with-description
  (testing "early"
    (is (= {:correct-action :raise
            :description "In early position, when called or raised to you, you should raise with AA - TT, AK, AQ."}
           (fox/action-with-description {:game-type :no-limit
                                         :position :early
                                         :street :pre-flop
                                         :action-to-you :raised
                                         :action-count fox/player-count-na
                                         :hand ["AS" "AH"]}))))
  (testing "blinds"
    (is (= {:correct-action :raise
            :description "In the blind, when called to you, you should raise with AA - 88, AK - AJ. You should call with suited connectors or suited big cards (T or better)."}
           (fox/action-with-description {:game-type :no-limit
                                         :position :blind
                                         :street :pre-flop
                                         :action-to-you :called
                                         :action-count fox/player-count-na
                                         :hand ["AS" "AH"]})))
    (is (= {:correct-action :raise
            :description "In the blind, when raised to you, you should raise with AA - QQ, AK. You should call with JJ - 77, suited connectors, or suited one gaps, provided you have a lot of chips."}
           (fox/action-with-description {:game-type :no-limit
                                         :position :blind
                                         :street :pre-flop
                                         :action-to-you :raised
                                         :action-count fox/player-count-na
                                         :hand ["AS" "AH"]}))))
  (testing "middle"
    (is (= {:correct-action :raise
            :description "In middle position, when folded to you, you should raise with AA - 77, AK - AJ, or suited cards QT or better. DON’T LIMP!"}
           (fox/action-with-description {:game-type :no-limit
                                         :position :middle
                                         :street :pre-flop
                                         :action-to-you :folded
                                         :action-count fox/player-count-na
                                         :hand ["AS" "AH"]})))
    (is (= {:correct-action :call
            :description "In middle position, when called to you, you should raise with AA - TT, AK - AJ. You should call with 99 - 22, suited cards QT or better, or suited connectors."}
           (fox/action-with-description {:game-type :no-limit
                                         :position :middle
                                         :street :pre-flop
                                         :action-to-you :called
                                         :action-count fox/player-count-na
                                         :hand ["9S" "9H"]})))
    (is (= {:correct-action :raise
            :description "In middle position, when raised to you, you should raise with AA - TT, AK, AQ. You should call with 99 - 22 or suited connectors."}
           (fox/action-with-description {:game-type :no-limit
                                         :position :middle
                                         :street :pre-flop
                                         :action-to-you :raised
                                         :action-count fox/player-count-na
                                         :hand ["AS" "AH"]}))))
  (testing "late"
    (is (= {:correct-action :raise
            :description "In late position, when folded to you, you should raise with AA - 22, AX suited, A7 or better, big cards (T or better), or suited connectors."}
           (fox/action-with-description {:game-type :no-limit
                                         :position :late
                                         :street :pre-flop
                                         :action-to-you :folded
                                         :action-count fox/player-count-na
                                         :hand ["AS" "AH"]})))
    (is (= {:correct-action :call
            :description "In late position, when called to you, you should raise with AA - TT, AK - AJ. You should call with 99 - 22, AX suited, suited cards 9 or better, or suited connectors."}
           (fox/action-with-description {:game-type :no-limit
                                         :position :late
                                         :street :pre-flop
                                         :action-to-you :called
                                         :action-count fox/player-count-na
                                         :hand ["9S" "9H"]})))
    (is (= {:correct-action :raise
            :description "In late position, when raised to you, you should raise with AA - TT, AK, AQ. You should call with 99 - 77 or suited connectors."}
           (fox/action-with-description {:game-type :no-limit
                                         :position :late
                                         :street :pre-flop
                                         :action-to-you :raised
                                         :action-count fox/player-count-na
                                         :hand ["AS" "AH"]})))))

(deftest test-top-two-pair?
  (is (fox/top-two-pair? ["3H" "AH" "AS" "7C" "7D"]))
  (is (not (fox/top-two-pair? ["AH" "AS" "7C" "3D" "3H"])))
  (is (not (fox/top-two-pair? ["KS" "JS" "9S" "7S" "5D"]))))

(deftest test-overpair?
  (is (fox/overpair? ["8H" "8S"] ["7C" "5D" "3H"]))
  (is (not (fox/overpair? ["6H" "6S"] ["7C" "5D" "3H"])))
  (is (not (fox/overpair? ["AH" "KS"] ["7C" "5D" "3H"]))))

(deftest test-top-pair-and-kicker?
  (is (fox/top-pair-and-kicker? ["AH" "8S"] ["8C" "5D" "3H"]))
  (is (fox/top-pair-and-kicker? ["AH" "KS"] ["AC" "5D" "3H"]))
  (is (not (fox/top-pair-and-kicker? ["KH" "QS"] ["KC" "5D" "3H"])))
  (is (not (fox/top-pair-and-kicker? ["AH" "AS"] ["8C" "5D" "3H"])))
  (is (not (fox/top-pair-and-kicker? ["AH" "5S"] ["8C" "5D" "3H"])))
  (is (not (fox/top-pair-and-kicker? ["AC" "QD"] ["AH" "TC" "7C"]))))

(deftest test-top-pair-low-pair?
  (is (fox/top-pair-low-pair? ["AH" "QS"] ["AC" "KD" "QH"]))
  (is (not (fox/top-pair-low-pair? ["AH" "KS"] ["AD" "KC" "QD"])))
  (is (not (fox/top-pair-low-pair? ["AH" "KS"] ["KC" "QD" "3H"])))
  (is (not (fox/top-pair-low-pair? ["KH" "TS"] ["KC" "QD" "3H"]))))

(deftest test-very-strong-made-hand?
  (let [very-strong-hands [["AH" "AS" "AC" "AD" "3H"]
                           ["AH" "AS" "AC" "5D" "5H"]
                           ["KS" "JS" "9S" "7S" "5S"]
                           ["KS" "QC" "JD" "TH" "9S"]
                           ["AH" "AS" "AC" "5D" "3H"]
                           ["AH" "AS" "7C" "7D" "3H"]]]
    (doseq [hand very-strong-hands]
      (is (fox/very-strong-made-hand? hand)))))

(deftest test-strong-made-hand?
  (is (fox/strong-made-hand? ["8H" "8S"] ["7C" "5D" "3H"]))
  (is (fox/strong-made-hand? ["AH" "8S"] ["8C" "5D" "3H"]))
  (is (fox/strong-made-hand? ["AH" "QS"] ["AC" "KD" "QH"]))
  (is (not (fox/strong-made-hand? ["AH" "5S"] ["8C" "5D" "3H"])))
  (is (not (fox/strong-made-hand? ["AC" "QD"] ["AH" "TC" "7C"]))))

(deftest test-mediocre-made-hand?
  (is (fox/mediocre-made-hand? ["KH" "7S"] ["7C" "5D" "3H"]))
  (is (not (fox/mediocre-made-hand? ["AH" "KS"] ["AC" "KD" "QH"]))))

(deftest test-made-hand?
  (is (fox/made-hand? ["AH" "AS"] ["7C" "5D" "3H"]) "pair should be made")
  (is (fox/made-hand? ["AH" "AS"] ["7C" "7D" "3H"]) "two pair should be made")
  (is (fox/made-hand? ["AH" "AS"] ["AC" "5D" "3H"]) "trips should be made")
  (is (fox/made-hand? ["AH" "AS"] ["AC" "AD" "3H"]) "quads should be made")
  (is (fox/made-hand? ["KS" "QC"] ["JD" "TH" "9S"]) "straight should be made")
  (is (fox/made-hand? ["KS" "JS"] ["9S" "7S" "5S"]) "flush should be made")
  (is (fox/made-hand? ["AH" "AS"] ["AC" "5D" "5H"]) "full house should be made")
  (is (not (fox/made-hand? ["KS" "JS"] ["9S" "7S" "5D"])) "trash should not be made")
  (is (not (fox/made-hand? ["KS" "QC"] ["JD" "TH" "8S"])) "draw should not be made"))

(deftest test-hand-category
  (let [very-strong-hands [{:hole ["AH" "AS"] :flop ["AC" "AD" "3H"]}
                           {:hole ["AH" "AS"] :flop ["AC" "5D" "5H"]}
                           {:hole ["KS" "JS"] :flop ["9S" "7S" "5S"]}
                           {:hole ["KS" "QC"] :flop ["JD" "TH" "9S"]}
                           {:hole ["AH" "AS"] :flop ["AC" "5D" "3H"]}
                           {:hole ["AH" "AS"] :flop ["7C" "7D" "3H"]}]
        strong-hands [{:hole ["8H" "8S"] :flop ["7C" "5D" "3H"]}
                      {:hole ["AH" "QS"] :flop ["AC" "KD" "QH"]}
                      {:hole ["AH" "8S"] :flop ["8C" "5D" "3H"]}]
        mediocre-hands [{:hole ["KH" "7S"] :flop ["7C" "5D" "3H"]}
                        {:hole ["AH" "5S"] :flop ["8C" "5D" "3H"]}
                        {:hole ["AH" "3S"] :flop ["8C" "5D" "3H"]}
                        {:hole ["KH" "KS"] :flop ["AC" "5D" "3H"]}
                        {:hole ["AC" "QD"] :flop ["AH" "TC" "7C"]}]]
    (doseq [{:keys [hole flop]} very-strong-hands]
      (is (= :very-strong (fox/hand-category hole flop)) (str "should be very strong:" hole flop)))
    (doseq [{:keys [hole flop]} strong-hands]
      (is (= :strong (fox/hand-category hole flop)) (str "should be strong:" hole flop)))
    (doseq [{:keys [hole flop]} mediocre-hands]
      (is (= :mediocre (fox/hand-category hole flop)) (str "should be mediocre:" hole flop)))))
