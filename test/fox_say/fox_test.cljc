(ns fox-say.fox-test
  (:require [clojure.test :refer [deftest testing is]]
            [fox-say.fox :as fox]))

(def ranks ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"])

(def suits ["S" "H" "C" "D"])

(def fresh-deck
  (for [rank ranks
        suit suits]
    (str rank suit)))

(comment
  (let [deck (shuffle fresh-deck)]
      (loop [deck deck]
        (let [hand (take 2 deck)]
          (when (seq hand)
            (println "hand" hand)
            (recur (drop 2 deck)))))))

(deftest test-utg
  (let [raising-hands [["AS" "AD"] ["KS" "KH"] ["QC" "QD"] ["JS" "JH"] ["TS" "TC"] ["AD" "KS"] ["AH" "QC"]]]
    (doseq [hand raising-hands]
      (is (= :raise (fox/action {:position :utg :action-to-you :called :hand hand}))
          (str "should raise utg when called with " hand))))
  (let [raising-hands [["AS" "AD"] ["KS" "KH"] ["QC" "QD"] ["JS" "JH"] ["AD" "KS"]]
        folding-hands [["AH" "QC"]]]
    (doseq [hand raising-hands]
      (is (= :raise (fox/action {:position :utg :action-to-you :raised :hand hand}))
          (str "should raise utg when raised to you with " hand)))
    (doseq [hand folding-hands]
      (is (= :fold (fox/action {:position :utg :action-to-you :raised :hand hand}))
          (str "should fold utg when raised to you with " hand)))))

(deftest test-blinds
  (testing "called to you"
    (let [raising-hands [["AS" "AD"] ["KS" "KH"] ["QC" "QD"] ["JS" "JH"] ["TS" "TC"] ["9S" "9H"] ["8C" "8D"]
                         ["AD" "KS"] ["AH" "QC"] ["AD" "JC"]]
          calling-hands [["AD" "TD"] ["KC" "QC"] ["KH" "JH"] ["KS" "TS"] ["QH" "JH"] ["QD" "TD"] ["JH" "TH"]
                         ["9H" "8H"] ["7S" "6S"] ["5D" "4D"] ["3C" "2C"]]
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
          calling-hands [["JS" "JH"] ["TS" "TC"] ["9S" "9H"] ["8C" "8D"] ["7H" "7D"] ["6S" "6C"]
                         ["KC" "QC"] ["QH" "JH"] ["JH" "TH"] ["9H" "8H"] ["7S" "6S"] ["5D" "4D"] ["3C" "2C"]
                         ["KH" "JH"] ["JS" "9S"] ["9C" "7C"] ["7D" "5D"]]
          folding-hands [["AH" "QC"] ["AD" "JC"] ["AD" "TC"] ["KS" "QC"] ["KD" "JH"] ["KH" "TS"] ["QS" "JH"] ["QC" "TD"] ["JS" "TH"]
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
        (is (= :fold (fox/action {:position :middle :action-to-you :called :hand hand}))
            (str "should fold in middle when folded to you with " hand)))))
  (testing "called to you"
    (let [calling-hands [["AS" "AD"] ["KS" "KH"] ["QC" "QD"] ["JS" "JH"] ["TS" "TC"] ["9S" "9H"]
                         ["AC" "KC"] ["AC" "QC"] ["AC" "JC"] ["AC" "TC"] ["AC" "QC"] ["AC" "JC"] ["AC" "TC"] ["QH" "JH"] ["QH" "TH"]
                         ["KC" "QC"] ["QH" "JH"] ["JH" "TH"] ["9H" "8H"] ["7S" "6S"] ["5D" "4D"] ["3C" "2C"]]]
      (doseq [hand calling-hands]
        (is (= :call (fox/action {:position :middle :action-to-you :called :hand hand}))
            (str "should call in middle when called to you with " hand)))))
  (testing "raised to you"
    (let [raising-hands [["AS" "AD"] ["KS" "KH"] ["QC" "QD"]
                         ["AD" "KS"]]
          calling-hands [["JS" "JH"] ["TS" "TC"] ["9S" "9H"] ["8C" "8D"] ["7H" "7D"] ["6S" "6C"] ["5H" "5D"] ["4S" "4C"] ["3S" "3D"] ["2H" "2C"]
                         ["KC" "QC"] ["QH" "JH"] ["JH" "TH"] ["9H" "8H"] ["7S" "6S"] ["5D" "4D"] ["3C" "2C"]]
          folding-hands [["AH" "QC"] ["AD" "JC"] ["AD" "TC"] ["KS" "QC"] ["KD" "JH"] ["KH" "TS"] ["QS" "JH"] ["QC" "TD"] ["JS" "TH"]
                         ["9C" "8H"] ["7H" "6S"] ["5H" "4D"] ["3D" "2C"]]]
      (doseq [hand raising-hands]
        (is (= :raise (fox/action {:position :middle :action-to-you :raised :hand hand}))
            (str "should raise in middle position when raised to you with " hand)))
      (doseq [hand calling-hands]
        (is (= :call (fox/action {:position :blind :action-to-you :raised :hand hand}))
            (str "should call in middle position when raised to you with " hand)))
      (doseq [hand folding-hands]
        (is (= :fold (fox/action {:position :blind :action-to-you :raised :hand hand}))
            (str "should fold in middle position when raised to you with " hand))))))

(deftest test-late
  (testing "folded to you")
  (testing "called to you")
  (testing "raised to you"))
