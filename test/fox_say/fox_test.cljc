(ns fox-say.fox-test
  (:require [clojure.test :refer [deftest testing is]]
            [fox-say.fox :as fox]))

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
          calling-hands [["AD" "TD"] ["KH" "JH"] ["KS" "TS"] ["QD" "TD"]
                         ["KC" "QC"] ["QH" "JH"] ["JH" "TH"]
                         ["9H" "8H"] ["7S" "6S"] ["5D" "4D"] ["3C" "2C"]
                         ]
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
    (let [raising-hands [["AH" "AS"] ["TC" "TD"] ["AH" "KS"] ["AD" "QC"]]
          calling-hands [["9S" "9H"] ["8C" "8D"] ["7H" "7D"]
                         ["KC" "QC"] ["QH" "JH"] ["JH" "TH"] ["9H" "8H"] ["7S" "6S"] ["5D" "4D"] ["3C" "2C"]]]
      (doseq [hand raising-hands]
        (is (= :raise (fox/action {:position :late :action-to-you :raised :hand hand}))
            (str "should raise in late position when called to you with " hand)))
      (doseq [hand calling-hands]
        (is (= :call (fox/action {:position :late :action-to-you :raised :hand hand}))
            (str "should call in late position when raised to you with " hand))))))

(deftest test-deal
  (let [{:keys [position action-to-you hand]} (fox/deal)]
    (is (fox/positions position))
    (is (fox/actions-to-you action-to-you))
    (is (= 2 (count hand)))))
