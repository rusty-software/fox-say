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
          (str "should raise utg when raised with " hand)))
    (doseq [hand folding-hands]
      (is (= :fold (fox/action {:position :utg :action-to-you :raised :hand hand}))
          (str "should fold utg when raised with " hand)))))

(deftest test-blinds
  (testing "called to you"
    (let [raising-hands [["AS" "AD"] ["KS" "KH"] ["QC" "QD"] ["JS" "JH"] ["TS" "TC"] ["9S" "9H"] ["8C" "8D"] ["7H" "7D"]
                         ["AD" "KS"] ["AH" "QC"] ["AD" "JC"]]
          calling-hands [["AD" "TD"] ["KC" "QC"] ["KH" "JH"] ["KS" "TS"] ["QH" "JH"] ["QD" "TD"] ["JH" "TH"]
                         ["9H" "8H"] ["7S" "6S"] ["5D" "4D"] ["3C" "2C"]]
          folding-hands [["AD" "TC"] ["KS" "QC"] ["KD" "JH"] ["KH" "TS"] ["QS" "JH"] ["QC" "TD"] ["JS" "TH"]
                         ["9C" "8H"] ["7H" "6S"] ["5H" "4D"] ["3D" "2C"]]]
      (doseq [hand raising-hands]
        (is (= :raise (fox/action {:position :blind :action-to-you :called :hand hand}))
            (str "should raise in the blind when called with " hand)))
      (doseq [hand calling-hands]
        (is (= :call (fox/action {:position :blind :action-to-you :called :hand hand}))
            (str "should call in the blind when called with " hand)))
      (doseq [hand folding-hands]
        (is (= :fold (fox/action {:position :blind :action-to-you :called :hand hand}))
            (str "should fold in the blind when called with " hand)))))
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
            (str "should raise in the blind when raised with " hand)))
      (doseq [hand calling-hands]
        (is (= :call (fox/action {:position :blind :action-to-you :raised :hand hand}))
            (str "should call in the blind when raised with " hand)))
      (doseq [hand folding-hands]
        (is (= :fold (fox/action {:position :blind :action-to-you :raised :hand hand}))
            (str "should fold in the blind when raised with " hand))))))

(deftest test-middle
  (testing "folded to you")
  (testing "called to you")
  (testing "raised to you"))

(deftest test-late
  (testing "folded to you")
  (testing "called to you")
  (testing "raised to you"))
