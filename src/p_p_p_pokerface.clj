(ns p-p-p-pokerface)

(defn rank [[fst _]]
  (if (Character/isDigit fst)
    (Integer/valueOf (str fst))
    (get {\T 10, \J 11, \Q 12, \K 13, \A 14} fst)))

(defn suit [[_ snd]]
  (str snd))

(defn pair? [hand]
  (let [values (map rank hand)
        freq (vals (frequencies values))]
    (>= (count (filter (fn [x] (== x 2)) freq)) 1)))

(defn three-of-a-kind? [hand]
  (let [values (map rank hand)
        freq (vals (frequencies values))]
    (== (count (filter (fn [x] (== x 3)) freq)) 1)))

(defn four-of-a-kind? [hand]
  (let [values (map rank hand)
        freq (vals (frequencies values))]
    (== (count (filter (fn [x] (== x 4)) freq)) 1)))

(defn flush? [hand]
  (let [values (map suit hand)
        freq (vals (frequencies values))]
    (== (count (filter (fn [x] (== x 5)) freq)) 1)))

(defn full-house? [hand]
  (let [values (map rank hand)
        freq (vals (frequencies values))]
    (and (== (count (filter (fn [x] (== x 3)) freq)) 1)
         (== (count (filter (fn [x] (== x 2)) freq)) 1))))

(defn two-pairs? [hand]
  (let [values (map rank hand)
        freq (vals (frequencies values))]
    (or (== (count (filter (fn [x] (== x 2)) freq)) 2)
        (four-of-a-kind? hand))))


(defn straight? [hand]
  (let [values1 (sort (map rank hand))
        first1 (first values1)
        values2 (sort (replace {14 1} values1))
        first2 (first values2)]
    (or (= values1 (range first1 (+ first1 5)))
        (= values2 (range first2 (+ first2 5))))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        results (map (fn [[func res]] (if (func hand) res 0)) checkers)]
    (apply max results)))
