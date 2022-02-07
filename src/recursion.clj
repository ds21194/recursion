(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn recursion-level [seq h]
  (cond
    (and (empty? seq) (== h 1)) true
    (or (> h 1) (empty? seq)) false
    :else (recursion-level (rest seq) (inc h))
    ))

(defn singleton? [coll]
  (recursion-level coll 0)
  )

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (empty? (rest coll)) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))
    ))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))
    ))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))
    ))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else (empty ())
    ))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (seq a-seq)
    ))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
    ))

(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1) (empty? seq-2)) (empty ())
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    (empty ())
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))
    ))

(defn my-range [up-to]
  (if (<= up-to 0)
    (empty ())
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq (empty ()))
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [reversed-tails (tails (reverse a-seq))]
    (reverse (map reverse reversed-tails))
    ))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (map concat (butlast (tails a-seq)) (butlast (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (if (contains? freqs (first a-seq))
                     (conj freqs {(first a-seq) (inc (get freqs (first a-seq)))})
                     (conj freqs {(first a-seq) 1}))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    (empty ())
    (let [[key value] (first a-map)]
      (concat (repeat value key) (un-frequencies (rest a-map))))
    ))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (> n 0) (my-drop (- n 1) (rest coll))
    :else (cons (first coll) (my-drop n (rest coll)))
    ))

(defn halve [a-seq]
  (let [f-seq-len (int (/ (count a-seq) 2))
        f-seq (my-take f-seq-len a-seq)
        s-seq (my-drop f-seq-len a-seq)]
    [f-seq s-seq]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) '()
    (empty? a-seq) (cons (first b-seq) (rest b-seq))
    (empty? b-seq) (cons (first a-seq) (rest a-seq))
    :else (if (< (first a-seq) (first b-seq))
            (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
            (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (let [a-seq-len (count a-seq)]
    (if (or (== a-seq-len 0) (== a-seq-len 1)) a-seq
      (let [[half-1 half-2] (halve a-seq)]
        (seq-merge (merge-sort half-1) (merge-sort half-2))))))

; TODO: finish the rest
(defn split-into-monotonics [a-seq]
  (let [monotonic? (fn [seq] (or (apply <= seq) (apply >= seq)))
        largest-mono (last (take-while monotonic? (inits a-seq)))]
    (if (empty? a-seq)
      '()
      (cons largest-mono (split-into-monotonics (drop (count largest-mono) a-seq))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

