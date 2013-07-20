(ns powerset.core)

(defn powerset1 
  "uses mapcat, split-at, and an iterative sequence of rotated sequences"
  [s]
  (let [len (count s)]
    (set (map set (mapcat (fn [rotated-seq]
                            (mapcat #(split-at % rotated-seq)
                                    (range len)))
                          (take len (iterate #(cons (last %) (butlast %))
                                             (seq s))))))))

(defn powerset2 
  "uses for comprehension, split-at, and an iterative sequence of rotated sequences"
  [s]
  (let [len (count s)]
    (set (for [rotated-seq (take len (iterate #(cons (last %) (butlast %))
                                              (seq s)))
               n (range len)
               sub-seq (split-at n rotated-seq)]
           (set sub-seq)))))

(defn powerset3
  "uses for comprehension and split-at"
  [s]
  (let [range-len (range (count s))]
    (set (for [rot range-len
               n range-len
               sub-seq (split-at n (apply concat (reverse (split-at rot (seq s)))))]
           (set sub-seq)))))
               
