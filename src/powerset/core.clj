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


; Sebastian's:
; #custom returns all permutations of a sequence s
(defn powerset [s]
  (if (empty? s)
    #{s}
    (let [e (first s)
          power-e (powerset (disj s e))]
      (clojure.set/union
        power-e
        (set (map #(conj % e) power-e))))))


; great (non-recursive) solution for powerset, by http://halfabrane.blogspot.com/2011/12/power-sets.html
  (defn power-set [s]
    (reduce (fn [ss x] 
              (concat ss 
                      (map #(conj % x)
                           ss))) 
            [#{}] 
            s))

               
