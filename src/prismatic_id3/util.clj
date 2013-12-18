(ns prismatic-id3.util)

(def separate (comp vals group-by))

(def id (fn [x] x))

(defn without [this that]
  (remove #(= this %) that))

(defn most-common [attr data]
  (first (apply (partial max-key #(count (last %)))
                (group-by attr data))))

(defn max-val [transformer data]
  (apply (partial max-key transformer) data))

(defn same? [pred & coll]
  (apply = (apply (partial map pred) coll)))
