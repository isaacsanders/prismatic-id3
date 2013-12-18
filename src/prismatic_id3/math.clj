(ns prismatic-id3.math
  (:use [clojure.walk]
        [prismatic-id3.util]))

(defn log [number base]
  (/ (Math/log number) (Math/log base)))

(defn log2 [number]
  (log number 2))

(defn sum [summand data]
  (walk summand #(apply + %) data))

(defn proportion [subset total]
  (/ (float (count subset))
     (float (count total))))

(defn entropy [data]
  (cond
    (empty? data) 0
    (same? :label data) 0
    :else (- (sum #(* (proportion % data) (log2 (proportion % data)))
                  (map #(map :label %) (separate :label data))))))

(defn information-gain [attr data]
  (- (entropy data)
     (sum #(* (proportion % data) (entropy %))
          (separate attr data))))
