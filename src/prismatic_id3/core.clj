(ns prismatic-id3.core
  (:require [clojure.data.json :as json])
  (:use [clojure.java.io :only [file reader]]
        [prismatic-id3.math]
        [prismatic-id3.util]))

(defn line->example [line]
  (let [[visit outcomes] (json/read-json line)
        choice (as-> visit choice
                     (:copy_choices choice)
                     (rand-nth choice)
                     (keyword choice))]
    (dissoc (assoc visit :copy_choice choice
                   :label (choice outcomes))
            :copy_choices)))

(defn leaf-node
  "creates a leaf node from examples and a target-attr"
  [examples target-attr]
  { target-attr (most-common target-attr examples) })

(defn build-id3-tree
  "Builds an ID3 Decision tree to find target-attr based on the examples"
  [examples target-attr attributes]
  (cond
    (same? target-attr examples) { target-attr (target-attr (first examples)) }
    (empty? attributes) (leaf-node examples target-attr)
    :else (let [attr (max-val #(information-gain % examples) attributes)
                groups (group-by attr examples)
                child-agent (agent {})]
            (loop [[value subset] (first groups)
                   others (rest groups)]
              (do
                (send child-agent assoc value
                      (if (empty? subset)
                        (leaf-node examples target-attr)
                        (build-id3-tree subset
                                        target-attr
                                        (without attr attributes))))
                (cond
                  (empty? others) { attr child-agent }
                  :else (recur (first others) (rest others))))))))

(defn classify
  "Classifies an unclassified example based
  on a tree made by build-id3-tree"
  [unclassified target-attr tree]
  (cond
    (contains? tree target-attr) (assoc unclassified target-attr (target-attr tree))
    :else (let [attr (-> tree keys first keyword)
                subtree (deref (tree attr))
                value (unclassified attr)]
              (recur unclassified target-attr (subtree value)))))

(defn file->examples
  "Takes a file and returns an agent of examples"
  [file-handle]
  (agent (as-> file-handle examples
               (reader examples)
               (line-seq examples)
               (pmap line->example examples))))

(defn remove-attr
  [target-attr examples]
  (map #(dissoc % target-attr) examples))

(defn -main
  "I solve the Prismatic growth problem"
  [examples-filename unclassifieds-filename target-attr]
  (let [examples (file->examples (file examples-filename))
        unclassifieds (file->examples (file unclassifieds-filename))
        target-attr (keyword target-attr)]
    (do
      (await examples)
      (await unclassifieds)
      (let [examples (deref examples)
            unclassifieds (remove-attr target-attr (deref unclassifieds))
            attrs (map keyword (without target-attr (keys (first examples))))
            tree (build-id3-tree examples target-attr attrs)
            trues (filter target-attr (pmap #(classify % target-attr tree) unclassifieds))]
        (println trues)
        (printf "%d / %d candidates are true\n" (count trues) (count unclassifieds))))))
