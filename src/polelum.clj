(ns polelum
  (:require [clojure.java.io :as io])
  (:import (morfologik.stemming PolishStemmer WordData)))

(def disamb
  (read-string (slurp (io/resource "disamb.edn"))))

(let [stemmer (PolishStemmer.)]
  (defn stem [word]
    (locking stemmer
      (let [res (map (fn [^WordData x] {:base (-> x .getStem str) :tag (-> x .getTag str)}) (.lookup stemmer word))]
        (when-not (empty? res) res)))))

(defn lemmatize [^String word]
  (or (disamb word)
      (let [lc (.toLowerCase word)]
        (or (disamb lc) (:base (first (stem lc)) lc)))))
