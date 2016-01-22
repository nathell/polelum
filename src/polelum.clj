(ns polelum
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (morfologik.stemming.polish PolishStemmer)
           (morfologik.stemming WordData)))

(def disamb
  (read-string (slurp (io/resource "disamb.edn"))))

(defn all-caps? [word]
  (every? #(or (Character/isUpperCase %) (not (Character/isLetter %))) word))

(defn title-case [word]
  (str (Character/toTitleCase (first word)) (string/lower-case (subs word 1))))

(let [stemmer (PolishStemmer.)]
  (defn stem [word]
    (locking stemmer
      (let [res (map (fn [^WordData x] {:base (-> x .getStem str) :tag (-> x .getTag str)}) (.lookup stemmer word))]
        (when-not (empty? res) res)))))

(defn lemmatize-single [^String word]
  (or (disamb word) (:base (first (stem word)))))

(defn lemmatize-multiple [^String word]
  (let [d (disamb word)]
    (concat (when d [d]) (map :base (stem word)))))

(defn lemmatize [^String word]
  (or (lemmatize-single word)
      (lemmatize-single (.toLowerCase word))
      (when (all-caps? word) (lemmatize-single (title-case word)))
      word))

(defn lemmatize-all [^String word]
  (let [v (distinct (concat (lemmatize-multiple word)
                            (lemmatize-multiple (.toLowerCase word))
                            (when (all-caps? word) (lemmatize-multiple (title-case word)))))]
    (if (seq v) (vec v) [word])))
