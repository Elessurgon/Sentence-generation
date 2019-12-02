(ns simple-language.core
  (:gen-class)
  (:require [instaparse.core :as insta]
            [clj-time.core :as time]))
     

(def as-and-bs
  (insta/parser
    "S = AB*
     AB = A B
     A = 'a'+
     B = 'b'+"))

(def sent
  (insta/parser
    "S = NP VP
     NP = A space* N space*
     VP = V space* NP space*
     A = 'a' | 'the'
     N = 'man' | 'ball' | 'woman'
     space = ' '*
     V = 'jumps'"))

(defn space? 
  [word]
  (= word " "))
  

(defn kill-space
  [sent]
  (let [new-sent (reduce (fn [lst word]
                           (println lst " word1-> " word)
                           (if (not (space? word))
                             (do
                              (conj lst word)
                              (println lst " word2-> " word)
                              (println (not (space? word))))))
                  [] sent)]
    (println (str new-sent))))


(defn new-parser
  [sent])

(declare sentence noun-phrase verb-phrase Article Verb Noun)

(defn sentence 
  []
  (let [[noun article] (noun-phrase)
        [verb article1 noun1] (verb-phrase)]
    (into [] [noun article verb article1 noun1])))

(defn noun-phrase
  []
  (into [] [(Article) (Noun)])) 

(defn verb-phrase
  []
  (let [[article noun] (noun-phrase)]
    (into [] [(Verb) article noun])))

(defn Article []
  (rand-nth '(the a)))

(defn Verb []
  (rand-nth '(hit took saw liked)))

(defn Noun []
  (rand-nth '(man ball woman table)))

(defn -main
  [num]
  (loop [number 0]
    (if (< number num)
      (do
        (println (sentence))
        (recur (inc number))))))

;;prototype 

;; Has an Extendable datastructure
(def ^:dynamic *simple-grammer*
  '((sentence -> (noun-phrase verb-phrase conjunction-phrase))
    (conjunction-phrase -> (Conjunction smaller-sentence))
    (smaller-sentence -> (verb-phrase)) 
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Conjunction -> and or)))
    

(def ^:dynamic *grammar* *simple-grammer*) 

(assoc {} 'noun *grammar*)

(defn retrive-assoc
  [name list]
  (loop [rules list]
    (let [element (first rules)]
      (if(= name (first element))
        element
        (do
          (if(empty? element)
            (println "Done")
            (recur(rest rules))))))))

(defn rule-lhs 
  [rule]
  (first rule))

(defn rule-rhs 
  [rule]
  (rest (rest rule)))

(defn rewrite 
  [category]
  (first (rule-rhs (retrive-assoc category *grammar*))))

(defn rewrite-terminal 
  [category]
  (rule-rhs (retrive-assoc category *grammar*)))

(defn generate
  [phrase]
  (if (seq? (rewrite phrase))
    (do
      (mapcat #(generate %) (rewrite phrase)))
    (do
      (let [word (rand-nth (rewrite-terminal phrase))]
        (println word)
        [word]))))            


;;Binary Tree
(defrecord Node [el left right])

(defn insert [{:keys [el left right] :as tree} value]
  (cond
    (nil? tree) (Node. value nil nil)
    (< value el) (Node. el (insert left value) right)
    (> value el) (Node. el left (insert right value))
    :else tree))

(defn min [{:keys [el left]}]
  (if left
    (recur left)
    el))

(defn remove [{:keys [el left right] :as tree} value]
  (cond
    (nil? tree) nil
    (< value el) (Node. el (remove left value) right)
    (> value el) (Node. el left (remove right value))
    (nil? left) right
    (nil? right) left
    :else (let [min-value (min right)]
            (Node. min-value left (remove right min-value)))))



(defn max [{:keys [el right]}]
  (if right
    (recur right)
    el))

(defn contains? [{:keys [el left right] :as tree} value]
  (cond
    (nil? tree) false
    (< value el) (recur left value)
    (> value el) (recur right value)
    :else true))

(defn count [{:keys [left right] :as tree}]
  (if tree
    (+ 1 (count left) (count right))
    0))

(defn height
  ([tree] (height tree 0))
  ([tree count]
   (if tree
     (max (height (:left tree) (inc count))
       (height (:right tree) (inc count))
       count))))

(defn bst?
  ([tree] (bst? tree Integer/MIN_VALUE Integer/MAX_VALUE))
  ([{:keys [el left right] :as tree} min max]
   (cond
     (nil? tree) true
     (or (< el min) (> el max)) false
     :else (and (bst? left min (dec el))
                (bst? right (inc el) max)))))

(def to-tree #(reduce insert nil %))

(defn to-list [{:keys [el left right] :as tree}]
  (when tree
    `(~@(to-list left) ~el ~@(to-list right))))

(def tree (to-tree '(5 8 2 3 4 1)))
