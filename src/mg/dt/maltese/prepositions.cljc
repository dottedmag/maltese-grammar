(ns mg.dt.maltese.prepositions
  (:require [clojure.string :as str]))

(defn- chuk-i [word]
  (if (str/starts-with? word "i") (subs word 1) word))

(defn- starts-xemxija [word]
  (re-matches #"[ċdnrstxżz].*" word))

(def ^:private simple
  {"ma'" "ma" "ta'" "ta" "ġo" "ġo" "sa" "sa"})

(defn- combine-definite-simple [prep-base word]
  (str prep-base (chuk-i word)))

(def ^:private contractable {"fi" ["f'" "f"] "bi" ["b'" "b"]})

(defn- combine-definite-contractable [prep-base1 prep-base2 word]
  (if (str/starts-with? word "l-")
    (str prep-base2 word)
    (str prep-base1 (chuk-i word))))

(def ^:private complex
  {"lill" ["lil" "li"] "għal" ["għal" "għa"]
   "bħal" ["bħal" "bħa"] "minn" ["mil" "mi"]})

(defn- combine-definite-complex [prep-base1 prep-base2 word]
  (let [word (chuk-i word)]
    (cond
      (re-matches #"l-l.*" word) (str prep-base2 word)
      (starts-xemxija word) (str prep-base2 word)
      :else (str prep-base1 word))))

(defn- combine-definite [prep word]
  (if-let [base (simple prep)]
    (combine-definite-simple base word)
    (if-let [contracted (contractable prep)]
      (combine-definite-contractable prep (second contracted) word)
      (let [bases (complex prep)]
        (combine-definite-complex (first bases) (second bases) word)))))

(def ^:private starts-cons-vowel
  #"(b|ċ|d|f|g|ġ|h|ħ|għ|j|k|l|m|n|p|q|r|s|t|v|w|x|z|ż)(a|e|i|ie|o).*")

(def ^:private starts-vowel-or-silent
  #"(a|e|i|ie|o|h|għ).*")

;; fi' bi'
(defn- contract [prep prep-contracted word]
  (cond
    (re-matches starts-vowel-or-silent word) (str prep-contracted word)
    (re-matches starts-cons-vowel word) (str prep-contracted word)
    :else (str prep " " word)))

(defn- combine-indefinite [prep word]
  (if-let [contracted (contractable prep)]
    (contract prep (first contracted) word)
    (str prep " " word)))

(defn combine [prep definite? word]
  (if definite?
    (combine-definite prep word)
    (combine-indefinite prep word)))
