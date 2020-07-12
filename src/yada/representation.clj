;; Copyright © 2014-2017, JUXT LTD.

(ns yada.representation
  (:require
   [clojure.string :as str]
   [yada.charset :as charset]
   [yada.util :refer [http-token OWS]]
   [juxt.pick.alpha.core :refer [pick]]
   [juxt.pick.alpha.apache :refer [using-apache-algo]]
   [juxt.reap.alpha.decoders :as reap]))

;; Languages ------------------------------------

(defrecord LanguageMap [language quality])

(defn parse-language [s]
  (let [[_ lang qvalue]
        (re-matches
         (re-pattern
          (str "(" http-token ")(?:" OWS ";" OWS "q=(0(?:.\\d{0,3})?|1(?:.0{0,3})?))?"))
         s)]
    ;; qvalue could be nil
    (when lang
      (map->LanguageMap
       {:language (vec (map str/lower-case (str/split lang #"-")))
        :quality (if qvalue (Float/parseFloat qvalue) (float 1.0))}))))

(defn- inject-parameter
  [content-type nm v]
  (-> content-type
      (update :juxt.http/parameters
              conj
              {:juxt.http/parameter-name nm :juxt.http/parameter-value v})
      (assoc-in [:juxt.http/parameter-map nm] v)))

(defn- ->pick-opts
  [req reps]
  {:juxt.http/variants
   (map (fn [rep]
          #:juxt.http
          {:content-type
           (reduce-kv
             inject-parameter
             (cond-> (reap/content-type (:name (:media-type rep)))
               (:charset rep)
               ;; charset also has a quality value, but pick doesn't represent
               ;; that, so callers will need to factor that in themselves.
               (inject-parameter "charset" (:alias (:charset rep)))
               (:quality (:media-type rep))
               (inject-parameter "q" (str (:quality (:media-type rep)))))
             (:parameters rep))
           :content-language (reap/content-language
                               (str (str/join "-" (:language (:language rep)))
                                 "; q="
                                 (:quality (:language rep))))
           :content-encoding (reap/content-encoding
                               (str (:coding (:encoding rep))
                                    (when-let [q (:quality (:encoding rep))]
                                      (str "; q=" q))))
           ::rep rep})
        reps)
   :juxt.http/request-headers
   (-> (:headers req)
       (update "accept" reap/accept)
       (update "accept-encoding" reap/accept-encoding)
       (update "accept-language" reap/accept-language))})

(defn select-best-representation
  "Given a request and a collection of representations, pick the best
  representation. This scores each representation against each of 4
  dimensions, each score contributes to its overall rating, the best
  being decided by the given algorithm, which defaults to
  'agent-preference-sequential-compare'."
  [req reps]
  (::rep (first
           (sort-by #(-> % ::rep :charset (:quality 1.0) -)
                    (:juxt.http/variants (pick using-apache-algo (->pick-opts req reps)))))))

(defn vary
  "From a representation-seq, find the variable dimensions"
  [reps]
  (cond-> #{}
    (< 1 (count (distinct (keep (comp #(when % (:name %)) :media-type) reps))))
    (conj :media-type)

    (< 1 (count (distinct (keep (comp #(when % (charset/charset %)) :charset) reps))))
    (conj :charset)

    (< 1 (count (distinct (keep (comp :coding :encoding) reps))))
    (conj :encoding)

    (< 1 (count (distinct (keep (comp :language :language) reps))))
    (conj :language)))

(defn to-vary-header
  "From the result of vary, construct a header"
  [vary]
  (str/join ", "
            (keep {:media-type "accept"
                   :charset "accept-charset"
                   :encoding "accept-encoding"
                   :language "accept-language"}
                  vary)))
