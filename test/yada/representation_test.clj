;; Copyright © 2014-2017, JUXT LTD.

(ns yada.representation-test
  (:require
   [clojure.test :refer :all]
   [schema.test :as st]
   [yada.charset :as charset]
   [yada.media-type :as mt]
   [yada.representation :as rep]
   [yada.test :refer [response-for]]))

(deftest content-type-test

  (testing "Basic match"
    (is (= (rep/select-best-representation
            {:headers {"accept" "text/html"}}
            [{:media-type (mt/string->media-type "text/html")}])
           {:media-type (mt/string->media-type "text/html")})))

  (testing "Basic match, with multiple options"
    (is (= (rep/select-best-representation
            {:headers {"accept" "image/png,text/html"}}
            [{:media-type (mt/string->media-type "text/html")}])
           {:media-type (mt/string->media-type "text/html")})))

  (testing "Basic match, with multiple options and q values"
      (is (= (rep/select-best-representation
              {:headers {"accept" "image/png,text/html;q=0.9"}}
              [{:media-type (mt/string->media-type "text/html;q=0.8")}])
             {:media-type (mt/string->media-type "text/html;q=0.8")})))

  (testing "Basic reject"
    (is (= (rep/select-best-representation
            {:headers {"accept" "text/html"}}
            [{:media-type (mt/string->media-type "text/plain")}])
           nil)))

  (testing "Basic reject with multiple options"
    (is (= (rep/select-best-representation
            {:headers {"accept" "image/png,text/html"}}
            [{:media-type (mt/string->media-type "text/plain")}])
           nil)))

  (testing "Basic reject due to zero accept quality"
    (is (= (rep/select-best-representation
            {:headers {"accept" "text/html;q=0"}}
            [{:media-type (mt/string->media-type "text/html")}])
           nil)))

  (testing "Basic reject due to zero representation quality"
    (is (= (rep/select-best-representation
            {:headers {"accept" "text/html"}}
            [{:media-type (mt/string->media-type "text/html;q=0")}])
           nil)))

  (testing "Wildcard match"
    (is (= (rep/select-best-representation
             {:headers {"accept" "image/png,text/*"}}
             [{:media-type (mt/string->media-type "text/html")}])
           {:media-type (mt/string->media-type "text/html")})))

  (testing "Wildcard type mismatch"
    (is (= (rep/select-best-representation
            {:headers {"accept" "text/*"}}
            [{:media-type (mt/string->media-type "image/png")}])
           nil)))

  (testing "Specific match beats wildcard"
    (is (= (rep/select-best-representation
             {:headers {"accept" "image/png,text/*,text/html"}}
             [{:media-type (mt/string->media-type "text/html")}])
           {:media-type (mt/string->media-type "text/html")})))

  (testing "Specific match beats wildcard, different order"
    (is (= (rep/select-best-representation
             {:headers {"accept" "text/html,text/*,image/png"}}
             [{:media-type (mt/string->media-type "text/html")}])
           {:media-type (mt/string->media-type "text/html")})))

  (testing "Parameter alignment"
    (is (= (rep/select-best-representation
            {:headers {"accept" "text/html;level=2"}}
            [{:media-type (mt/string->media-type "text/html;level=1")}])
           nil)))

  (testing "Greater number of parameters matches"
    (is (= (rep/select-best-representation
             {:headers {"accept" "text/html,text/html;level=1"}}
             [{:media-type (mt/string->media-type "text/html;level=1")}])
           {:media-type (mt/string->media-type "text/html;level=1")}))))

(deftest charset-test

  (testing "Basic match"
    (is (= (rep/select-best-representation
            {:headers {"accept-charset" "utf-8"}}
            [{:charset (charset/to-charset-map "utf-8")}])
           {:charset (charset/to-charset-map "utf-8")})))

  (testing "Basic match with wildcard"
    (is (= (rep/select-best-representation
            {:headers {"accept-charset" "*"}}
            [{:charset (charset/to-charset-map "utf-8")}])
           {:charset (charset/to-charset-map "utf-8")})))

  (testing "Basic match with zero accept quality"
    (is (= (rep/select-best-representation
            {:headers {"accept-charset" "utf-8;q=0"}}
            [{:charset (charset/to-charset-map "utf-8")}])
           nil)))

  (testing "Basic match with zero rep quality"
    (is (= (rep/select-best-representation
            {:headers {"accept-charset" "utf-8"}}
            [{:charset (charset/to-charset-map "utf-8;q=0")}])
           nil)))

  (testing "Basic match with wildcard, multiple choices"
    (is (= (rep/select-best-representation
            {:headers {"accept-charset" "*;q=0.9,utf-8"}}
            [{:charset (charset/to-charset-map "utf-8")}])
           {:charset (charset/to-charset-map "utf-8")})))

  (testing "Basic match with wildcard, multiple choices, matches wildcard"
    (is (= (rep/select-best-representation
            {:headers {"accept-charset" "*;q=0.9,utf-8"}}
            [{:charset (charset/to-charset-map "us-ascii")}])
           {:charset (charset/to-charset-map "us-ascii")})))

  (testing "Quality values"
    (is (= (rep/select-best-representation
            {:headers {"accept-charset" "utf-8;q=0.8"}}
            [{:charset (charset/to-charset-map "utf-8;q=0.9")}])
           {:charset (charset/to-charset-map "utf-8;q=0.9")})))

  (testing "Multiple choices"
    (is (= (rep/select-best-representation
            {:headers {"accept-charset" "us-ascii,utf-8;q=0.9,Shift_JIS"}}
            [{:charset (charset/to-charset-map "utf-8")}])
           {:charset (charset/to-charset-map "utf-8")})))

  (testing "Multiple choices but none ok"
    (is (= (rep/select-best-representation
            {:headers {"accept-charset" "us-ascii,Shift_JIS"}}
            [{:charset (charset/to-charset-map "utf-8")}])
           nil))))

(deftest accept-charset-test
  "Test fix for #126"
  (is
   (response-for
    {:methods
     {:get
      {:produces [{:media-type "application/json"}]
       :response (fn [ctx] "")}}}
    :get "/" {:headers {"accept-charset" "*"}}))
  (testing "#132 fixed"
    (is
     (= (:status
         (response-for
          {:methods
           {:get
            {:produces [{:media-type "text/plain"
                         }]
             :response (fn [ctx] "foobar")}}}
          :get "/" {:headers {"accept-charset" "UTF-8"}}))
        200))))

(deftest encoding-test

  (testing "Representation has no encoding, it is acceptable, rule 2"
    (is (= (rep/select-best-representation
            {:headers {"accept-encoding" "gzip"}}
            [{}])
           {})))

  ;; Broken: bug in pick, doesn't handle q=0
  ;; (testing "... except when not"
  ;;   (is (= (rep/select-best-representation
  ;;           {:headers {"accept-encoding" "gzip, identity;q=0"}}
  ;;           [{}])
  ;;          nil)))

  ;; (testing "... except when not with wildcard"
  ;;   (is (= (rep/select-best-representation
  ;;           {:headers {"accept-encoding" "gzip, *;q=0"}}
  ;;           [{}])
  ;;          nil)))

  (testing "... except when not"
    (is (= (rep/select-best-representation
            {:headers {"accept-encoding" "gzip, identity;q=0.5, *;q=0"}}
            [{}])
           {})))

  (testing "Basic match"
    (is (= (rep/select-best-representation
            {:headers {"accept-encoding" "gzip, compress"}}
            [{:encoding {:coding "gzip", :quality 0.7}}])
           {:encoding {:coding "gzip", :quality 0.7}})))

  ;; Pick provides a rep, even though it doesn't match
  ;; (testing "Basic reject"
  ;;   (is (= (rep/select-best-representation
  ;;           {:headers {"accept-encoding" "gzip, compress"}}
  ;;           [{:encoding {:coding "deflate", :quality 0.4}}])
  ;;          nil)))

  ;; Pick provides a rep, even though it doesn't match
  ;; (testing "Empty accept encoding header field value"
  ;;   ;; The client does NOT want any codings
  ;;   (is (= (rep/select-best-representation
  ;;           {:headers {"accept-encoding" ""}}
  ;;           {:encoding (rep/parse-encoding "deflate,gzip;q=0.4")})
  ;;          nil)))
  )

(st/deftest language-test

  (testing "Basic match"
    (is (= (rep/select-best-representation
            {:headers {"accept-language" "en"}}
            [{:language (rep/parse-language "en")}])
           {:language (rep/parse-language "en")})))

  (testing "Wildcard match"
    (is (= (rep/select-best-representation
            {:headers {"accept-language" "en-*;q=0.8"}}
            [{:language (rep/parse-language "en-US;q=0.7")}])
           {:language (rep/parse-language "en-US;q=0.7")})))

  (testing "Reject"
    (is (= (rep/select-best-representation
             {:headers {"accept-language" "en-US"}}
             [{:language (rep/parse-language "de")}
              {:language (rep/parse-language "en-US")}])
           {:language (rep/parse-language "en-US")}))
    (is (= (rep/select-best-representation
            {:headers {"accept-language" "en-US"}}
            [{:language (rep/parse-language "en-GB")}
             {:language (rep/parse-language "en-US")}])
           {:language (rep/parse-language "en-US")}))))

;; TODO: Put the whole thing together, where an accept header corresponds to
;; the construction of a transducer that filters all the possible
;; (not-rejected) representations, then a pro-active pick of the 'best'
;; one, by some determination (content-type first, total quality
;; degradation, etc.)

;; TODO: Special case: when the accept-encoding header field value is
;; empty, the encoding is set to identity, no matter what. When creating
;; a list of representations, always create identity versions. See 5.3.4
;; This can be done by interpretting :encoding "gzip, deflate" to be
;; :encoding "gzip, deflate, identity;q=0.001" and applying
;; clojure.core/distinct on the final set of representations.

;; "A request without any Accept header
;; field implies that the user agent
;; will accept any media type in response."
;; -- RFC 7231 Section 5.3.2

;; "A request without any Accept-Charset header field implies
;; that the user agent will accept any charset in response. "
;; -- RFC 7231 Section 5.3.3

(deftest ^{:doc "If you find bugs in yada with end-to-end (proactive)
  content negotiation, here's a good place to put a test."}
  select-representation-test

  (testing "Best quality charset"
    (is (= (rep/select-best-representation
            {:headers {"accept" "text/html"}}
            [{:media-type (mt/string->media-type "text/html")
              :charset (charset/to-charset-map "windows-1255;q=0.9")}
             {:media-type (mt/string->media-type "text/html")
              :charset (charset/to-charset-map "utf-8")}])
           {:media-type (mt/string->media-type "text/html")
            :charset (charset/to-charset-map "utf-8")})))

  (let [reps [{:media-type (mt/string->media-type "text/html")
               :charset (charset/to-charset-map "utf-8")}
              {:media-type (mt/string->media-type "text/xml;q=0.9")
               :charset (charset/to-charset-map "utf-8")}
              {:media-type (mt/string->media-type "text/xml;q=0.9")
               :charset (charset/to-charset-map "us-ascii")}
              {:media-type (mt/string->media-type "image/png")}]]

    (testing "No headers. Implied Accept: */*"
      (is (= (rep/select-best-representation
              {:headers {}} reps)
             {:media-type (mt/string->media-type "text/html")
              :charset (charset/to-charset-map "utf-8")})))

    (testing "Basic match"
      (is (= (rep/select-best-representation
              {:headers {"accept" "text/html"}} reps)
             {:media-type (mt/string->media-type "text/html")
              :charset (charset/to-charset-map "utf-8")}))

      (is (= (rep/select-best-representation {:headers {"accept" "text/xml"}} reps)
             {:media-type (mt/string->media-type "text/xml;q=0.9")
              :charset (charset/to-charset-map "utf-8")}))

      (is (= (rep/select-best-representation
              {:headers {"accept" "image/png"}} reps)
             {:media-type (mt/string->media-type "image/png")})))

    (testing "Wildcard match"
      (is (= (rep/select-best-representation
              {:headers {"accept" "image/*"}}
              reps)
             {:media-type (mt/string->media-type "image/png")})))))

(deftest select-language
  (let [gb {:media-type (mt/string->media-type "text/html")
            :language (rep/parse-language "en-GB")}
        us {:media-type (mt/string->media-type "text/html")
            :language (rep/parse-language "en-US")}
        da {:media-type (mt/string->media-type "text/html")
            :language (rep/parse-language "da")}
        ;; RFC 2277
        dflt {:media-type (mt/string->media-type "text/html")
              :language (rep/parse-language "i-default")}
        headers {:headers {"accept-language" "da, en-gb;q=0.8, en;q=0.7"}}]

    (testing "Languages"
      (is (= (rep/select-best-representation headers [gb us da]) da))
      (is (= (rep/select-best-representation headers [us gb]) gb))
      (is (= (rep/select-best-representation headers [us]) us))
      (is (= (rep/select-best-representation headers [us dflt]) us))
      (is (= (rep/select-best-representation headers []) nil)))))

;; Encodings
