(ns selmer.errors-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [selmer.errors :as errors]
            [selmer.parser :as parser]
            [selmer.filters :as filters])
  (:import [java.io StringReader]))

;; ---------------------------------------------------------------------------
;; Did-you-mean
;; ---------------------------------------------------------------------------

(deftest did-you-mean-finds-nearby-typo
  (is (= "currency-format"
         (errors/did-you-mean "currency-frmat" ["currency-format" "upper" "lower"]))))

(deftest did-you-mean-returns-nil-when-nothing-close
  (is (nil? (errors/did-you-mean "completely-unrelated"
                                 ["abc" "def" "ghi"]))))

(deftest did-you-mean-handles-empty-candidates
  (is (nil? (errors/did-you-mean "anything" []))))

(deftest did-you-mean-handles-nil-target
  (is (nil? (errors/did-you-mean nil ["x"]))))

(deftest did-you-mean-accepts-keyword-candidates
  (is (= "endif"
         (errors/did-you-mean "endiff" [:endif :endfor :endblock]))))

;; ---------------------------------------------------------------------------
;; format-error: no crash on missing data
;; ---------------------------------------------------------------------------

(deftest format-error-on-plain-exception
  (let [out (errors/format-error (Exception. "bare"))]
    (is (string? out))
    (is (str/includes? out "bare"))))

(deftest format-error-on-ex-info-without-location
  (let [out (errors/format-error (ex-info "no location" {:type :selmer/parse-error}))]
    (is (str/includes? out "no location"))
    (is (str/includes? out "template parse error"))))

(deftest format-error-never-throws
  (let [out (errors/format-error
             (ex-info "x" {:selmer.util/location {:template "/no/such/file/here"
                                                  :line 99 :col 1}}))]
    (is (string? out))))

;; ---------------------------------------------------------------------------
;; format-error: with location, no source available (keyword template)
;; ---------------------------------------------------------------------------

(deftest format-error-with-string-template-location
  (let [out (errors/format-error
             (ex-info "boom"
               {:type :selmer/parse-error
                :selmer.util/location {:template :string :line 1 :col 1}}))]
    (is (str/includes? out "string:1:1"))))

;; ---------------------------------------------------------------------------
;; format-error: with real on-disk template
;; ---------------------------------------------------------------------------

(deftest format-error-includes-source-snippet-when-template-readable
  (let [tmp (java.io.File/createTempFile "selmer-err-test" ".html")]
    (try
      (spit tmp "line 1\nline 2\nbad {% wibble %}\nline 4\n")
      (let [out (errors/format-error
                 (ex-info "unrecognized tag: :wibble"
                   {:type :selmer/parse-error
                    :tag-name :wibble
                    :selmer.util/location
                    {:template (.getAbsolutePath tmp)
                     :line 3 :col 5
                     :end-line 3 :end-col 17}}))]
        (is (str/includes? out "line 2"))
        (is (str/includes? out "bad {% wibble %}"))
        (is (str/includes? out "^"))
        (is (str/includes? out (str (.getAbsolutePath tmp) ":3:5"))))
      (finally
        (.delete tmp)))))

(deftest format-error-includes-opener-location-for-unclosed-blocks
  (let [tmp (java.io.File/createTempFile "selmer-err-test" ".html")]
    (try
      (spit tmp "{% if x %}\nhello\n")
      (let [out (errors/format-error
                 (ex-info "No closing tag found for :if"
                   {:type :selmer/parse-error
                    :tag-name :if
                    :selmer.util/location
                    {:template (.getAbsolutePath tmp) :line 3 :col 1}
                    :selmer.util/opener-location
                    {:template (.getAbsolutePath tmp) :line 1 :col 1
                     :end-line 1 :end-col 11}}))]
        (is (str/includes? out "opened at"))
        (is (str/includes? out "{% if x %}")))
      (finally
        (.delete tmp)))))

;; ---------------------------------------------------------------------------
;; Integration: parser exception → formatter
;; ---------------------------------------------------------------------------

(deftest formats-real-parse-error-end-to-end
  (let [t (try
            (parser/parse parser/parse-input (StringReader. "{% wibble %}"))
            nil
            (catch clojure.lang.ExceptionInfo e e))]
    (is (some? t))
    (let [out (errors/format-error t)]
      (is (str/includes? out "wibble"))
      (is (str/includes? out "template parse error")))))

(deftest formats-real-render-error-end-to-end
  (filters/add-filter! :test-explode-errors
    (fn [_] (throw (ex-info "boom" {}))))
  (try
    (let [t (try
              (parser/render "ok {{x|test-explode-errors}}" {:x "y"})
              nil
              (catch clojure.lang.ExceptionInfo e e))]
      (is (some? t))
      (let [out (errors/format-error t)]
        (is (str/includes? out "template render error"))
        (is (str/includes? out "string:1:4"))))
    (finally
      (swap! filters/filters dissoc :test-explode-errors))))
