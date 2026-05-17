(ns selmer.middleware-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [selmer.middleware :as mw]
            [selmer.parser :as parser])
  (:import [java.io StringReader]))

(deftest validation-error-still-rendered-via-template
  ;; Back-compat: the legacy :selmer/validation-error path still renders
  ;; the included error-template against the ex-data.
  (let [handler  (fn [_] (throw (ex-info "boom"
                                  {:type :selmer/validation-error
                                   :error "something went wrong"
                                   :error-template "<p>{{error}}.</p>"
                                   :line nil
                                   :template nil
                                   :validation-errors []})))
        wrapped  (mw/wrap-error-page handler)
        response (wrapped {})]
    (is (= 500 (:status response)))
    (is (str/includes? (:body response) "something went wrong"))))

(deftest parse-error-rendered-with-location-aware-page
  (let [handler  (fn [_] (parser/parse parser/parse-input
                                       (StringReader. "{% wibble %}")))
        wrapped  (mw/wrap-error-page handler)
        response (wrapped {})]
    (is (= 500 (:status response)))
    (is (str/includes? (:body response) "Template error"))
    (is (str/includes? (:body response) "wibble"))
    (is (str/includes? (:body response) "string:1:1"))))

(deftest unrelated-exceptions-are-rethrown
  (let [handler (fn [_] (throw (ex-info "other"
                                 {:type :something/else})))
        wrapped (mw/wrap-error-page handler)]
    (is (thrown? clojure.lang.ExceptionInfo (wrapped {})))))

(deftest html-escapes-error-message
  (let [handler (fn [_] (throw (ex-info "<script>"
                                 {:type :selmer/parse-error
                                  :selmer.util/location
                                  {:template :string :line 1 :col 1}})))
        wrapped (mw/wrap-error-page handler)
        body    (:body (wrapped {}))]
    (is (str/includes? body "&lt;script&gt;"))
    (is (not (str/includes? body "<script>")))))
