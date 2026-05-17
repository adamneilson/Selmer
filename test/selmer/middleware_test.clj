(ns selmer.middleware-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [selmer.middleware :as mw]
            [selmer.parser :as parser])
  (:import [java.io StringReader]))

(deftest validation-error-routes-through-location-aware-page
  ;; Validation errors now use the same location-aware page as parse
  ;; and render errors, since the validator's exception data
  ;; ({:template :line :validation-errors [...]}) is enough to
  ;; synthesize a useful source snippet.
  (let [handler  (fn [_] (throw (ex-info "Unrecognized tag found {% wbble %}"
                                  {:type :selmer/validation-error
                                   :error "Unrecognized tag found"
                                   :error-template "<p>{{error}}.</p>"
                                   :line 5
                                   :template "templates/demo.html"
                                   :validation-errors
                                   [{:tag "{% wbble %}" :line 5}]})))
        wrapped  (mw/wrap-error-page handler)
        response (wrapped {})]
    (is (= 500 (:status response)))
    (is (str/includes? (:body response) "Template error"))
    (is (str/includes? (:body response) "wbble"))))

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
