(ns selmer.position-test
  "Contract tests for the source-location information that Selmer attaches
   to parsed tags and to the exceptions thrown from parse and render.

   These tests pin down the externally visible behaviour of the
   position-tracking change. They go through the public parser entry
   points (parse, parse-input, render, render-file) rather than poking
   at internals."
  (:require [clojure.test :refer :all]
            [selmer.parser :as parser]
            [selmer.filters :as filters])
  (:import [selmer.node FunctionNode]
           [java.io File StringReader]))

(def template-path (str "test/templates" File/separator))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- parse [s]
  (parser/parse parser/parse-input (StringReader. s)))

(defn- function-nodes [ast]
  (filter #(instance? FunctionNode %) ast))

(defn- locations [s]
  (->> (parse s)
       function-nodes
       (map #(-> ^FunctionNode % .handler meta :tag :selmer.util/location))))

(defn- tag-of [ast n]
  (-> ^FunctionNode (nth (function-nodes ast) n) .handler meta :tag))

;; ---------------------------------------------------------------------------
;; Variable and expression tags carry location
;; ---------------------------------------------------------------------------

(deftest variable-tag-carries-location
  (let [loc (first (locations "{{name}}"))]
    (is (= {:line 1 :col 1 :end-line 1 :end-col 9 :template :string} loc))))

(deftest expression-tag-carries-location
  (let [loc (first (locations "{% if x %}body{% endif %}"))]
    (is (= 1 (:line loc)))
    (is (= 1 (:col loc)))))

(deftest tag-with-leading-text
  (let [loc (first (locations "hello {{name}}"))]
    (is (= {:line 1 :col 7 :end-line 1 :end-col 15 :template :string} loc))))

(deftest multiple-tags-on-one-line-have-distinct-columns
  (let [[a b c] (locations "{{a}} {{b}} {{c}}")]
    (is (= 1 (:col a)))
    (is (= 7 (:col b)))
    (is (= 13 (:col c)))
    (is (= [1 1 1] [(:line a) (:line b) (:line c)]))))

(deftest tags-across-lines-track-line-number
  (let [[a b c] (locations "{{a}}\n{{b}}\n\n{{c}}")]
    (is (= 1 (:line a)))
    (is (= 2 (:line b)))
    (is (= 4 (:line c)))
    (is (= [1 1 1] [(:col a) (:col b) (:col c)]))))

(deftest crlf-line-endings-still-track-correctly
  (let [[a b] (locations "{{a}}\r\n{{b}}")]
    (is (= 1 (:line a)))
    (is (= 2 (:line b)))))

(deftest nested-block-tag-locations
  ;; The body of a block tag is consumed by the tag handler and lives
  ;; inside the tag's compiled function, so only the outer block tag is
  ;; a top-level FunctionNode. The body's nested locations are not
  ;; observable via the top-level AST; the block tag's own location is.
  (let [ast      (parse "{% for i in xs %}{{i}}{% endfor %}")
        nodes    (function-nodes ast)
        for-loc  (-> ^FunctionNode (first nodes)
                     .handler meta :tag :selmer.util/location)]
    (is (= 1 (count nodes)))
    (is (= 1 (:line for-loc)))
    (is (= 1 (:col for-loc)))))

(deftest filter-tag-location-points-at-opening-brace
  (let [loc (first (locations "  {{x|upper}}"))]
    (is (= 3 (:col loc)) "column should be the first opening brace")))

;; ---------------------------------------------------------------------------
;; Template identifier plumbing
;; ---------------------------------------------------------------------------

(deftest parse-string-uses-default-template-name
  (let [loc (first (locations "{{x}}"))]
    (is (= :string (:template loc)))))

(deftest parse-input-with-explicit-template
  (let [ast (parser/parse parser/parse-input (StringReader. "{{x}}")
                          {:template "inline.html"})
        loc (-> ^FunctionNode (first (function-nodes ast))
                .handler meta :tag :selmer.util/location)]
    (is (= "inline.html" (:template loc)))))

(deftest parse-input-with-path-string-uses-it-as-template
  ;; When a path string is passed directly to parse-input, the path becomes
  ;; the template identifier so consumers can match what they passed.
  (parser/cache-off!)
  (let [path "test/templates/if.html"
        ast (parser/parse parser/parse-input path)
        loc (some #(-> ^FunctionNode %
                       .handler meta :tag :selmer.util/location)
                  (function-nodes ast))]
    (is (= path (:template loc)))))

;; ---------------------------------------------------------------------------
;; Parse-time errors carry locations
;; ---------------------------------------------------------------------------

(deftest unknown-tag-error-carries-location
  (try
    (parse "{% wibble %}")
    (is false "should have thrown")
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
        (is (= :selmer/parse-error (:type data)))
        (is (= :wibble (:tag-name data)))
        (is (= {:line 1 :col 1 :end-line 1 :end-col 13 :template :string}
               (:selmer.util/location data)))))))

(deftest unclosed-if-error-points-at-opener
  (try
    (parse "hello {% if x %}body")
    (is false "should have thrown")
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
        (is (= :selmer/parse-error (:type data)))
        (is (= :if (:tag-name data)))
        (let [opener (:selmer.util/opener-location data)]
          (is (some? opener) "opener-location should be present")
          (is (= 1 (:line opener)))
          (is (= 7 (:col opener)) "opener column is the if's opening {"))))))

(deftest unclosed-block-opener-location-survives-nested-tags
  (try
    (parse "{% if x %}{{a}}{{b}}")
    (is false "should have thrown")
    (catch clojure.lang.ExceptionInfo e
      (let [opener (:selmer.util/opener-location (ex-data e))]
        (is (= 1 (:line opener)))
        (is (= 1 (:col opener)))))))

(deftest unclosed-short-comment-error-has-location
  (try
    (parse "hello {# unfinished comment")
    (is false "should have thrown")
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
        (is (= :selmer/parse-error (:type data)))
        (is (some? (:selmer.util/location data)))))))

;; ---------------------------------------------------------------------------
;; Render-time errors carry locations
;; ---------------------------------------------------------------------------

(deftest filter-throwing-at-render-carries-location
  (filters/add-filter! :test-boom
    (fn [_] (throw (ex-info "boom" {:filter-issue true}))))
  (try
    (try
      (parser/render "line1\nline2 {{x|test-boom}}" {:x "hi"})
      (is false "should have thrown")
      (catch clojure.lang.ExceptionInfo e
        (let [data (ex-data e)]
          (is (= true (:filter-issue data))
              "inner ex-data must be preserved")
          (let [loc (:selmer.util/location data)]
            (is (some? loc))
            (is (= 2 (:line loc)) "location should be the line of the filter")))))
    (finally
      (swap! filters/filters dissoc :test-boom))))

(deftest tag-handler-throwing-at-render-carries-location
  (parser/add-tag! :test-explode
    (fn [_args _context-map]
      (throw (ex-info "kaboom" {:tag-issue true}))))
  (try
    (try
      (parser/render "\n\n{% test-explode %}" {})
      (is false "should have thrown")
      (catch clojure.lang.ExceptionInfo e
        (let [data (ex-data e)]
          (is (= true (:tag-issue data)))
          (let [loc (:selmer.util/location data)]
            (is (= 3 (:line loc)))))))
    (finally
      (parser/remove-tag! :test-explode))))

(deftest innermost-location-wins-when-already-present
  ;; If an inner exception already carries a :selmer.util/location, the
  ;; render-time wrap must NOT overwrite it. This protects the precise
  ;; inner location through outer rethrows.
  (filters/add-filter! :test-precise
    (fn [_]
      (throw (ex-info "precise"
               {:selmer.util/location {:line 99 :col 99 :template "inner"}}))))
  (try
    (try
      (parser/render "{{x|test-precise}}" {:x "y"})
      (is false "should have thrown")
      (catch clojure.lang.ExceptionInfo e
        (let [loc (:selmer.util/location (ex-data e))]
          (is (= 99 (:line loc)))
          (is (= "inner" (:template loc))))))
    (finally
      (swap! filters/filters dissoc :test-precise))))

;; ---------------------------------------------------------------------------
;; Include and extends behaviour (phase-a: outermost template only)
;; ---------------------------------------------------------------------------

(deftest tags-in-included-templates-still-carry-some-location
  ;; Phase-a limitation: included templates are flattened by
  ;; preprocess-template before parse* sees them, so :template names the
  ;; outer source and lines are relative to the flattened text. This
  ;; pins down the current behaviour so a future phase-c PR has a clear
  ;; baseline to improve against.
  (parser/cache-off!)
  (let [ast (parser/parse parser/parse-file "templates/include.html" {})]
    (is (every? some?
                (->> (function-nodes ast)
                     (map #(-> ^FunctionNode % .handler meta :tag
                               :selmer.util/location))))
        "every tag should still get a (possibly flattened) location")))
