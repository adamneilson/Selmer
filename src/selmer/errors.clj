(ns selmer.errors
  "Pretty-printer for Selmer's location-aware exceptions.

  When a parse or render error carries a `:selmer.util/location` in its
  `ex-data`, `format-error` produces a Rust-style message with a few
  lines of source context and a caret pointing at the offending tag:

      template parse error: unrecognized tag: :wibble - did you forget to close a tag?
        --> templates/billing.html:42:3
         |
      40 |   <td>{{plan.name}}</td>
      41 |   <td>{{plan.interval}}</td>
      42 |   <td>{% wibble %}</td>
         |       ^^^^^^^^^^^^ here

  For exceptions without a location, the message and ex-data are
  returned in a plain pretty form so callers can still print something
  useful.

  The formatter never throws: any internal failure falls back to the
  exception's `.getMessage`. Safe to call from middleware and error
  pages."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]))

;; ---------------------------------------------------------------------------
;; Did-you-mean for filters
;; ---------------------------------------------------------------------------

(defn ^:private levenshtein [^String a ^String b]
  (let [m (.length a)
        n (.length b)]
    (cond
      (zero? m) n
      (zero? n) m
      :else
      (let [prev (int-array (inc n))
            curr (int-array (inc n))]
        (dotimes [j (inc n)] (aset prev j j))
        (dotimes [i m]
          (aset curr 0 (inc i))
          (dotimes [j n]
            (let [cost (if (= (.charAt a i) (.charAt b j)) 0 1)]
              (aset curr (inc j)
                    (min (inc (aget curr j))
                         (inc (aget prev (inc j)))
                         (+ (aget prev j) cost)))))
          (System/arraycopy curr 0 prev 0 (inc n)))
        (aget prev n)))))

(defn did-you-mean
  "Return the candidate from `candidates` (a seq of strings) most similar
   to `target`, or nil if nothing is plausibly close. A match is
   plausibly close if its edit distance is at most `(max 2 (/ (count target) 3))`."
  [target candidates]
  (when (and target (seq candidates))
    (let [target (name target)
          best   (apply min-key
                        #(levenshtein target (name %))
                        candidates)
          dist   (levenshtein target (name best))
          limit  (max 2 (quot (count target) 3))]
      (when (<= dist limit)
        (name best)))))

;; ---------------------------------------------------------------------------
;; Source snippet rendering
;; ---------------------------------------------------------------------------

(defn ^:private try-read-template
  "Return the template source as a string, or nil if it can't be read.

   Accepts:
   - java.net.URL
   - keyword (returns nil; e.g. :string)
   - file path string (relative to working directory)
   - classpath resource name
   - string of the form 'file:/...' (as produced by Selmer's
     resource-path when reporting validation errors)"
  [template]
  (when template
    (try
      (cond
        (instance? java.net.URL template) (slurp template)
        (keyword? template)               nil
        (string? template)
        (or (try (when (.exists (File. ^String template))
                   (slurp template))
                 (catch Throwable _ nil))
            (when (or (.startsWith ^String template "file:")
                      (.startsWith ^String template "jar:"))
              (try (slurp (java.net.URL. ^String template))
                   (catch Throwable _ nil)))
            (when-let [r (io/resource template)]
              (slurp r))))
      (catch Throwable _ nil))))

(defn ^:private gutter-width [^long line]
  (-> line str count))

(defn ^:private render-snippet
  "Build the multi-line snippet block. Returns nil if no source is available."
  [location]
  (when-let [src (try-read-template (:template location))]
    (let [lines        (str/split src #"\r\n|\n|\r" -1)
          line         (long (:line location))
          col          (long (:col location 1))
          end-line     (:end-line location)
          end-col      (:end-col location)
          single-line? (or (nil? end-line) (= line end-line))
          width        (gutter-width (max line (or end-line 0)))
          fmt-line     (fn [n s]
                         (format (str "%" width "d | %s") n s))
          fmt-gutter   (str (apply str (repeat width \space)) " |")
          target-text  (nth lines (dec line) "")
          caret-len    (if (and single-line? end-col)
                         (max 1 (- (long end-col) col))
                         (max 1 (- (count target-text) (dec col))))
          caret-line   (str fmt-gutter " "
                            (apply str (repeat (dec col) \space))
                            (apply str (repeat caret-len \^))
                            " here")
          context-line (fn [n]
                         (when (and (pos? n) (<= n (count lines)))
                           (fmt-line n (nth lines (dec n)))))]
      (->> [(context-line (- line 2))
            (context-line (- line 1))
            (fmt-line line target-text)
            caret-line]
           (remove nil?)
           (str/join "\n")))))

;; ---------------------------------------------------------------------------
;; Public formatter
;; ---------------------------------------------------------------------------

(defn ^:private error-kind [data]
  (case (:type data)
    :selmer/parse-error       "template parse error"
    :selmer/render-error      "template render error"
    :selmer/validation-error  "template validation error"
    "template error"))

(defn ^:private validation-location
  "Synthesize a :selmer.util/location from the legacy
   :selmer/validation-error shape ({:template path :line N :validation-errors [...]}).
   Returns nil if neither :template nor :line is present.

   :col is not populated by the validator, so the caret defaults to
   column 1; the source snippet still anchors the user to the right
   line."
  [data]
  (let [t (:template data)
        ;; The top-level :line on validation errors is sometimes nil
        ;; when the validator can't pin one down. Fall back to the
        ;; first validation-errors entry that does have a line.
        l (or (:line data)
              (some :line (:validation-errors data)))]
    (when (or t l)
      (cond-> {}
        t (assoc :template (cond (instance? java.net.URL t) (str t)
                                 :else                       t))
        l (assoc :line l :col 1)))))

(defn ^:private location-line [location]
  (let [t (:template location)
        l (:line location)
        c (:col location)]
    (cond
      (and t l c) (str "  --> " (if (keyword? t) (name t) t) ":" l ":" c)
      (and t l)   (str "  --> " (if (keyword? t) (name t) t) ":" l)
      t           (str "  --> " (if (keyword? t) (name t) t))
      :else       nil)))

(defn ^:private suggestion-line [data]
  (when-let [tag-name (some-> (:tag-name data) name)]
    (when-let [candidates (some-> (resolve 'selmer.tags/expr-tags)
                                  deref
                                  deref
                                  keys
                                  seq)]
      (let [candidate-names (set (map name candidates))]
        ;; Only suggest when the tag-name is NOT itself registered.
        ;; If it IS registered, this is an unclosed-block error (the
        ;; tag exists, it just wasn't closed) and a suggestion would
        ;; be misleading.
        (when-not (contains? candidate-names tag-name)
          (when-let [guess (did-you-mean tag-name candidate-names)]
            (str "  = did you mean `" guess "`?")))))))

(defn format-error
  "Format a Selmer exception with source context.

  `t` is anything supported by `clojure.core/ex-data`: typically an
  `ExceptionInfo` thrown by parse* or render-template. Returns a
  multi-line string ready for printing.

  Never throws. If anything goes wrong the bare exception message is
  returned."
  ^String [^Throwable t]
  (try
    (let [data       (ex-data t)
          ;; For legacy :selmer/validation-error exceptions, synthesize
          ;; a location from the top-level :template/:line keys so the
          ;; formatter can still produce a source snippet.
          location   (or (:selmer.util/location data)
                         (when (= :selmer/validation-error (:type data))
                           (validation-location data)))
          opener     (:selmer.util/opener-location data)
          header     (str (error-kind data) ": " (.getMessage t))
          loc-line   (some-> location location-line)
          snippet    (some-> location render-snippet)
          suggest    (suggestion-line data)
          opener-loc (some-> opener location-line)
          opener-snp (some-> opener render-snippet)]
      (->> [header
            loc-line
            snippet
            suggest
            (when opener-loc "\nopened at:")
            opener-loc
            opener-snp]
           (remove nil?)
           (str/join "\n")))
    (catch Throwable _
      (or (.getMessage t) (str t)))))
