(ns selmer.reader-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [selmer.reader :as r])
  (:import [java.io StringReader]))

(defn- ->reader
  "Wrap a literal content string so ->position-reader treats it as input."
  ([s]          (r/->position-reader (StringReader. s)))
  ([s template] (r/->position-reader (StringReader. s) template)))

(defn- read-all [pr]
  (loop [out []]
    (if-let [c (r/read-char pr)]
      (recur (conj out c))
      out)))

(deftest initial-position
  (let [pr (->reader "")]
    (is (= {:line 1 :col 1 :template :string} (r/position pr)))))

(deftest custom-template-name
  (let [pr (->reader "abc" "foo.html")]
    (is (= "foo.html" (:template (r/position pr))))))

(deftest read-char-returns-char
  (let [pr (->reader "ab")]
    (is (= \a (r/read-char pr)))
    (is (= \b (r/read-char pr)))
    (is (nil? (r/read-char pr)))))

(deftest read-char-advances-column
  (let [pr (->reader "abc")]
    (r/read-char pr)
    (is (= {:line 1 :col 2 :template :string} (r/position pr)))
    (r/read-char pr)
    (r/read-char pr)
    (is (= {:line 1 :col 4 :template :string} (r/position pr)))))

(deftest position-stable-at-eof
  (let [pr (->reader "ab")]
    (r/read-char pr) (r/read-char pr)
    (let [pos-at-eof (r/position pr)]
      (r/read-char pr)
      (r/read-char pr)
      (is (= pos-at-eof (r/position pr))))))

(deftest newline-unix
  (let [pr (->reader "ab\ncd")]
    (dotimes [_ 3] (r/read-char pr))
    (is (= {:line 2 :col 1 :template :string} (r/position pr)))
    (r/read-char pr)
    (r/read-char pr)
    (is (= {:line 2 :col 3 :template :string} (r/position pr)))))

(deftest newline-windows-crlf
  (let [pr (->reader "a\r\nb")]
    (r/read-char pr)
    (is (= 2 (:col (r/position pr))))
    (r/read-char pr)
    (is (= {:line 2 :col 1 :template :string} (r/position pr)))
    (r/read-char pr)
    (is (= {:line 2 :col 1 :template :string} (r/position pr)))
    (r/read-char pr)
    (is (= {:line 2 :col 2 :template :string} (r/position pr)))))

(deftest newline-classic-mac-cr
  (let [pr (->reader "a\rb")]
    (r/read-char pr)
    (r/read-char pr)
    (is (= {:line 2 :col 1 :template :string} (r/position pr)))
    (r/read-char pr)
    (is (= {:line 2 :col 2 :template :string} (r/position pr)))))

(deftest mixed-line-endings
  (let [pr (->reader "a\nb\r\nc\rd")]
    (is (= [\a \newline \b \return \newline \c \return \d] (read-all pr)))
    (is (= 4 (:line (r/position pr))))))

(deftest empty-lines
  (let [pr (->reader "\n\n\nx")]
    (r/read-char pr)
    (r/read-char pr)
    (r/read-char pr)
    (is (= {:line 4 :col 1 :template :string} (r/position pr)))
    (r/read-char pr)
    (is (= {:line 4 :col 2 :template :string} (r/position pr)))))

(deftest peek-does-not-advance
  (let [pr (->reader "abc")]
    (r/read-char pr)
    (is (= \b (r/peek-char pr)))
    (is (= {:line 1 :col 2 :template :string} (r/position pr)))
    (is (= \b (r/read-char pr)))
    (is (= {:line 1 :col 3 :template :string} (r/position pr)))))

(deftest peek-at-eof
  (let [pr (->reader "")]
    (is (nil? (r/peek-char pr)))
    (is (= {:line 1 :col 1 :template :string} (r/position pr)))))

(deftest peek-then-read-many-times
  (let [pr (->reader "ab")]
    (is (= \a (r/peek-char pr)))
    (is (= \a (r/peek-char pr)))
    (is (= \a (r/peek-char pr)))
    (is (= \a (r/read-char pr)))
    (is (= \b (r/peek-char pr)))
    (is (= \b (r/read-char pr)))))

(deftest peek-across-newline
  (let [pr (->reader "\nb")]
    (is (= \newline (r/peek-char pr)))
    (is (= {:line 1 :col 1 :template :string} (r/position pr)))
    (r/read-char pr)
    (is (= {:line 2 :col 1 :template :string} (r/position pr)))))

(deftest unicode-bmp-chars-are-one-column
  (let [pr (->reader "café")]
    (read-all pr)
    (is (= {:line 1 :col 5 :template :string} (r/position pr)))))

(deftest tab-counts-as-one-column
  (let [pr (->reader "a\tb")]
    (r/read-char pr)
    (r/read-char pr)
    (is (= {:line 1 :col 3 :template :string} (r/position pr)))))

(deftest large-input
  (let [lines (str/join "\n" (repeat 1000 "hello world"))
        pr    (->reader lines)]
    (read-all pr)
    (is (= 1000 (:line (r/position pr))))
    (is (= 12   (:col (r/position pr))))))

(deftest accepts-reader-input
  (let [pr (r/->position-reader (java.io.StringReader. "ab"))]
    (is (= [\a \b] (read-all pr)))))

(deftest closeable
  (let [pr (->reader "abc")]
    (.close pr)
    (is true "closing a PositionReader does not throw")))

(deftest position-suitable-for-tokens
  ;; Pattern that read-tag-info will use: snapshot position before a tag,
  ;; consume the tag body, snapshot position after. The two positions
  ;; delimit the tag in source.
  (let [pr (->reader "ab{{name}}cd")]
    (dotimes [_ 2] (r/read-char pr))
    (let [start (r/position pr)]
      (is (= {:line 1 :col 3 :template :string} start))
      (dotimes [_ 8] (r/read-char pr))
      (let [end (r/position pr)]
        (is (= {:line 1 :col 11 :template :string} end))))))

(deftest position-across-multi-line-tag
  (let [pr (->reader "{% if\n   x %}")]
    (let [start (r/position pr)]
      (is (= {:line 1 :col 1 :template :string} start))
      (read-all pr)
      (let [end (r/position pr)]
        (is (= 2 (:line end)))))))

(deftest last-position-before-any-read
  (let [pr (->reader "abc")]
    (is (= (r/position pr) (r/last-position pr)))))

(deftest last-position-after-single-read
  (let [pr (->reader "abc")]
    (r/read-char pr)
    (is (= {:line 1 :col 1 :template :string} (r/last-position pr)))
    (is (= {:line 1 :col 2 :template :string} (r/position pr)))))

(deftest last-position-points-at-most-recent-char
  ;; Pattern used by read-tag-info: parse* has just read \{, calls
  ;; read-tag-info; the tag-start is the \{ which is now last-position.
  (let [pr (->reader "ab{%foo%}")]
    (dotimes [_ 2] (r/read-char pr))
    (r/read-char pr)
    (is (= {:line 1 :col 3 :template :string} (r/last-position pr))
        "last-position is the position of the just-read \\{")))

(deftest last-position-across-newline
  (let [pr (->reader "a\nb")]
    (r/read-char pr)
    (r/read-char pr)
    (r/read-char pr)
    (is (= {:line 2 :col 1 :template :string} (r/last-position pr))
        "last-position of the b is line 2 col 1")
    (is (= {:line 2 :col 2 :template :string} (r/position pr)))))

(deftest last-position-not-affected-by-peek
  (let [pr (->reader "abc")]
    (r/read-char pr)
    (let [before (r/last-position pr)]
      (r/peek-char pr)
      (r/peek-char pr)
      (is (= before (r/last-position pr))))))
