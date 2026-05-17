(ns selmer.reader
  "Position-aware character reader for the Selmer parser.

  Wraps a java.io.Reader and tracks line and column on every read so that
  tokens and AST nodes can carry their source location through to error
  messages.

  Line counting handles all three common line endings:
  - \\n        (Unix)
  - \\r\\n     (Windows; counted as one break)
  - \\r        (classic Mac)

  Columns are 1-based and measured in UTF-16 code units, matching what
  java.io.Reader exposes. BMP characters count as one column; characters
  outside the BMP (surrogate pairs) count as two. Tab is one column.

  Position points at the *next* character to be read. After consuming the
  final character of a 3-char single-line input, position is
  {:line 1 :col 4}."
  (:require [clojure.java.io :as io])
  (:import [java.io PushbackReader]))

(defprotocol IPositionReader
  (-read-char [this]
    "Read one character and advance position. Returns the character or
     nil at EOF. Position is unchanged on EOF.")
  (-peek-char [this]
    "Return the next character without advancing position, or nil at EOF.")
  (-position [this]
    "Return {:line :col :template} pointing at the next character.")
  (-last-position [this]
    "Return {:line :col :template} pointing at the most recently read
     character. Equals -position before any reads."))

(deftype PositionReader
    [^PushbackReader rdr
     ^:unsynchronized-mutable line
     ^:unsynchronized-mutable col
     ^:unsynchronized-mutable prev-line
     ^:unsynchronized-mutable prev-col
     ^:unsynchronized-mutable prev-cr?
     template]
  IPositionReader
  (-read-char [_]
    (let [c (.read rdr)]
      (when (not= c -1)
        (set! prev-line line)
        (set! prev-col col)
        (let [ch (char c)]
          (cond
            (= ch \return)
            (do (set! line (inc line))
                (set! col 1)
                (set! prev-cr? true))

            (= ch \newline)
            (do (when-not prev-cr?
                  (set! line (inc line)))
                (set! col 1)
                (set! prev-cr? false))

            :else
            (do (set! col (inc col))
                (set! prev-cr? false)))
          ch))))

  (-peek-char [_]
    (let [c (.read rdr)]
      (when (not= c -1)
        (.unread rdr c)
        (char c))))

  (-position [_]
    {:line line :col col :template template})

  (-last-position [_]
    {:line prev-line :col prev-col :template template})

  java.io.Closeable
  (close [_] (.close rdr)))

(defn ->position-reader
  "Wrap `input` in a PositionReader. `input` is anything that
   clojure.java.io/reader accepts (Reader, File, URL, URI, Socket,
   InputStream, byte array, or path String). To pass literal template
   content, wrap it in a java.io.StringReader first.

   `template` is an optional source identifier surfaced via `position`.
   Defaults to :string."
  ([input] (->position-reader input :string))
  ([input template]
   (PositionReader. (PushbackReader. (io/reader input) 1)
                    1 1 1 1 false template)))

(defn read-char    [pr] (-read-char pr))
(defn peek-char    [pr] (-peek-char pr))
(defn position     [pr] (-position pr))
(defn last-position [pr] (-last-position pr))
