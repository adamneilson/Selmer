(ns selmer.middleware
  (:require [clojure.string :as str]
            [selmer.errors :as errors]
            [selmer.parser :as parser]))

(defn ^:private html-escape [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

(defn ^:private location-aware-error-page [ex]
  (let [body (str "<!doctype html><html><head><meta charset='utf-8'>"
                  "<title>Template error</title>"
                  "<style>"
                  "body{font:14px/1.5 -apple-system,Segoe UI,sans-serif;"
                  "margin:0;background:#1d1f21;color:#c5c8c6;}"
                  "h1{background:#a32306;color:#fff;margin:0;"
                  "padding:14px 24px;font-size:18px;font-weight:600;}"
                  "pre{margin:0;padding:24px;font:13px/1.55 "
                  "'SF Mono',Menlo,Consolas,monospace;white-space:pre-wrap;"
                  "color:#c5c8c6;}"
                  ".loc{color:#b5bd68;}"
                  "</style></head><body>"
                  "<h1>Template error</h1>"
                  "<pre>" (html-escape (errors/format-error ex)) "</pre>"
                  "</body></html>")]
    {:status  500
     :headers {"Content-Type" "text/html; charset=utf-8"}
     :body    body}))

(defn handle-template-parsing-error [ex]
  (let [{:keys [type error-template] :as data} (ex-data ex)]
    (cond
      ;; Legacy validation errors: the parser/validator produced an
      ;; HTML template to render against the ex-data. Preserved for
      ;; back-compat with anything depending on the historical page.
      (= :selmer/validation-error type)
      {:status  500
       :headers {"Content-Type" "text/html; charset=utf-8"}
       :body    (parser/render error-template data)}

      ;; New-style parse and render errors carry source locations
      ;; that the errors formatter can turn into a snippet.
      (contains? #{:selmer/parse-error :selmer/render-error} type)
      (location-aware-error-page ex)

      :else
      (throw ex))))

(defn wrap-error-page
  "Development middleware that renders a friendly error page when a
   Selmer parse or render error escapes the handler.

   Recognised exception types:
   - :selmer/validation-error  legacy HTML error-template flow
   - :selmer/parse-error       formatted via selmer.errors/format-error
   - :selmer/render-error      formatted via selmer.errors/format-error

   Other exceptions are re-thrown so the surrounding stack can handle them."
  [handler]
  (fn
    ([request]
     (try
       (handler request)
       (catch clojure.lang.ExceptionInfo ex
         (handle-template-parsing-error ex))))
    ([request respond raise]
     (try
       (handler request respond raise)
       (catch clojure.lang.ExceptionInfo ex
         (respond (handle-template-parsing-error ex)))))))
