(ns hiccup-ring-utils.core
  (:require [hiccup.core :refer [html]]
            [clojure.stacktrace :refer [print-stack-trace]]))

(defmacro wrap-http-exception [& form]
  "catch any clj-http/client exceptions and return them as a string"
  `(try ~@form
        (catch Exception e#
          (if (-> e# .getMessage (.startsWith "clj-http: status"))
            (str (-> e# .getData :status) ":"
                 (-> e# .getData :body))
            (do
              ;;(print-stack-trace e#)
              (throw e#))))))


(defn- assoc-noclobber
  ;;https://github.com/amalloy/useful/blob/bcb07414cf3dd5a09794b76490c0cf18758f1888/src/flatland/useful/parallel.clj#L33
  "An assoc wrapper which ensures that existing keys will not be
  clobbered by subsequent assoc invocations.
  Used as a helper for locking-memoize to ensure that (delay) refs
  cannot be lost by swap! retry behavior."

  [m k v]
  (if (contains? m k) m
      (assoc m k v)))

(defn pmemoize
  ;;https://github.com/amalloy/useful/blob/bcb07414cf3dd5a09794b76490c0cf18758f1888/src/flatland/useful/parallel.clj#L33
  "Memoizes the function f, using the same approach as
  clojure.core/memoize. The practical difference is that this function
  provides the gurantee that in spite of parallel invocations of the
  memoized function each input to f will only ever be memoized
  once. This resolves an implementation detail in clojure.core/memoize
  which allows f to be applied to args without locking the cache to
  prevent other threads duplicating the work."

  [f]
  (let [mem (atom {})]
    (fn [ & args ]
      (if-let [e (find @mem args)]
        (deref (val e))
        (-> (swap! mem assoc-noclobber
                   args (delay (apply f args)))
            (get args)
            (deref))))))

(defmacro defn-pmemoized [& defn-args]
  "like defn, but memoized with pmemoize"
  (let [name (first defn-args)
        rest (rest defn-args)]
  `(do (defn ~name ~@rest)
       (def ~name (pmemoize ~name)))))

(defn format-keywords [string keywords-map]
  ;;(->> (clojure.walk/stringify-keys keywords-map)
  (->> (map (juxt (comp str first) second) keywords-map)
       flatten
       (apply hash-map)
       (reduce-kv clojure.string/replace
                  string)))

(def js-lazy-load-self-replace
  "function XHR_self_replace_:ID () {
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function() {
	if (xmlhttp.readyState == XMLHttpRequest.DONE ) {
	    var innerHTML = xmlhttp.responseText;
            document.getElementById(':ID').innerHTML = innerHTML;
	}
    };

    xmlhttp.open('GET', ':URL', true);
    xmlhttp.send();
  }
  XHR_self_replace_:ID();")

(defn lazy-element
  "Return a 'lazy' element which makes an XHR request to the
specified url, then overwrites itself with the reponse contents.
An optinal tmp-contents are shown while XHR is executed"
  ([url] (lazy-element url nil))
  ([url tmp-contents]
   (let [id (str (rand-int Integer/MAX_VALUE))
         javascript (format-keywords
                     js-lazy-load-self-replace
                     {:ID id :URL url})]
     [:div {:id id}
      [:script {:type "text/javascript"} javascript]
      tmp-contents])))


(defn html-table-for-rows [rows & additional-args]
  "construct an html table from the given rows of
hiccup html elements"
  (apply vector :table (first additional-args)
         (map (comp
               (partial apply vector :tr)
               (partial map (partial vector :td)))
              rows)))

(defn html-table-from-map [clj-map]
  "construct an html table from the given associative structure"
  (html-table-for-rows
   (map (fn [[name val]]
          [[:div [:b name] ": "]
           [:div (or (str val) "NULL")]]) clj-map)))

(defn route-for-entries-table
  [req-to-entries-fun
   column-names
   entry-to-html-row-fun]
  "takes a function req -> [entry1 entry2 ...],
a vector of column names [col1-name col2-name ...]
a function entry -> [entry-col1-value entry-col2-value]
returns a route that produces the specified html table
"
  (fn [req]
    (->
     (->> (req-to-entries-fun req)
          (pmap entry-to-html-row-fun)
          (map-indexed (fn [i row] (into [i] row)))
          (concat [(map (partial vector :b)
                        (into ["#"] column-names))]))
     (html-table-for-rows {:border true})
     html
     wrap-http-exception)))
