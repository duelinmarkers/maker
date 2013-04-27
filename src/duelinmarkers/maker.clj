(ns duelinmarkers.maker
  (:refer-clojure :exclude (extend))
  (:require clojure.pprint))

(defonce prototypes (atom {}))

(defn- extend [bases object]
  (if-let [missing (seq (remove @prototypes bases))]
    (throw (IllegalArgumentException.
            (str "Unknown base prototype(s): " missing)))
    (let [objects (-> (mapv @prototypes bases) (conj object))]
      (reduce conj objects))))

(defn add-prototype
  ([k object] (add-prototype k [] object))
  ([k bases object]
     (let [new-prototype (extend bases object)]
       (swap! prototypes assoc k new-prototype))))

(defn- do-generate [done todo]
  (loop [done done todo todo
         [k gen-v :as entry] (first todo) breadcrumbs []]
    #_(clojure.pprint/pprint {:done done :todo todo
                              :entry entry :breadcrumbs breadcrumbs})
    (when (> (count breadcrumbs) 50)
      (throw (IllegalArgumentException. (str "Looks like a circular dependency. Break the circle by specifying one of " breadcrumbs))))
    (if (nil? entry)
      done
      (case (::gen (meta gen-v))
        true
        (let [todo (dissoc todo k)]
          (recur (assoc done k (apply (first gen-v) (rest gen-v)))
                 todo
                 (first todo)
                 breadcrumbs))
        :from
        (let [[arg-keys f] gen-v]
          (if-let [not-done
                   (first (filter #(not (contains? done %)) arg-keys))]
            (recur done
                   todo
                   [not-done (get todo not-done)]
                   (conj breadcrumbs k))
            (let [todo (dissoc todo k)]
              (recur (assoc done k
                            (apply f (map (partial get done) arg-keys)))
                     todo
                     (first todo)
                     breadcrumbs))))
        (throw (ex-info {:done done :todo todo :entry entry
                         :breadcrumbs breadcrumbs}))))))

(defn- generate [o]
  (let [{done false todo true}
        (group-by #(contains? (meta (val %)) ::gen) o)
        done (into (empty o) done)
        todo (into {} todo)]
    (do-generate done todo)))

(defn make
  "Provides the named prototype or an object derived from one."
  {:arglists '([k] [k override-object] [k & override-kvs])}
  ([k] (make k {}))
  ([k overrides]
     (generate (conj (get @prototypes k) overrides)))
  ([k k1 v1 & {:as overrides}]
     (let [overrides (assoc overrides k1 v1)]
       (make k overrides))))

(defn gen {:arglists '([f & args])}
  [& args] (with-meta args {::gen true}))

(defn gen-from-fn [& args]
  (with-meta args {::gen :from}))

(defmacro gen-from [deps & body]
  `(gen-from-fn ~(mapv keyword deps) (fn ~deps ~@body)))
