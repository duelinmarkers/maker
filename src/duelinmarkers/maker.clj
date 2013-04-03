(ns duelinmarkers.maker
  (:refer-clojure :exclude (extend)))

(defonce prototypes (atom {}))

(defn- extend [bases object]
  (let [objects (-> (mapv @prototypes bases) (conj object))]
    (reduce conj objects)))

(defn add-prototype
  "Typically called in preloaded test startup code, adds a maker prototype.
  If it's to participate in extension, it should support conj (i.e., any
  IPersistentCollection could work, including maps and records). "
  ([k object] (add-prototype k [] object))
  ([k bases object]
     (let [new-prototype (extend bases object)]
       (swap! prototypes assoc k new-prototype))))

(defmulti make
  "Provides the named prototype or an object derived from one."
  {:arglists '([k] [k override-object] [k & override-kvs])}
  #(first %&))

(defmethod make :default
  ([k] (get @prototypes k))
  ([k overrides]
     (conj (make k) overrides))
  ([k k1 v1 & {:as overrides}]
     (let [overrides (assoc overrides k1 v1)]
       (make k overrides))))
