(ns duelinmarkers.maker
  (:refer-clojure :exclude (extend))
  (:require [clojure.walk :refer (prewalk)]))

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

(defn- generate [o]
  (prewalk (fn [val]
             (if (::gen (meta val)) (apply (first val) (rest val)) val))
           o))

(defmulti make
  "Provides the named prototype or an object derived from one."
  {:arglists '([k] [k override-object] [k & override-kvs])}
  #(first %&))

(defmethod make :default
  ([k] (make k {}))
  ([k overrides]
     (generate (conj (get @prototypes k) overrides)))
  ([k k1 v1 & {:as overrides}]
     (let [overrides (assoc overrides k1 v1)]
       (make k overrides))))
