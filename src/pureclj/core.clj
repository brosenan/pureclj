(ns pureclj.core)

(defmulti transpile class)
(defmethod transpile Number [expr] (fn [x] expr))
(defmethod transpile String [expr] (fn [x] expr))
(defmethod transpile clojure.lang.Symbol [sym] (fn [map] (map sym)))


(def safe-functions #{+})
(defmethod transpile clojure.lang.ASeq [expr]
  (if (contains? safe-functions (first expr))
    (fn [map] (eval expr))
    (throw (Exception. (str "Attempt to use an unsafe function " (first expr))))))
