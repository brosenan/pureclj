(ns pureclj.core
  (:use [clojure.set]))

(defmulti symbols class)
(defmethod symbols Number [expr] #{})
(defmethod symbols String [expr] #{})
(defmethod symbols nil [expr] #{})
(defmethod symbols clojure.lang.Symbol [symbol] #{symbol})

(defmethod symbols clojure.lang.ISeq [seq]
  (if (empty? seq)
    #{}
    (union (symbols (first seq)) (symbols (rest seq)))))

(defmethod symbols clojure.lang.PersistentVector [vec]
  (if (empty? vec)
    #{}
    (union (symbols (first vec)) (symbols (rest vec)))))

(defmethod symbols clojure.lang.IPersistentMap [map]
  (union (symbols (keys map)) (symbols (vals map))))
