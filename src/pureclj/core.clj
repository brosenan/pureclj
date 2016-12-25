(ns pureclj.core
  (:use [clojure.set]))

(declare list-symbols)

(defmulti symbols class :default :no-symbols)
(defmethod symbols :no-symbols [expr] #{})

(defmethod symbols clojure.lang.Symbol [symbol] #{symbol})

(defmethod symbols clojure.lang.ISeq [seq]
  (apply list-symbols (macroexpand seq)))

(defmethod symbols clojure.lang.PersistentVector [vec]
  (if (empty? vec)
    #{}
    (union (symbols (first vec)) (symbols (rest vec)))))

(defmethod symbols clojure.lang.IPersistentMap [map]
  (union (symbols (keys map)) (symbols (vals map))))

(defmethod symbols clojure.lang.IPersistentSet [set]
  (symbols (seq set)))

(defmulti list-symbols (fn ([first & args] first)
                           ([] :function-call)) :default :function-call)

(defmethod list-symbols :function-call [& seq]
  (if (empty? seq)
    #{}
    (union (symbols (first seq)) (symbols (rest seq)))))

(defn bindings-symbols [bindings syms]
  (if (empty? bindings)
    syms
    (let [[pattern val] (take 2 bindings)]
      (union (symbols val) (difference
                             (bindings-symbols (drop 2 bindings) syms)
                             (symbols pattern))))))

(defmethod list-symbols 'let* [_ bindings expr]
  (bindings-symbols bindings (symbols expr)))

(defn fn-bindings [bindings]
  (if (empty? bindings)
    #{}
    (let [[args & body] (first bindings)]
      (union (difference (symbols body) (symbols args))
             (fn-bindings (rest bindings))))))

(defmethod list-symbols 'fn*
  ([_ & bindings]
   (if (symbol? (first bindings))
     (fn-bindings (rest bindings))
     (fn-bindings bindings))))
