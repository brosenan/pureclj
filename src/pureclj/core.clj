(ns pureclj.core
  (:use [clojure.set]))

(declare list-symbols)

(defmulti symbols class)
(derive Number ::primitive)
(derive String ::primitive)

(defmethod symbols ::primitive [expr] #{})
(defmethod symbols nil [expr] #{})

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

(defmulti ^:private list-symbols (fn ([first & args] first)
                           ([] :function-call)) :default :function-call)

(defmethod list-symbols :function-call [& seq]
  (if (empty? seq)
    #{}
    (union (symbols (first seq)) (symbols (rest seq)))))

(defn ^:private bindings-symbols [bindings syms]
  (if (empty? bindings)
    syms
    (let [[pattern val] (take 2 bindings)]
      (union (symbols val) (difference
                             (bindings-symbols (drop 2 bindings) syms)
                             (symbols pattern))))))

(defmethod list-symbols 'let* [_ bindings & expr]
  (bindings-symbols bindings (symbols expr)))

(defn ^:private fn-bindings [bindings]
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

(defmethod list-symbols 'def
  ([_ & args] (throw (Exception. "def is not allowed"))))

(defmethod list-symbols 'quote
  ([_ quoted] #{}))

(defmethod list-symbols 'if
  ([_ & args] (symbols args)))

(defmethod list-symbols 'do
  ([_ & args] (symbols args)))

(defmethod list-symbols 'var
  ([_ sym] (throw (Exception. "vars are not allowed"))))

(defmethod list-symbols 'loop*
  ([_ bindings & body] (bindings-symbols bindings (symbols body))))

(defmethod list-symbols 'recur
  ([_ & exprs] (symbols exprs)))

(defmethod list-symbols 'throw
  ([_ exception] (throw (Exception. "throw is not allowed. Use error instead"))))

(defmethod list-symbols 'try
  ([_ & body] (throw (Exception. "try/catch is not allowed"))))


(defn ^:private create-bindings [syms env]
  (apply concat (for [sym syms] [sym (env sym)])))

(defn box [expr env]
  (let [syms (symbols expr)
        missing (difference syms (set (keys env)))]
    (if (empty? missing)
      (eval `(let [~@(create-bindings syms env)] ~expr))
      (throw (Exception. (str "symbols " missing " are not defined in the environment"))))))
