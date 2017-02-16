(ns pureclj.core-spec
  (:require [pureclj.core :refer :all]
            [midje.sweet :refer :all]))

[[:chapter "symbols: Extract symbols from expressions"]]
"should return an empty set for a constant"
(fact 
 (symbols 2) => #{})
"should return an empty set for a boolean"
(fact 
 (symbols true) => #{})
"should return an empty set for a keyword"
(fact
 (symbols :foo) => #{})
"should return en empty set for nil"
(fact
 (symbols nil) => #{})
"should return an empty set for a string literal"
(fact 
 (symbols "foo") => #{})
"should return a singleton set for a symbol"
(fact
 (symbols 'x) => #{'x})
"should reconstruct lists"
(fact
 (symbols '(+ a b)) => #{'+ 'a 'b})
"should reconstruct a vector"
(fact
 (symbols '[1 a 2 b]) => #{'a 'b} )
"should reconstruct a map"
(fact
 (symbols {'x 'y}) => #{'x 'y} )
"should reconstruct a set"
(fact
 (symbols #{'a 'b 3 4 5}) => #{'a 'b} )
"should remove bound variables in a let* form"
(fact
 (symbols '(let* [x 1 y 2] (+ x y))) => #{'+} )
"should add symbols in bindings expressions"
(fact
 (symbols '(let* [x a] x)) => #{'a} )
"should apply macros"
(fact
 (symbols '(let [x a] x a)) => #{'a} )
"should remove symbols from an unnamed fn* arg list"
(fact
 (symbols '(fn [x y] (+ x y a))) => #{'a '+} )
"should remove symbols from a named fn*"
(fact
 (symbols '(fn foo [x y] (+ x y a))) => #{'a '+} )
"should remove the function name in case of a recursive call"
(fact
 (symbols '(fn foo [x] (foo x))) => #{} )
"should support multi-clause functions"
(fact
 (symbols '(fn ([x] (+ a x))
             ([x y] (+ b x y)))) => #{'a 'b '+} )
"should throw an exception on a def form"
(fact
 (symbols '(def x 2)) => (throws "def is not allowed") )
"should ignore the contents of a quote"
(fact
 (symbols ''(a b c)) => #{} )
"should ignore the 'if' of an if form"
(fact
 (symbols '(if a b c)) => #{'a 'b 'c} )
"should ignore the 'do' of a do form"
(fact
 (symbols '(do a b c)) => #{'a 'b 'c} )
"should throw an exception on a var form"
(fact
 (symbols '#'var) => (throws "vars are not allowed") )
"should handle the bindings of a loop"
(fact
 (symbols '(loop [x a y b] a x b y c)) => #{'a 'b 'c} )
"should ignore the 'recur' in a recur form"
(fact
 (symbols '(recur a b)) => #{'a 'b} )
"should throw an exception on a throw form"
(fact
 (symbols '(throw foo)) => (throws Exception "throw is not allowed. Use error instead") )
"should throw an exception on a try form"
(fact
 (symbols '(try foo bar baz)) => (throws "try/catch is not allowed") )

[[:chapter {:title "box: Evaluate expressions through an environment" :tag "box"}]]
"should return a constant for a constant"
(fact
 (box 3.14 {}) => 3.14)
"should throw an exception if a symbol that exists in expr is not a key in env"
(fact
 (box 'x {}) => (throws "symbols #{x} are not defined in the environment") )
"should assign symbols their value in the environment"
(fact
 (box '(+ x 2) {'x 3 '+ *}) => 6 )
"should work with functions already defined in the environment"
(fact
 (let [env (update-env '(defn foo [x] (inc x)) {'inc inc})]
   (box '(foo 2) env)) => 3 )
"should work with qualified symbols as long as they are in env"
(fact
 (box '(clojure.core/inc 2) (add-ns 'clojure.core {})) => 3 )


[[:chapter "update-env: Update an environment based on defs" :tag "update-env"]]
"should add a key to env of a def form"
(fact
               (update-env '(def y (inc x)) {'x 2 'inc inc}) => {'x 2 'y 3 'inc inc} )
"should apply macros"
(fact
      (let [env (update-env '(defn foo [x] (inc x)) {'inc inc})
            f (env 'foo)]
        (f 1)) => 2 )
"should apply all defs in a do block"
(fact
 (update-env '(do (def x 1) (def y 2)) {}) => {'x 1 'y 2} )

[[:chapter {:title "add-ns: Import a namespace to an environment" :tag "add-ns"}]]

"should extend env"
(fact
 (add-ns 'clojure.core {'x 3}) => #(contains? % 'x))
"should add all publics from ns"
(fact
 (add-ns 'clojure.core {'x 3}) => #(contains? % '+))
"should apply filters if supplied"
(fact
 (add-ns 'clojure.core [(name-filter-out #".*!")] {}) => #(not (contains? % 'swap!)))
"should apply multiple filters if supplied"
(fact
 (add-ns 'clojure.core [(name-filter-out #".*!") (name-filter-out #"print.*")] {}) => #(not (contains? % 'println)))
"should return an env containing functions"
(fact
 (let [env (add-ns 'clojure.core {})
       f (env 'inc)]
   (f 2)) => 3 )
"should also add the fully-qualified names to env"
(fact
 (add-ns 'clojure.core {}) => #(contains? % 'clojure.core/+))

[[:chapter {:title "name-filter"}]]
"should return true for env entries that  match the patter"
(fact
 (filter (name-filter #"x+") ['x 'xx 'xy 'yy]) => '(x xx) )

[[:chapter {:title "name-filter-out"}]]
"should return true for env entries that  match the patter"
(fact
 (filter (name-filter-out #"x+") ['x 'xx 'xy 'yy]) => '(xy yy) )

