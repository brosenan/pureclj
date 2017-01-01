(ns pureclj.core-spec
  (:require [speclj.core :refer :all]
            [pureclj.core :refer :all]))

(describe "(symbols expr)"
  (it "should return an empty set for a constant"
      (should= #{} (symbols 2)))
  (it "should return an empty set for a boolean"
      (should= #{} (symbols true)))
  (it "should return en empty set for nil"
      (should= #{} (symbols nil)))
  (it "should return an empty set for a string literal"
      (should= #{} (symbols "foo")))
  (it "should return a singleton set for a symbol"
      (should= #{'x} (symbols 'x)))
  (it "should reconstruct lists"
      (should= #{'+ 'a 'b} (symbols '(+ a b))))
  (it "should reconstruct a vector"
      (should= #{'a 'b} (symbols '[1 a 2 b])))
  (it "should reconstruct a map"
      (should= #{'x 'y} (symbols {'x 'y})))
  (it "should reconstruct a set"
      (should= #{'a 'b} (symbols #{'a 'b 3 4 5})))
  (it "should remove bound variables in a let* form"
      (should= #{'+} (symbols '(let* [x 1 y 2] (+ x y)))))
  (it "should add symbols in bindings expressions"
      (should= #{'a} (symbols '(let* [x a] x))))
  (it "should apply macros"
      (should= #{'a} (symbols '(let [x a] x a))))
  (it "should remove symbols from an unnamed fn* arg list"
      (should= #{'a '+} (symbols '(fn [x y] (+ x y a)))))
  (it "should remove symbols from a named fn*"
      (should= #{'a '+} (symbols '(fn foo [x y] (+ x y a)))))
  (it "should remove the function name in case of a recursive call"
      (should= #{} (symbols '(fn foo [x] (foo x)))))
  (it "should support multi-clause functions"
      (should= #{'a 'b '+} (symbols '(fn ([x] (+ a x))
                                       ([x y] (+ b x y))))))
  (it "should throw an exception on a def form"
      (should-throw Exception "def is not allowed" (symbols '(def x 2))))
  (it "should ignore the contents of a quote"
      (should= #{} (symbols ''(a b c))))
  (it "should ignore the 'if' of an if form"
      (should= #{'a 'b 'c} (symbols '(if a b c))))
  (it "should ignore the 'do' of a do form"
      (should= #{'a 'b 'c} (symbols '(do a b c))))
  (it "should throw an exception on a var form"
      (should-throw Exception "vars are not allowed" (symbols '#'var)))
  (it "should handle the bindings of a loop"
      (should= #{'a 'b 'c} (symbols '(loop [x a y b] a x b y c))))
  (it "should ignore the 'recur' in a recur form"
      (should= #{'a 'b} (symbols '(recur a b))))
  (it "should throw an exception on a throw form"
      (should-throw Exception "throw is not allowed. Use error instead" (symbols '(throw foo))))
  (it "should throw an exception on a try form"
      (should-throw Exception "try/catch is not allowed" (symbols '(try foo bar baz)))))

(describe "(box expr env)"
          (it "should return a constant for a constant"
              (should= 3.14 (box 3.14 {})))
          (it "should throw an exception if a symbol that exists in expr is not a key in env"
              (should-throw Exception "symbols #{x} are not defined in the environment" (box 'x {})))
          (it "should assign symbols their value in the environment"
              (should= 6 (box '(+ x 2) {'x 3 '+ *})))
          (it "should work with functions already defined in the environment"
              (should= 3 (let [env (update-env '(defn foo [x] (inc x)) {'inc inc})]
                           (box '(foo 2) env))))
          (it "should work with qualified symbols as long as they are in env"
              (should= 3 (box '(clojure.core/inc 2) (add-ns 'clojure.core {})))))

(describe "(update-env expr env)"
          (it "should add a key to env of a def form"
              (should= {'x 2 'y 3 'inc inc} (update-env '(def y (inc x)) {'x 2 'inc inc})))
          (it "should apply macros"
              (should= 2 (let [env (update-env '(defn foo [x] (inc x)) {'inc inc})
                               f (env 'foo)]
                           (f 1))))
          (it "should apply all defs in a do block"
              (should= {'x 1 'y 2} (update-env '(do (def x 1) (def y 2)) {}))))

(describe "(add-ns ns filters? env)"
          (it "should extend env"
              (should-contain 'x (add-ns 'clojure.core {'x 3})))
          (it "should add all publics from ns"
              (should-contain '+ (add-ns 'clojure.core {'x 3})))
          (it "should apply filters if supplied"
              (should-not-contain 'swap! (add-ns 'clojure.core [(name-filter-out #".*!")] {})))
          (it "should apply multiple filters if supplied"
              (should-not-contain 'println (add-ns 'clojure.core [(name-filter-out #".*!") (name-filter-out #"print.*")] {})))
          (it "should return an env containing functions"
              (should= 3 (let [env (add-ns 'clojure.core {})
                             f (env 'inc)]
                           (f 2))))
          (it "should also add the fully-qualified names to env"
              (should-contain 'clojure.core/+ (add-ns 'clojure.core {}))))

(describe "(name-filter regex)"
          (it "should return true for env entries that  match the patter"
              (should= '(x xx) (filter (name-filter #"x+") ['x 'xx 'xy 'yy]))))
(describe "(name-filter-out regex)"
          (it "should return true for env entries that  match the patter"
              (should= '(xy yy) (filter (name-filter-out #"x+") ['x 'xx 'xy 'yy]))))
