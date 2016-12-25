(ns pureclj.core-spec
  (:require [speclj.core :refer :all]
            [pureclj.core :refer :all]))

(describe "(symbols expr)"
  (it "should return an empty set for a constant"
      (should= #{} (symbols 2)))
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
      (should= #{'a} (symbols '(let [x a] x))))
  (it "should remove symbols from an unnamed fn* arg list"
      (should= #{'a '+} (symbols '(fn [x y] (+ x y a)))))
  (it "should remove symbols from a named fn*"
      (should= #{'a '+} (symbols '(fn foo [x y] (+ x y a)))))
  (it "should support multi-clause functions"
      (should= #{'a 'b '+} (symbols '(fn ([x] (+ a x))
                                    ([x y] (+ b x y)))))))

