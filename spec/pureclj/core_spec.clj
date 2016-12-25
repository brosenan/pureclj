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
      (should= #{'x 'y} (symbols {'x 'y}))))
