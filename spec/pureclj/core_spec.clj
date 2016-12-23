(ns pureclj.core-spec
  (:require [speclj.core :refer :all]
            [pureclj.core :refer :all]))

(describe "(transpile expr)"
  (it "should convert a number to a function that returns that number"
      (should= 2 ((transpile 2) {})))
  (it "should convert a string to a function that returns that string"
      (should= "foo" ((transpile "foo") {})))
  (it "should convert a symbol to a function that looks up that symbol in the given map"
      (should= 5 ((transpile 'x) {'x 5 'y 7})))
  (it "should convert a safe function call to a function that executes the first function"
      (should= 3 ((transpile `(~+ 1 2)) {})))
  (it "should fail on an unsafe function"
      (should-throw (transpile `(~swap! x inc)))))
