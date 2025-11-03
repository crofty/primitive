(ns primitive.model-test
  (:require [clojure.test :refer [deftest is testing]]
            [primitive.image :as image]
            [primitive.model :as model])
  (:import [java.util Random]))

(deftest run-reduces-error
  (let [target (image/make-image 32 32 {:r 255 :g 255 :b 255})
        rng (Random. 42)
        initial (model/create target {:r 0 :g 0 :b 0} {:alpha 200 :samples 10 :mutations 20 :rng rng})
        result (model/run initial 5)]
    (is (< (:score result) (:score initial)))))

