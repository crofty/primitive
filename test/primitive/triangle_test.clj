(ns primitive.triangle-test
  (:require [clojure.test :refer [deftest is testing]]
            [primitive.image :as image]
            [primitive.triangle :as triangle])
  (:import [java.util Random]
           [java.awt.image BufferedImage]))

(defn blank-image []
  (image/make-image 16 16 {:r 0 :g 0 :b 0}))

(deftest random-triangle-in-bounds
  (let [rng (Random. 1)
        tri (triangle/random-triangle rng 16 16)]
    (doseq [[x y] (:points tri)]
      (is (<= 0.0 x 16.0))
      (is (<= 0.0 y 16.0)))))

(deftest evaluate-produces-score
  (let [rng (Random. 2)
        target (image/make-image 16 16 {:r 255 :g 255 :b 255})
        state {:target target
               :current (image/make-image 16 16 {:r 0 :g 0 :b 0})
               :alpha 128}
        tri (triangle/random-triangle rng 16 16)
        result (triangle/evaluate state tri)]
    (is result)
    (is (number? (:score result)))))

