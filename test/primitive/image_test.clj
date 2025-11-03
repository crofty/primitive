(ns primitive.image-test
  (:require [clojure.test :refer [deftest is testing]]
            [primitive.image :as image])
  (:import [java.awt Color]
           [java.awt.image BufferedImage]))

(defn fill-image [colors]
  (let [height (count colors)
        width (count (first colors))
        img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics img)]
    (doseq [y (range height)
            x (range width)]
      (let [{:keys [r g b]} (get-in colors [y x])]
        (.setColor g (Color. ^int r ^int g ^int b))
        (.fillRect g x y 1 1)))
    (.dispose g)
    img))

(deftest average-color-test
  (let [img (fill-image [[{:r 255 :g 0 :b 0} {:r 0 :g 0 :b 255}]
                         [{:r 0 :g 255 :b 0} {:r 255 :g 255 :b 255}]])
        avg (image/average-color img)]
    (is (= {:r 128 :g 128 :b 128} avg))))

(deftest mse-test
  (let [img (image/make-image 4 4 {:r 10 :g 20 :b 30})
        img2 (image/make-image 4 4 {:r 10 :g 20 :b 30})
        img3 (image/make-image 4 4 {:r 30 :g 40 :b 50})]
    (is (zero? (image/mse img img2)))
    (is (< 0 (image/mse img img3)))))

(deftest parse-hex-color-test
  (is (= {:r 255 :g 0 :b 255} (image/parse-hex-color "#ff00ff")))
  (is (= {:r 16 :g 32 :b 48} (image/parse-hex-color "0x102030"))))

(deftest scale-longest-test
  (let [img (image/make-image 400 200 {:r 0 :g 0 :b 0})
        scaled (image/scale-longest img 200)]
    (is (= 200 (.getWidth scaled)))
    (is (= 100 (.getHeight scaled)))))

