(ns primitive.triangle
  (:require [primitive.image :as image])
  (:import [java.awt.geom Path2D$Double]
           [java.awt RenderingHints]
           [java.awt.image BufferedImage]
           [java.util Random]))

(defn random-triangle [^Random rng width height]
  (letfn [(point [] [(* (.nextDouble rng) width)
                     (* (.nextDouble rng) height)])]
    {:points [(point) (point) (point)]}))

(defn clamp [v min-val max-val]
  (-> v (max min-val) (min max-val)))

(defn mutate [^Random rng {:keys [points] :as triangle} width height]
  (let [idx (.nextInt rng 3)
        [x y] (points idx)
        scale (double (max width height))
        dx (* (.nextGaussian rng) (* 0.1 scale))
        dy (* (.nextGaussian rng) (* 0.1 scale))
        nx (clamp (+ x dx) 0.0 (dec width))
        ny (clamp (+ y dy) 0.0 (dec height))
        new-points (assoc points idx [nx ny])]
    (assoc triangle :points new-points)))

(defn path [{:keys [points]}]
  (let [[[x1 y1] [x2 y2] [x3 y3]] points
        p (Path2D$Double.)]
    (.moveTo p x1 y1)
    (.lineTo p x2 y2)
    (.lineTo p x3 y3)
    (.closePath p)
    p))

(defn- bounds [^BufferedImage image ^Path2D$Double path]
  (let [rect (.getBounds2D path)
        max-x (dec (.getWidth image))
        max-y (dec (.getHeight image))]
    {:min-x (max 0 (int (Math/floor (.getMinX rect))))
     :max-x (min max-x (int (Math/ceil (.getMaxX rect))))
     :min-y (max 0 (int (Math/floor (.getMinY rect))))
     :max-y (min max-y (int (Math/ceil (.getMaxY rect))))}))

(defn- pixel-stats [^BufferedImage target ^Path2D$Double path]
  (let [{:keys [min-x max-x min-y max-y]} (bounds target path)]
    (loop [x min-x
           sum-r 0.0
           sum-g 0.0
           sum-b 0.0
           count 0]
      (if (> x max-x)
        {:count count :sum-r sum-r :sum-g sum-g :sum-b sum-b}
        (let [result (loop [y min-y
                             r sum-r
                             g sum-g
                             b sum-b
                             c count]
                        (if (> y max-y)
                          [r g b c]
                          (if (.contains path (+ x 0.5) (+ y 0.5))
                            (let [rgb (.getRGB target x y)
                                  tr (bit-and (bit-shift-right rgb 16) 0xff)
                                  tg (bit-and (bit-shift-right rgb 8) 0xff)
                                  tb (bit-and rgb 0xff)]
                              (recur (inc y)
                                     (+ r tr)
                                     (+ g tg)
                                     (+ b tb)
                                     (inc c)))
                            (recur (inc y) r g b c))))]
          (recur (inc x) (result 0) (result 1) (result 2) (result 3)))))))

(defn stats->color [{:keys [count sum-r sum-g sum-b]}]
  (when (pos? count)
    (let [factor (/ 1.0 count)]
      {:r (int (Math/round (* sum-r factor)))
       :g (int (Math/round (* sum-g factor)))
       :b (int (Math/round (* sum-b factor)))})))

(defn draw! [^BufferedImage image ^Path2D$Double path color alpha]
  (let [g (.createGraphics image)]
    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (image/composite g alpha)
    (.setColor g (image/map->color color))
    (.fill g path)
    (.dispose g)
    image))

(defn evaluate [{:keys [target current alpha]} triangle]
  (let [p (path triangle)
        stats (pixel-stats target p)
        color (stats->color stats)]
    (when (and color (pos? (:count stats)))
      (let [candidate (image/copy-image current)]
        (draw! candidate p color alpha)
        {:shape (assoc triangle :color color)
         :image candidate
         :score (image/mse target candidate)})))

