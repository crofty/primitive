(ns primitive.image
  (:require [clojure.string :as str])
  (:import [java.awt Color Graphics2D RenderingHints AlphaComposite]
           [java.awt.image BufferedImage]
           [java.awt.geom AffineTransform]
           [javax.imageio ImageIO]
           [java.io File]))

(defn load-image [path]
  (let [file (File. path)
        image (ImageIO/read file)]
    (when (nil? image)
      (throw (ex-info (str "Unable to read image: " path) {:path path})))
    image))

(defn- draw-scaled [^BufferedImage source width height]
  (let [scaled (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics scaled)]
    (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BILINEAR)
    (.drawImage g source 0 0 width height nil)
    (.dispose g)
    scaled))

(defn resize-max [^BufferedImage image max-size]
  (let [w (.getWidth image)
        h (.getHeight image)]
    (if (or (<= max-size 0)
            (and (<= w max-size) (<= h max-size)))
      image
      (let [ratio (double (/ max-size (double (max w h))))
            width (max 1 (int (Math/round (* w ratio))))
            height (max 1 (int (Math/round (* h ratio))))]
        (draw-scaled image width height)))))

(defn resize-to [^BufferedImage image size]
  (let [w (.getWidth image)
        h (.getHeight image)]
    (if (or (<= size 0)
            (and (= w size) (= h size)))
      image
      (draw-scaled image size size))))

(defn average-color [^BufferedImage image]
  (let [w (.getWidth image)
        h (.getHeight image)
        pixel-count (* w h)]
    (loop [x 0
           sum-r 0.0
           sum-g 0.0
           sum-b 0.0]
      (if (= x w)
        (let [factor (/ 1.0 pixel-count)]
          {:r (int (Math/round (* sum-r factor)))
           :g (int (Math/round (* sum-g factor)))
           :b (int (Math/round (* sum-b factor)))})
        (let [col (loop [y 0
                         r sum-r
                         g sum-g
                         b sum-b]
                    (if (= y h)
                      [r g b]
                      (let [rgb (.getRGB image x y)
                            r1 (bit-and (bit-shift-right rgb 16) 0xff)
                            g1 (bit-and (bit-shift-right rgb 8) 0xff)
                            b1 (bit-and rgb 0xff)]
                        (recur (inc y)
                               (+ r r1)
                               (+ g g1)
                               (+ b b1)))))];; note: r g b reused
          (recur (inc x) (col 0) (col 1) (col 2)))))))

(defn make-image [width height {:keys [r g b]}]
  (let [img (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        g2 (.createGraphics img)]
    (.setColor g2 (Color. ^int r ^int g ^int b))
    (.fillRect g2 0 0 width height)
    (.dispose g2)
    img))

(defn copy-image [^BufferedImage image]
  (let [w (.getWidth image)
        h (.getHeight image)
        copy (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics copy)]
    (.drawImage g image 0 0 nil)
    (.dispose g)
    copy))

(defn mse [^BufferedImage a ^BufferedImage b]
  (let [w (.getWidth a)
        h (.getHeight a)
        total (* w h 3)]
    (loop [x 0
           acc 0.0]
      (if (= x w)
        (/ acc total)
        (let [sum (loop [y 0
                          s acc]
                    (if (= y h)
                      s
                      (let [rgb-a (.getRGB a x y)
                            rgb-b (.getRGB b x y)
                            ra (bit-and (bit-shift-right rgb-a 16) 0xff)
                            ga (bit-and (bit-shift-right rgb-a 8) 0xff)
                            ba (bit-and rgb-a 0xff)
                            rb (bit-and (bit-shift-right rgb-b 16) 0xff)
                            gb (bit-and (bit-shift-right rgb-b 8) 0xff)
                            bb (bit-and rgb-b 0xff)
                            dr (- ra rb)
                            dg (- ga gb)
                            db (- ba bb)]
                        (recur (inc y)
                               (+ s (* dr dr) (* dg dg) (* db db))))))]
          (recur (inc x) sum))))))

(defn map->color [{:keys [r g b]}]
  (Color. ^int r ^int g ^int b))

(defn parse-hex-color [s]
  (let [clean (-> s (str/replace "#" "") (str/replace "0x" ""))
        value (Integer/parseUnsignedInt clean 16)
        r (bit-and (bit-shift-right value 16) 0xff)
        g (bit-and (bit-shift-right value 8) 0xff)
        b (bit-and value 0xff)]
    {:r r :g g :b b}))

(defn save-image [^BufferedImage image path]
  (let [lower (str/lower-case path)
        idx (.lastIndexOf lower ".")]
    (when (neg? idx)
      (throw (ex-info "Output file must have an extension" {:path path})))
    (let [ext (.substring lower (inc idx))
          file (File. path)]
      (case ext
        ("png") (ImageIO/write image "png" file)
        ("jpg" "jpeg") (let [w (.getWidth image)
                               h (.getHeight image)
                               rgb (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
                               g (.createGraphics rgb)]
                           (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BILINEAR)
                           (.drawImage g image 0 0 nil)
                           (.dispose g)
                           (ImageIO/write rgb "jpg" file))
        (throw (ex-info (str "Unsupported output format: " ext) {:extension ext :path path}))))))

(defn scale-longest [^BufferedImage image size]
  (if (<= size 0)
    image
    (resize-max image size)))

(defn scale-output [^BufferedImage image size]
  (if (<= size 0)
    image
    (resize-to image size)))

(defn with-alpha [^Color color alpha]
  (Color. (.getRed color) (.getGreen color) (.getBlue color) alpha))

(defn composite [^Graphics2D g alpha]
  (.setComposite g (AlphaComposite/getInstance AlphaComposite/SRC_OVER (float (/ alpha 255.0)))))