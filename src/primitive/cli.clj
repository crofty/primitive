(ns primitive.cli
  (:require [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [primitive.image :as image]
            [primitive.model :as model])
  (:import [java.util Random]
           [java.io File])
  (:gen-class))

(def cli-options
  [["-i" "--input PATH" "Input image path"]
   ["-o" "--output PATH" "Output image path" :multi true]
   ["-n" "--count N" "Number of shapes" :parse-fn #(Integer/parseInt %) :default 0]
   ["-m" "--mode MODE" "Shape mode (only 1=triangle supported)" :parse-fn #(Integer/parseInt %) :default 1]
   ["-a" "--alpha N" "Alpha value (0-255)" :parse-fn #(Integer/parseInt %) :default 128]
   ["-r" "--resize N" "Resize large input images to this size" :parse-fn #(Integer/parseInt %) :default 256]
   ["-s" "--size N" "Output image size" :parse-fn #(Integer/parseInt %) :default 0]
   [nil "--samples N" "Number of candidate shapes to evaluate per iteration" :parse-fn #(Integer/parseInt %) :default 20]
   [nil "--mutations N" "Number of mutations per candidate" :parse-fn #(Integer/parseInt %) :default 40]
   ["--bg" "--background HEX" "Background color in hex"]
   [nil "--seed N" "Random seed" :parse-fn #(Long/parseLong %)]
   ["-v" "--verbose"]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Primitive Pictures (Clojure Edition)"
        ""
        "Usage: primitive -i input.png -o output.png -n 100"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(defn error-msg [errors]
  (str "Errors:\n" (str/join \newline errors)))

(defn exit [status msg]
  (when msg
    (println msg))
  (System/exit status))

(defn ensure-output-dir! [path]
  (let [file (File. path)
        parent (.getParentFile file)]
    (when parent
      (.mkdirs parent))))

(defn run-model [state count verbose]
  (loop [i 0
         current state]
    (if (= i count)
      current
      (let [[next _] (model/step current)]
        (when verbose
          (println (format "Step %d/%d score=%.6f" (inc i) count (:score next))))
        (recur (inc i) next)))))

(defn sanitize-args [args]
  (->> args
       (remove #(= "--" %))
       vec))

(defn -main [& args]
  (let [args (sanitize-args args)
        {:keys [options summary errors]} (cli/parse-opts args cli-options)
        {:keys [input output count mode alpha resize size background samples mutations seed verbose help]} options]
    (cond
      help (exit 0 (usage summary))
      errors (exit 1 (error-msg errors)))
    (let [outputs (or output [])
          problems (cond-> []
                     (str/blank? input) (conj "Input path is required")
                     (empty? outputs) (conj "At least one output path is required")
                     (<= count 0) (conj "Count must be greater than zero")
                     (not= mode 1) (conj "Only triangle mode (1) is supported in the Clojure implementation")
                     (or (< alpha 0) (> alpha 255)) (conj "Alpha must be between 0 and 255"))]
      (when (seq problems)
        (exit 1 (error-msg problems)))
      (let [input-image (-> (image/load-image input)
                            (image/scale-longest resize))
            bg (if (str/blank? background)
                 (image/average-color input-image)
                 (image/parse-hex-color background))
            rng (if seed (Random. seed) (Random.))
            model (model/create input-image bg {:alpha alpha
                                               :samples samples
                                               :mutations mutations
                                               :rng rng
                                               :verbose verbose})
            result (run-model model count verbose)
            final-image (image/scale-output (:current result) size)]
        (doseq [path outputs]
          (let [formatted (if (str/includes? path "%")
                            (format path count)
                            path)]
            (ensure-output-dir! formatted)
            (image/save-image final-image formatted)))))))

