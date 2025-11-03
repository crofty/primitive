(ns primitive.model
  (:require [primitive.image :as image]
            [primitive.triangle :as triangle])
  (:import [java.util Random]
           [java.awt.image BufferedImage]))

(def default-options
  {:alpha 128
   :samples 20
   :mutations 40})

(defn create
  ([^BufferedImage target background]
   (create target background {}))
  ([^BufferedImage target background {:keys [alpha samples mutations rng verbose]
                                      :or {alpha (:alpha default-options)
                                           samples (:samples default-options)
                                           mutations (:mutations default-options)}}]
   (let [width (.getWidth target)
         height (.getHeight target)
         base (or background (image/average-color target))
         current (image/make-image width height base)
         seed (or rng (Random.))]
     {:target target
      :current current
      :width width
      :height height
      :alpha alpha
      :samples samples
      :mutations mutations
      :rng seed
      :score (image/mse target current)
      :verbose verbose
      :steps 0
      :evaluations 0})))

(defn- hill-climb [{:keys [mutations rng width height] :as state} initial]
  (loop [best initial
         evals 1
         step 0]
    (if (>= step mutations)
      {:result best :evaluations evals}
      (let [mutated (triangle/mutate rng (:shape best) width height)
            evaluation (triangle/evaluate state mutated)
            evals (inc evals)
            improved (and evaluation (< (:score evaluation) (:score best)))
            best (cond
                   improved evaluation
                   :else best)]
        (recur best evals (inc step))))))

(defn- search-shape [state]
  (let [{:keys [rng width height]} state
        triangle (triangle/random-triangle rng width height)]
    (when-let [initial (triangle/evaluate state triangle)]
      (hill-climb state initial))))

(defn step [{:keys [samples] :as state}]
  (loop [i 0
         best nil
         best-score Double/POSITIVE_INFINITY
         evaluations 0]
    (if (= i samples)
      (if best
        [(-> state
             (assoc :current (:image best)
                    :score (:score best))
             (update :steps inc)
             (update :evaluations + evaluations))
         evaluations]
        [(-> state (update :steps inc) (update :evaluations + evaluations)) evaluations])
      (let [result (search-shape state)
            evaluations (if result (+ evaluations (:evaluations result)) evaluations)]
        (if (and result (< (:score (:result result)) best-score))
          (recur (inc i) (:result result) (:score (:result result)) evaluations)
          (recur (inc i) best best-score evaluations))))))

(defn run [state count]
  (loop [remaining count
         s state]
    (if (zero? remaining)
      s
      (let [[next _] (step s)]
        (recur (dec remaining) next)))))

