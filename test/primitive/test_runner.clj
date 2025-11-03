(ns primitive.test-runner
  (:require [clojure.test :as t]
            primitive.image-test
            primitive.model-test
            primitive.triangle-test)
  (:gen-class))

(defn -main [& _]
  (let [result (t/run-tests 'primitive.image-test
                            'primitive.model-test
                            'primitive.triangle-test)
        failures (+ (:fail result) (:error result))]
    (System/exit (if (zero? failures) 0 1))))

