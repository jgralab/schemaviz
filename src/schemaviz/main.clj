(ns schemaviz.main
  (:require [clojure.string  :as str]
            [clojure.java.io :as io])
  (:use funnyqt.tg
        [schemaviz.core :only [*options* visualize-schema]]))

(defn handle-options [args]
  (apply hash-map (mapcat (fn [[o v]]
                            [(keyword (str/replace o "--" "")) v])
                          (partition 2 args))))

(defn print-usage []
  (binding [*out* *err*]
    (println "Usage:")
    (println "======")
    (println)
    (println "lein run [options] schemafile.tg outfile")
    (println)
    (println "Valid options are:")
    (doseq [[k [v d]] *options*]
      (println (str "--" (name k) ":") d)
      (println "  Default value:" v))))

(defn validate-options [opts]
  (loop [opts opts]
    (if (seq opts)
      (let [[k v] (first opts)]
        (if (*options* k)
          (recur (rest opts))
          (binding [*out* *err*]
            (println "Unknown option:" (str "--" (name k)))
            (print-usage)
            false)))
      true)))

(defn -main [& args]
  (let [[ofile sfile] (take 2 (reverse args))
        opts (handle-options (butlast (butlast args)))]
    (cond
     ;;;;
     (not (and ofile sfile))
     (print-usage)
     ;;;;
     (not (.exists (io/file sfile)))
     (binding [*out* *err*]
        (println "Schema file" sfile "doesn't exist!"))
     ;;;;
     :else (when (validate-options opts)
             (println "Visualizing" sfile "to" ofile "using options:" opts)
             (visualize-schema (load-schema sfile) ofile opts)))))
