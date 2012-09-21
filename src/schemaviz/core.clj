(ns schemaviz.core
  (:use funnyqt.tg
        funnyqt.query
        funnyqt.query.tg)
  (:require [clojure.string :as str])
  (:import
   (de.uni_koblenz.jgralab.schema Schema)))

;;# Code

(def make-id
  (memoize (fn [v] (gensym "v"))))

(def ^:dynamic *options*
  {:rankdir "BT"
   :ranksep "3.0"
   :fontname "Helvetica"
   :vertexclass-fill-color "lightblue"
   :edgeclass-fill-color "khaki"
   :specialization-color "gray"})

;;## Attributes

(defn emit-attribute [sep a]
  (let [d (adj a :domain)]
    (str (value a :name) ": " (value d :qualifiedName)
         (when-let [dv (value a :defaultValue)]
           (str " := " dv))
         sep)))

;;## VertexClasses

(defn emit-vertex-class [vc]
  (let [id (make-id vc)]
    (str "  " id " [fillcolor=" (:vertexclass-fill-color *options*)
         ", style=filled, shape=record, label=\"{{"
         (value vc :qualifiedName) "}"
         (when (seq (iseq vc 'HasAttribute))
           (apply str "|" (map (partial emit-attribute "\\l")
                               (adjs vc :attribute))))
         "}\""
         "];\n")))

(defn emit-vertex-classes [sg]
  (apply str (map emit-vertex-class (vseq sg 'VertexClass))))

;;## EdgeClasses

(defn emit-inc-class [where ic]
  (str where "label=\""
       (when-let [role (value ic :roleName)]
         (str role "\\n"))
       (when-let [redefs (seq (iseq ic 'Redefines :out))]
         (str "{redefines " (str/join ", "
                                      (map #(value (omega %) :roleName)
                                           redefs))
              "}\\n"))
       (let [min (Integer/valueOf (value ic :min))
             max (Integer/valueOf (value ic :max))]
         (cond
          (== min max) min
          (and (zero? min) (== Integer/MAX_VALUE max)) "*"
          :else (str "(" min "," (if (== Integer/MAX_VALUE max)
                                   "*"
                                   max)
                     ")")))
       "\""))

(defn emit-edge-class-1 [id ec]
  (str "  " id " [fillcolor=" (:edgeclass-fill-color *options*)
       ", style=filled, shape=oval, label=\""
       (value ec :qualifiedName)
       (when (seq (iseq ec 'HasAttribute))
         (apply str "\\n" (map (partial emit-attribute "\\n")
                               (adjs ec :attribute))))
       "\""
       "];\n"))

(defn emit-edge-class [ec]
  (let [id (make-id ec)
        fic (adj ec :from)
        tic (adj ec :to)
        tak (value tic :aggregation)
        fvc (make-id (adj fic :targetclass))
        tvc (make-id (adj tic :targetclass))]
    (str (emit-edge-class-1 id ec)
         "  " fvc " -> " id  "[dir=both, arrowhead=none, arrowtail="
         (cond
          (= tak (enum-constant ec 'structure.AggregationKind.COMPOSITE)) "diamond"
          (= tak (enum-constant ec 'structure.AggregationKind.SHARED))    "ediamond"
          :else "none") ", "
         (emit-inc-class "tail" fic)
         "];\n"
         "  " id  " -> " tvc "[" (emit-inc-class "head" tic)  "];\n")))

(defn emit-edge-classes [sg]
  (apply str (map emit-edge-class (vseq sg 'EdgeClass))))

;;## Specializations

(defn emit-specialization [s]
  (let [f (make-id (alpha s))
        t (make-id (omega s))]
    (str "  " f " -> " t " [color=" (:specialization-color *options*)
         ", arrowhead=empty];\n")))

(defn emit-specializations [sg]
  (apply str (map emit-specialization
                  (eseq sg '[SpecializesVertexClass SpecializesEdgeClass]))))

;;## Main

(defn visualize-schema
  "Prints a visualization of Schema or SchemaGraph s to the file f."
  ([s f]
     (visualize-schema s f *options*))
  ([s f opts]
     (binding [*options* (merge *options* opts)]
       (let [sg (if (instance? Schema s)
                  (schema-graph s)
                  s)
             vcs (emit-vertex-classes sg)
             ecs (emit-edge-classes sg)
             specializations (emit-specializations sg)]
         (spit f
               (str "digraph Extracted {\n"
                    "  rankdir=" (:rankdir *options*) ";\n"
                    "  ranksep=" (:ranksep *options*) ";\n"
                    "  node [fontname=" (:fontname *options*) "];"
                    "  edge [fontname=" (:fontname *options*) "];"
                    vcs
                    ecs
                    specializations
                    "}\n"))))))

#_(visualize-schema
 (load-schema "/home/horn/Repos/uni/jgralab/src/de/uni_koblenz/jgralab/schema/GrumlSchema.tg")
 "/home/horn/test.dot")

(visualize-schema
 (load-schema "/home/horn/cobol.tg")
 "/home/horn/test.dot")
