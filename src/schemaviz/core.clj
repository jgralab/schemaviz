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
    (str "  " id " [fillcolor=lightblue, style=filled, shape=record, label=\"{{"
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
  (str "  " id " [fillcolor=khaki, style=filled, shape=oval, label=\""
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
    (str "  " f " -> " t " [color=gray, arrowhead=empty];\n")))

(defn emit-specializations [sg]
  (apply str (map emit-specialization
                  (eseq sg '[SpecializesVertexClass SpecializesEdgeClass]))))

;;## Main

(defn visualize-schema
  "Prints a visualization of Schema or SchemaGraph s to the file f."
  [s f]
  (let [sg (if (instance? Schema s)
             (schema-graph s)
             s)
        vcs (emit-vertex-classes sg)
        ecs (emit-edge-classes sg)
        specializations (emit-specializations sg)]
    (spit f
          (str "digraph Extracted {\n"
               "  rankdir=BT;\n"
               "  ranksep=3.0;\n"
               "  node [fontname=\"Helvetica\"];"
               "  edge [fontname=\"Helvetica\"];"
               vcs
               ecs
               specializations
               "}\n"))))

#_(visualize-schema
 (load-schema "/home/horn/Repos/uni/jgralab/src/de/uni_koblenz/jgralab/schema/GrumlSchema.tg")
 "/home/horn/test.dot")

(visualize-schema
 (load-schema "/home/horn/cobol.tg")
 "/home/horn/test.dot")
