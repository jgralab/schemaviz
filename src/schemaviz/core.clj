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

(defn emit-attribute [a]
  (let [d (adj a :domain)]
    (str (value a :name) ": " (value d :qualifiedName)
         (when-let [dv (value a :defaultValue)]
           (str " := " dv))
         "\\l")))

;;## VertexClasses

(defn emit-vertex-class [vc]
  (let [id (make-id vc)]
    (str "  " id " [shape=record, label=\"{{"
         (value vc :qualifiedName) "}"
         (when (seq (iseq vc 'HasAttribute))
           (apply str "|" (map emit-attribute (adjs vc :attribute))))
         "}\""
         "];\n")))

(defn emit-vertex-classes [sg]
  (apply str (map emit-vertex-class (vseq sg 'VertexClass))))

;;## EdgeClasses

;; TODO: nen rautensymbol für ECs, so dass die auch generalisierungen können!
;; also pro EC 2 kanten und nen raute-knoten in der mitte, der auch die ID der
;; EC trägt.
(defn emit-edge-class [ec]
  (let [id (make-id ec)
        src (make-id (adj ec :from :targetclass))
        dst (make-id (adj ec :to :targetclass))]
    (str "  " src " -> " dst " ["
         "label=\"" (value ec :qualifiedName)
         (when (seq (iseq ec 'HasAttribute))
           (apply str "\\n" (map emit-attribute (adjs ec :attribute))))
         "\""
         "];\n")))

(defn emit-edge-classes [sg]
  (apply str (map emit-edge-class (vseq sg 'EdgeClass))))

;;## Main

(defn visualize-schema
  "Prints a visualization of Schema or SchemaGraph s to the file f."
  [s f]
  (let [sg (if (instance? Schema s)
             (schema-graph s)
             s)
        vcs (emit-vertex-classes sg)
        ecs (emit-edge-classes sg)
        generalizations nil]
    (spit f
          (str "digraph Extracted {\n"
               "  rankdir=BT;\n"
               "  ranksep=1.0;\n"
               vcs
               ecs
               generalizations
               "}\n"))))

#_(visualize-schema
 (load-schema "/home/horn/Repos/uni/jgralab/src/de/uni_koblenz/jgralab/schema/GrumlSchema.tg")
 "/home/horn/test.dot")

(visualize-schema
 (load-schema "/home/horn/cobol.tg")
 "/home/horn/test.dot")
