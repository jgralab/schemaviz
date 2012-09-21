(ns schemaviz.core
  (:use funnyqt.tg
        funnyqt.query
        funnyqt.query.tg
        funnyqt.protocols)
  (:require [clojure.string :as str])
  (:import
   (de.uni_koblenz.jgralab.schema Schema)))

;;# Code

(def make-id
  (memoize (fn [v] (gensym "v"))))

(def ^:dynamic *options*
  "Default options to use.  May be overridden to customize the layout by
  providing an own options map to `visualize-schema` whose entries are merged
  with the default opts.

  The meaning of the options are as follows:

    :rankdir                 See http://www.graphviz.org/content/attrs#drankdir
    :ranksep                 See http://www.graphviz.org/content/attrs#dranksep
    :fontname                See http://www.graphviz.org/content/attrs#dfontname
    :graphclass-fill-color   The fill color of the graphclass node
    :edgeclass-fill-color    The fill color of edgeclass nodes
    :edgeclass-weight        The weight of edgeclass edges.  If it is higher
                             than :specialization-weight, then edgeclass edges
                             are tried to be shorter and more straight than
                             specialization edges.
    :specialization-color    The color of specialization edges
    :specialization-weight   See :edgeclass-weight
    :enumdomain-fill-color   The fill color of enumdomain nodes
    :recorddomain-fill-color The fill color of recorddomains"
  {:rankdir "BT"
   :ranksep "3.0"
   :fontname "Helvetica"
   :graphclass-fill-color "green"
   :vertexclass-fill-color "lightblue"
   :edgeclass-fill-color "khaki"
   :edgeclass-weight 1
   :specialization-color "gray"
   :specialization-weight 2
   :enumdomain-fill-color "goldenrod"
   :recorddomain-fill-color "lightsalmon"})

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

(defn emit-inc-class [ic]
  (str "label=\""
       (let [min (Integer/valueOf (value ic :min))
             max (Integer/valueOf (value ic :max))]
         (cond
          (== min max) min
          (and (zero? min) (== Integer/MAX_VALUE max)) "*"
          :else (str "(" min "," (if (== Integer/MAX_VALUE max)
                                   "*"
                                   max)
                     ")")))
       (when-let [role (value ic :roleName)]
         (str "\\l" role))
       (when-let [redefs (seq (iseq ic 'Redefines :out))]
         (str "\\l{redefines " (str/join ", "
                                      (map #(value (omega %) :roleName)
                                           redefs))
              "}\\n"))
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
         "  " fvc " -> " id  " [dir=both, arrowhead=none, "
         "weight=" (:edgeclass-weight *options*)
         ", arrowtail="
         (cond
          (= tak (enum-constant ec 'structure.AggregationKind.COMPOSITE)) "diamond"
          (= tak (enum-constant ec 'structure.AggregationKind.SHARED))    "ediamond"
          :else "none") ", "
         (emit-inc-class fic)
         "];\n"
         "  " id  " -> " tvc " ["
         "weight=" (:edgeclass-weight *options*) ", "
         (emit-inc-class tic)  "];\n")))

(defn emit-edge-classes [sg]
  (apply str (map emit-edge-class (vseq sg 'EdgeClass))))

;;## Specializations

(defn emit-specialization [s]
  (let [f (make-id (alpha s))
        t (make-id (omega s))]
    (str "  " f " -> " t " [color=" (:specialization-color *options*)
         ", weight=" (:specialization-weight *options*)
         ", arrowhead=empty];\n")))

(defn emit-specializations [sg]
  (apply str (map emit-specialization
                  (eseq sg '[SpecializesVertexClass SpecializesEdgeClass]))))

;;## Custom domains

(defn emit-enum-domain [d]
  (str "    " (make-id d) " [fillcolor=" (:enumdomain-fill-color *options*)
         ", style=filled, shape=record, label=\"{{«enum»\\n"
         (value d :qualifiedName) "}"
         " | " (str/join "\\n" (value d :enumConstants))
         "}\""
         "];\n"))

(defn emit-record-domain [d]
  (str "    " (make-id d) " [fillcolor=" (:recorddomain-fill-color *options*)
       ", style=filled, shape=record, label=\"{{«record»\\n"
       (value d :qualifiedName) "}"
       " | " (str/join "\\n" (map #(str (value % :name) ": "
                                        (value (omega %) :qualifiedName))
                                  (iseq d 'HasRecordDomainComponent)))
       "}\""
       "];\n"))

(defn emit-custom-domains-cluster [sg]
  (when-let [ds (vseq sg '[EnumDomain RecordDomain])]
    (apply str (map (fn [d]
                           (if (has-type? d 'EnumDomain)
                             (emit-enum-domain d)
                             (emit-record-domain d)))
                         ds))))

;;## The graph class

(defn emit-graph-class [sg]
  (let [gc (the (vseq sg 'GraphClass))]
    (str "  " (make-id gc) " [fillcolor=" (:graphclass-fill-color *options*)
         ", style=filled, shape=record, label=\"{{«graphclass»\\n"
         (value gc :qualifiedName) "}"
         (when (seq (iseq gc 'HasAttribute))
           (apply str "|" (map (partial emit-attribute "\\l")
                               (adjs gc :attribute))))
         "}\""
         "];\n")))

;;## Main

(defn visualize-schema
  "Prints a dot visualization of Schema or SchemaGraph `s` to the file `f`.
  For a descriptions of `options`, see `*options*`."
  ([s f]
     (visualize-schema s f *options*))
  ([s f options]
     (binding [*options* (merge *options* options)]
       (let [sg (if (instance? Schema s)
                  (schema-graph s)
                  s)
             gc (emit-graph-class sg)
             vcs (emit-vertex-classes sg)
             ecs (emit-edge-classes sg)
             specializations (emit-specializations sg)
             doms (emit-custom-domains-cluster sg)]
         (spit f
               (str "digraph Extracted {\n"
                    "  rankdir=" (:rankdir *options*) ";\n"
                    "  ranksep=" (:ranksep *options*) ";\n"
                    "  node [fontname=" (:fontname *options*) "];\n"
                    "  edge [fontname=" (:fontname *options*) "];\n"
                    gc
                    vcs
                    ecs
                    specializations
                    "  subgraph clusterCustomDomains {\n"
                    doms
                    "}\n"
                    "}\n"))))))

#_(visualize-schema
 (load-schema "/home/horn/Repos/uni/jgralab/src/de/uni_koblenz/jgralab/schema/GrumlSchema.tg")
 "/home/horn/test.dot")

#_(visualize-schema
 (load-schema "/home/horn/Repos/uni/grabaja/java5.tg")
 "/home/horn/test.dot")

#_(visualize-schema
 (load-schema "/home/horn/cobol.tg")
 "/home/horn/test.dot")
