(ns schemaviz.core
  (:use funnyqt.tg
        funnyqt.query
        funnyqt.query.tg
        funnyqt.protocols)
  (:require clojure.java.shell
            [clojure.string :as str])
  (:import
   (de.uni_koblenz.jgralab.schema Schema)))

;;# Code

(def make-id
  (memoize (fn [v] (gensym "v"))))

(def ^:dynamic *options*
  "Default options to use.  May be overridden to customize the layout by
  providing an own options map to `visualize-schema` whose entries are merged
  with the default opts."
  {:rankdir ["BT" "See http://www.graphviz.org/content/attrs#drankdir"]
   :ranksep ["3.0" "See http://www.graphviz.org/content/attrs#dranksep"]
   :fontname ["Helvetica" "See http://www.graphviz.org/content/attrs#dfontname"]
   :graphclass-fill-color ["green" "The fill color of the graphclass node"]
   :vertexclass-fill-color ["lightblue" "The fill color of vertexclass nodes"]
   :edgeclass-fill-color ["khaki" "The fill color of edgeclass nodes"]
   :edgeclass-weight [1 "The weight of edgeclass edges.  If it is
  higher than :specialization-weight, then edgeclass edges are tried to be
  shorter and more straight than specialization edges."]
   :specialization-color ["gray" "The color of specialization edges"]
   :specialization-weight [2 "The weight of specialization
  edges. See :edgeclass-weight."]
   :enumdomain-fill-color ["goldenrod" "The fill color of enumdomain nodes"]
   :recorddomain-fill-color ["lightsalmon" "The fill color of recorddomain nodes"]})

(defn get-opt [opt]
  (first (opt *options*)))

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
    (str "  " id " [fillcolor=" (get-opt :vertexclass-fill-color)
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
  (str "  " id " [fillcolor=" (get-opt :edgeclass-fill-color)
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
         "weight=" (get-opt :edgeclass-weight)
         ", arrowtail="
         (cond
          (= tak (enum-constant ec 'structure.AggregationKind.COMPOSITE)) "diamond"
          (= tak (enum-constant ec 'structure.AggregationKind.SHARED))    "ediamond"
          :else "none") ", "
         (emit-inc-class fic)
         "];\n"
         "  " id  " -> " tvc " ["
         "weight=" (get-opt :edgeclass-weight) ", "
         (emit-inc-class tic)  "];\n")))

(defn emit-edge-classes [sg]
  (apply str (map emit-edge-class (vseq sg 'EdgeClass))))

;;## Specializations

(defn emit-specialization [s]
  (let [f (make-id (alpha s))
        t (make-id (omega s))]
    (str "  " f " -> " t " [color=" (get-opt :specialization-color)
         ", weight=" (get-opt :specialization-weight)
         ", arrowhead=empty];\n")))

(defn emit-specializations [sg]
  (apply str (map emit-specialization
                  (eseq sg '[SpecializesVertexClass SpecializesEdgeClass]))))

;;## Custom domains

(defn emit-enum-domain [d]
  (str "    " (make-id d) " [fillcolor=" (get-opt :enumdomain-fill-color)
         ", style=filled, shape=record, label=\"{{«enum»\\n"
         (value d :qualifiedName) "}"
         " | " (str/join "\\n" (value d :enumConstants))
         "}\""
         "];\n"))

(defn emit-record-domain [d]
  (str "    " (make-id d) " [fillcolor=" (get-opt :recorddomain-fill-color)
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
    (str "  " (make-id gc) " [fillcolor=" (get-opt :graphclass-fill-color)
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
             doms (emit-custom-domains-cluster sg)
             ds (str "digraph Extracted {\n"
                     "  rankdir=" (get-opt :rankdir) ";\n"
                     "  ranksep=" (get-opt :ranksep) ";\n"
                     "  node [fontname=" (get-opt :fontname) "];\n"
                     "  edge [fontname=" (get-opt :fontname) "];\n"
                     gc
                     vcs
                     ecs
                     specializations
                     "  subgraph clusterCustomDomains {\n"
                     doms
                     "}\n"
                     "}\n")
             suffix (second (re-matches #".*\.([^.]+)$" f))
             lang (get #{"dot" "xdot" "ps" "svg" "svgz" "png" "gif" "pdf" "eps" "gtk"}
                       suffix "pdf")
             r (clojure.java.shell/sh "dot" (str "-T" lang) "-o" f :in ds)]
         (when-not (zero? (:exit r))
           (throw (RuntimeException. (format "Dotting failed: %s" (:err r)))))))))

#_(visualize-schema
 (load-schema "/home/horn/Repos/uni/jgralab/src/de/uni_koblenz/jgralab/schema/GrumlSchema.tg")
 "/home/horn/test.gtk")

#_(visualize-schema
 (load-schema "/home/horn/Repos/uni/grabaja/java5.tg")
 "/home/horn/test.dot")

#_(visualize-schema
 (load-schema "/home/horn/cobol.tg")
 "/home/horn/test.dot")
