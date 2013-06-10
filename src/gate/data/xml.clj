(ns ^{:doc "A Clojure library for parsing XML files created by GATE - the General Architecture for Text Engineering"
      :author "Daniel R. Schlegel and Mike Prentice"}
  gate.data.xml
  (:require [clojure.xml :as xml]
            [clojure.string :as str]))

(defn raw-annotation-sets
  "Return the GATE annotation sets"
  [gateDocument]
  (filter #(= (:tag %) :AnnotationSet) (:content gateDocument)))

(defn raw-annotations
  "Return a map from annotation set name to content. The default annotation set (null in GATE) has the nil key."
  [annotationSets]
  (apply merge (map #(hash-map (:Name (:attrs %)) (:content %)) annotationSets)))

;;A gate XML feature contains 2 parts, a Name and a Value. This converts, for example:
;;  <Name className="java.lang.String">kind</Name>
;;  <Value className="java.lang.String">nn</Value>
;;To:
;;{:kind nn} - that is, a map with a single entry from :kind to nn. 
(defn feature
  "Destructure a GATE XML feature and return a single-entry map"
  [f]
  (let [{[{tag1 :tag [c1] :content}
          {tag2 :tag [c2] :content}] :content} f]
    (if (= tag1 :Name)
      {(keyword c1) c2}
      {(keyword c2) c1})))

(defn feature-map
  "Return the feature map of an annotation"
  [annot]
  (apply merge (map feature
                    (filter #(= (:tag %) :Feature) (:content annot)))))

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.
  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d]). From the no longer maintained clojure.contrib.seq"
  [s]
  (map vector (iterate inc 0) s))

(defn positions
  "Returns a lazy sequence containing the positions at which pred
   is true for items in coll. (From clojure.contrib.seq)"
  [pred coll]
  (for [[idx elt] (indexed coll) :when (pred elt)] idx))

;;Takes as input a starting and ending node index, and an XML document. 
;;It first filters the XML document so it only contains nodes with the tag "TextWithNodes"
;;TextWithNodes is the area where the text of the document is preserved.
;;It then finds the starting and ending nodes in the filtered document and creates a vector of them (relvec).
;;It then grabs all the strings from relvec to create the returned original text.
(defn get-text-in-range
  "Return the text in the given range from TextWithNodes"
  [start end doc]
  (let [nodes (:content (first (filter #(= (:tag %) :TextWithNodes)
                                       (:content doc))))
        seqstart (first (positions
                         #(and (map? %) (= (:id (:attrs %)) start)) nodes))
        seqend (first (positions
                       #(and (map? %) (= (:id (:attrs %)) end)) nodes))
        relnodes (subvec nodes seqstart seqend)]
    (apply str (filter string? (map #(if (and (map? (first %)) (map? (second %)))
                                       " "
                                       (second %))
                                    (partition 2 1 relnodes))))))

;;Takes as input an annotation (created from raw-annotations) and converts it into a 
;;map with keys :id, :type, :start, :end, :text, and :featureMap. (Mike calls this a struct, 
;;and while he's not *wrong*, the struct was never defined - so this is really just a map).
(defn raw->annot
  "Convert an XML annotation to a annot struct"
  [annotation gateDocument]
  (let [{:keys [Id Type StartNode EndNode]} (:attrs annotation)
        features (feature-map annotation)
        text (get-text-in-range StartNode EndNode gateDocument)]
    {:id Id, :type Type, :start (Integer/valueOf StartNode), :end (Integer/valueOf EndNode), :text text,
     :featureMap features}))

(defn annotations
  "Return the annotations of the gateDocument as a list of annotation structures"
  [gateDocument]
  (let [annots (raw-annotations (raw-annotation-sets gateDocument))]
    (loop [k (keys annots)
           annotmap annots]
      (if (not (empty? k))
        (recur (rest k)
               (assoc annotmap (first k) (map #(raw->annot % gateDocument) (annotmap (first k)))))
        annotmap))))

(defn parse
  [file]
  (annotations (xml/parse file)))


;;;;;;;;;;;;;;;;;;;;;
;;; Parse Matches ;;;
;;;;;;;;;;;;;;;;;;;;;

(defn matches-annots
  "Return annotations with matches"
  [annots]
  (filter #(not (nil? (:matches (:featureMap %)))) annots))

(defn matches-strs
  "Return the matches strings"
  [annots]
  (let [matches-annots (matches-annots annots)]
    (map #(:matches (:featureMap %)) matches-annots)))

(defn matches-sets
  [annots]
  (let [strfn (fn [x] (map #(str "n" %) x))
        matches-strs (matches-strs annots)]
    (set (map #(set (strfn (str/split % #";"))) matches-strs))))

(defn in-range?
  "Return true if token is in (part of) the annotation"
  [tok annot]
  (let [tok-start (:start tok)
        tok-end (:end tok)
        annot-start (:start annot)
        annot-end (:end annot)]
    (if (or (nil? tok-start) (nil? tok-end) (nil? annot-start) (nil? annot-end))
      (println tok annot))
    (and (>= tok-start annot-start) (<= tok-end annot-end))))

;;Takes an annot structure and a list of tokens 
;;(remember, tokens are just annotations of type Token). 
;;It filters the list of tokens
;;to return only the ones in the range of the annot.
(defn get-tokens-in-range
  "Return a list of tokens in range of annot"
  [toks annot]
  (filter #(in-range? % annot) toks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; New Merging Algorithm ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn merge-feature-maps-destructively
  [map1 map2]
  (let [maj-map1 (:majorType map1)
        maj-map2 (:majorType map2)
	trunc-map1 (if (and (not (nil? maj-map1))
                            (not (= maj-map1 "person_first"))
                            (not (= maj-map1 "person_full"))
                            (not (= maj-map1 "surname"))
                            (not (= maj-map1 "organization"))
			    (not (= maj-map1 "loc_key"))
			    (not (= maj-map1 "post"))
			    (not (= maj-map1 "stop")))
			map1
                        (dissoc map1 :majorType :minorType))
	trunc-map2 (if (and (not (nil? maj-map2))
                            (not (= maj-map2 "person_first"))
                            (not (= maj-map2 "person_full"))
                            (not (= maj-map2 "surname"))
                            (not (= maj-map2 "organization"))
			    (not (= maj-map1 "loc_key"))
			    (not (= maj-map1 "post"))
			    (not (= maj-map1 "stop")))
			map2
                        (dissoc map2 :majorType :minorType))]
    (merge trunc-map1 trunc-map2)))

(defn merge-entities
  "Merges entities which have the same range, so we end up with one consistent entity"
  [entity-annots merge-maps]
  (let [efn (fn [ent] {(keyword (str (:start ent) "-" (:end ent))) ent})]
    (apply merge-with merge-maps (map efn entity-annots))))

;;Takes as input a token (an annotation of type token that is), and makes a map of token location to token.
(defn annot-loc-map
  "Return a map of a token ID to the token"
  [annot]
  (let [lfn (fn [a] {(keyword (str (:start a) "-" (:end a))) a})]
    (lfn annot)))

(defn location-map
  "Return a map of token locations to tokens"
  [annots]
  (apply merge (map annot-loc-map annots)))

;; Input is a list of named entities. Early ones in the list get priority over later ones.
(defn make-locmap
  "Return the locmap for named entities"
  [toks named-entities id-updates matches]
  (let [merge-maps (fn [res lat]
	  (if (not (and (= (:start res) (:start lat)) (= (:end res) (:end lat)))) lat
	    (do
	      (let [newid (:id lat)
		    newfeat (merge-feature-maps-destructively (:featureMap res) (:featureMap lat))
		    newmap (-> lat (assoc :featureMap newfeat) (assoc :id newid))
		    newmatches (ref @matches)]
		(doseq [mlist (seq @matches)]
		  (let [ml (ref mlist)] 
		    (doseq [m (seq @ml)]
		      (if (= (str m) (str (sid res)))
			(dosync (ref-set ml (conj (disj @ml (str (sid res))) (str (sid lat)))))))
		    (if (not (= @ml mlist))
		      (dosync (ref-set newmatches (conj (disj @newmatches mlist) @ml))))))
		(dosync (ref-set matches @newmatches))
		(if (not (nil? @id-updates)) (dosync (ref-set id-updates (assoc @id-updates (keyword (sid res)) (sid lat)))))
		newmap))))
        locmaptoks (location-map toks)
        locmapents (merge-entities named-entities merge-maps)
        newmap (dosync (ref-set id-updates (hash-map)))
        mergedmap (merge-with merge-maps locmaptoks locmapents)]
    mergedmap))
