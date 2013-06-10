gate.data.xml
====

A Clojure library for parsing XML files created by GATE - the General Architecture for Text Engineering

Example Usage
----

```clojure
(use 'gate.data.xml)

(parse "/path/to/file.xml")
;;=> An map from AnnotationSet name to a list of annotation maps for easy use in Clojure programs.
```
