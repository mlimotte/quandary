(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'io.github.marclimotte/quandary)
(def version "0.9.0")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def lib-jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (b/write-pom {:class-dir class-dir
                :lib       lib
                :version   version
                :basis     basis
                :src-dirs  ["src"]
                :scm       {:url "https://github.com/marclimotte/quandary"}})
  (b/copy-dir {:src-dirs   ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file  lib-jar-file}))

(defn deploy [opts]
  (jar opts)
  ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
   (merge {:installer :remote
           :artifact  lib-jar-file
           :pom-file  (b/pom-path {:lib lib :class-dir class-dir})}
          opts)))
