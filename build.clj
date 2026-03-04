(ns build
  "Execute portions of this build using the `build` alias in
  deps.edn; or use `the bin/release` script.
  For help, see https://clojure.org/guides/tools_build

  In this build, we break the artifacts into a deps lib jar and a
  project jar. This way, we can include the two jars (the big \"lib\"
  jar first) in the Docker image as different layers. Doing so will improve
  build and upload time of the Docker image.
  "
  (:require [clojure.tools.build.api :as b]
            [clojure.java.io :as io])
  (:import [java.util Date]
           [java.nio.file Files Paths]
           [java.security MessageDigest]
           [java.net URI]))

(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/quandary.jar"))

(defn clean [_]
  (b/delete {:path "target"}))

(defn deps-md5 [_]
  (let [deps-path (Paths/get (URI. (str "file://" (System/getProperty "user.dir") "/deps.edn")))
        data (Files/readAllBytes deps-path)
        hash (.digest (MessageDigest/getInstance "MD5") data)
        checksum (.toString (BigInteger. 1 hash) 16)]
    checksum))

(defn get-sha [_]
  (b/git-process {:git-args "describe --match=NeVeRmAtCh --always --abbrev=40 --dirty"}))

(defn create-build-info-file [_]
  (b/write-file {:path    "./resources/release/build-info.edn"
                 :content {:build-time (Date.)
                           :git-sha    (get-sha {})}}))

(defn lib-jar [_]
  (let [checksum (deps-md5 {})
        deps-path (format "target/all-libs-%s.jar" checksum)]
    (if (.exists (io/file deps-path))
      (println "Using existing lib-jar:" deps-path)
      (do (doseq [f (.listFiles (b/resolve-path "./target"))
                  :when (re-matches #"all-libs-?[0-9a-f]*\.jar" (.getName f))
                  :let [path (.getCanonicalPath f)]]
            (println "Removing: " path)
            (b/delete {:path path}))
          (println "Creating lib-jar:" deps-path)
          (b/uber {:uber-file deps-path
                   ;; class-dir is required, but we're only using `b/uber` to build a
                   ;; jar with all deps. It is not a real uber jar.
                   :class-dir "target/does-not-exist"
                   :basis     basis
                   :exclude   #{#"^license/.*"}             ; Avoids some dep jar conflicts
                   })))))

(defn project-jar [_]
  (b/delete {:path "target/classes"})
  (b/copy-dir {:src-dirs   ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file  jar-file}))

(defn build [args]
  ;; Don't clean all of target b/c we want to reuse the `lib-jar` is possible
  (lib-jar args)
  (create-build-info-file args)
  (project-jar args))
