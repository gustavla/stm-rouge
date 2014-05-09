(require 'leiningen.core.eval)

(def lwjgl-native-classifier 
  (let [classifiers {:macosx "natives-osx"
                     :linux "natives-linux"
                     :windows "natives-windows"}
        os (leiningen.core.eval/get-os)]
    (get classifiers os)))

(defproject stm-rogue "0.1.0-SNAPSHOT"
  :description "OpenGL"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.lwjgl.lwjgl/lwjgl "2.9.1"]
                 [org.lwjgl.lwjgl/lwjgl_util "2.9.1"]
                 [org.lwjgl.lwjgl/lwjgl-platform "2.9.1"
                  :classifier ~lwjgl-native-classifier :native-prefix ""]
                 [slick-util "1.0.0"]]
  :java-source-paths ["src/java"]
  :main stm-rogue.core)

