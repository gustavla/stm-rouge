(ns stm-rogue.core
  (:require clojure.stacktrace)
  (:import (org.lwjgl.opengl Display 
                             DisplayMode 
                             PixelFormat
                             ContextAttribs
                             GL11
                             GL13
                             GL15
                             GL20
                             GL30)
           (org.lwjgl.util.vector Matrix4f
                                  Matrix)
           (org.lwjgl.input Keyboard
                            Mouse)
           (org.lwjgl Sys
                      BufferUtils
                      LWJGLException)
           (org.newdawn.slick.opengl Texture 
                                     TextureLoader)
           (org.newdawn.slick.util ResourceLoader)
           (java.io IOException)
           (java.lang.reflect Array)
           (java.util Random)
           (java.nio FloatBuffer
                     IntBuffer
                     Buffer))

  (:gen-class))

;(set! *warn-on-reflection* true)

(def directions {:left [-1 0]
                 :right [1 0]
                 :up [0 1]
                 :down [0 -1]})

(defn get-time []
  (/ (System/nanoTime) 1000000.0))

(def alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ.,!?0123456789-+()@#$%[ ")

(def WIDTH 800)
(def HEIGHT 600)

(def gl-commands (atom '()))
(def gl-reload-resources (atom false))

(defmacro gl [& args]
  "Executes command in GL thread."
  `(swap! gl-commands conj (quote ~args)))

(defn gl-reload []
  (reset! gl-reload-resources true))
  
(defn create-vector [size f]
  "Creates a 2d vector calling (f x y) at every element"
  (apply vector (map f (range size))))

(defn create-vector-2d [width height f]
  (create-vector width (fn [x] (create-vector height #(f x %)))))

(def dim0 40)
(def dim1 30)
(def layers 2)

(def running true)

(def data (float-array (* dim0 dim1 layers 40)))
(def data-indices (int-array (* dim0 dim1 layers 6))) 

(defstruct tile-struct :type :walkable :entity-id)
(defstruct entity-struct :entity-id :loc :type :dir :busy)

(def world
  (create-vector-2d dim0 dim1 (fn [_ _] 
                                (ref (struct tile-struct 1 true -1)))))

(defn location [loc]
  (let [[x y] loc]
    [(mod x dim0) (mod y dim1)]))

(defn place [[x y]] 
  (-> world (nth x) (nth y)))

(defn create-entity [loc entity-id entity-type dir]
  (dosync
    (let [p (place loc)
          entity (struct entity-struct entity-id loc entity-type dir false)]
      (alter p assoc :entity-id entity-id)
      (agent entity))))


;(def hero (create-entity [10 10] 5 0))
;(def enemy (create-entity [13 13] 3 0))

(defn tile-vacant? [tile]
  (and (:walkable tile) (= (:entity-id tile) -1)))


(defn create-enemies [n seed]
  (let [r (Random. seed)]
    ; Create a hash-map with integers as the IDs
    (into {}
      (for [x (range n)
            :let [i (.nextInt r dim0)
                  j (.nextInt r dim1)
                  p (place [i j])
                  entity-type (+ 50 (.nextInt r 10))]
            :when (tile-vacant? @p)]
        (dosync 
          [x (create-entity [i j] x entity-type 0)])))))

(def enemies (create-enemies 200 0))


;(def entities (vec I#

(defn place-entity [loc new-loc entity]
  (println "place-entity" loc new-loc)
  (dosync 
    (let [p (place new-loc)]
      (alter p assoc :entity-id entity)
      new-loc)))

(defn entity-move-unsynced [entity loc new-loc-unwrapped abs-cur]
  (let [p (place loc)
        new-loc (location new-loc-unwrapped)
        new-p (place new-loc)
        entity-id (:entity-id entity)
        new-entity (assoc entity :loc new-loc :busy (+ abs-cur 150))]
    (if (and entity (tile-vacant? @new-p)) 
      (do 
        ;(println "loc" loc "new-loc" new-loc)
        ;(println "setting" p "to -1 and" new-p "to" entity-id)
        (alter p assoc :entity-id -1)
        (alter new-p assoc :entity-id entity-id)
        new-entity)
      entity)))

(defn reset-world [seed]
  (let [r (Random. seed)]
    (dosync 
      (dotimes [i dim0]
        (dotimes [j dim1]
          (ref-set (place [i j]) (struct tile-struct (inc (.nextInt r 2)) true -1))))

      ;(send hero place-entity [10 10] (struct entity-struct 1 0 false))
      ;(send enemy place-entity [5 5] (struct entity-struct 2 0 false))
      )))


(defn entity-rel-move [entity dir abs-cur]
  (dosync
    (let [loc (:loc entity)
          delta (directions dir)
          new-loc (map + loc delta)]
      (entity-move-unsynced entity loc new-loc abs-cur))))

(defn behave
  "NCP behavior"
  [entity]
  (let []
    (. Thread (sleep 100))
    (dosync
      (when running
        (send-off *agent* #'behave))

      (let [dir-index (rand-int 4)
            dir (nth (keys directions) dir-index)]
        (send *agent* entity-rel-move dir (get-time)))))
  entity)

(defn start-enemies [enemies]
  (doall (for [[k, enemy] (seq enemies)]
           (send-off enemy #'behave))))


(defn die-if-opengl-error [place]
  (let [error-value (GL11/glGetError)]
    (if-not (= error-value GL11/GL_NO_ERROR)
      (throw (new Exception (str place ": GL Error " error-value))))))


(defn init-opengl []
  (GL11/glClearColor 0.8 0.8 1.0 1.0)
  (GL11/glViewport 0 0 (Display/getWidth) (Display/getHeight))
  (GL11/glDisable GL11/GL_CULL_FACE)
  (GL11/glDisable GL11/GL_DEPTH_TEST)
  (die-if-opengl-error "init"))

(defn destroy-opengl [res]
  (GL20/glUseProgram 0)
  (GL20/glDeleteProgram (-> res :shaders :p-id))
  
  ; Select the VAO
  (GL30/glBindVertexArray (-> res :quads :vao-id))

  ; Disable the VBO index from the VAO attributes list
  (GL20/glDisableVertexAttribArray 0)
  (GL20/glDisableVertexAttribArray 1)
  (GL20/glDisableVertexAttribArray 2)

  ; Delete the vertex VBO
  (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER 0)
  (GL15/glDeleteBuffers (int (-> res :quads :vbo-id)))
  
  ; Delete the index VBO
  (GL15/glBindBuffer GL15/GL_ELEMENT_ARRAY_BUFFER 0)
  (GL15/glDeleteBuffers (int (-> res :quads :vboi-id)))
  
  ; Delete the VAO
  (GL30/glBindVertexArray 0)
  (GL30/glDeleteVertexArrays (int (-> res :quads :vao-id))))

(defn set-texture [^long x ^long y ^long layer ^long t]
  (let [offset (long (* 40 (* (+ (* (* layer dim0) dim1) (+ (* x dim1) y)))))
        tx (float (mod t 10))
        ty (float (quot t 10))
        tx1 (+ (float 1.0) tx)
        ty1 (+ (float 1.0) ty)
        frac (/ (float 10.0) (float 128.0))
        txf (* tx frac)
        tyf (* ty frac)
        tx1f (* tx1 frac)
        ty1f (* ty1 frac) ]

    (. Array (setFloat data (+ offset  (long 8)) txf))
    (. Array (setFloat data (+ offset  (long 9)) tyf))

    (. Array (setFloat data (+ offset (long 18)) txf))
    (. Array (setFloat data (+ offset (long 19)) ty1f))

    (. Array (setFloat data (+ offset (long 28)) tx1f))
    (. Array (setFloat data (+ offset (long 29)) ty1f))

    (. Array (setFloat data (+ offset (long 38)) tx1f))
    (. Array (setFloat data (+ offset (long 39)) tyf))))

    ;(aset-float data (+ offset  (long 8)) txf) 
    ;(aset-float data (+ offset  (long 9)) tyf)

    ;(aset-float data (+ offset (long 18)) txf) 
    ;(aset-float data (+ offset (long 19)) ty1f) 

    ;(aset-float data (+ offset (long 28)) tx1f) 
    ;(aset-float data (+ offset (long 29)) ty1f)

    ;(aset-float data (+ offset (long 38)) tx1f) 
    ;(aset-float data (+ offset (long 39)) tyf))
  
(defn setup-quads []
  (let [vertices-buffer (BufferUtils/createFloatBuffer (alength ^floats data))
        indices-buffer (BufferUtils/createIntBuffer (alength ^ints data-indices))
        s 20.0]
    (doseq [layer (range layers)
            i     (range dim0)
            j     (range dim1)]
      (let [sh (+ (* layer dim0 dim1) (+ j (* i dim1)))
            offset        (* 40 sh) 
            offset2       (*  6 sh) 
            index-offset  (*  4 sh)]
        (doseq [v (range 4)]
          (let [xorv (bit-xor (bit-and 1 v) (bit-shift-right (bit-and 2 v) 1))
                v-offset (+ offset (* v 10))]
            ; Position
            ;(aset-float data (+ v-offset 0) (* s (+ i (bit-shift-right v 1))))
            ;(aset-float data (+ v-offset 1) (* s (+ j (- 1 xorv))))
            (aset-float data (+ v-offset 0) (* s (+ i (quot v 2))))
            (aset-float data (+ v-offset 1) (* s (+ j (- 1 xorv))))
            (aset-float data (+ v-offset 2) 0.0)
            (aset-float data (+ v-offset 3) 1.0)
            ; Color
            (aset-float data (+ v-offset 4) 1.0)
            (aset-float data (+ v-offset 5) 1.0)
            (aset-float data (+ v-offset 6) 1.0)
            (aset-float data (+ v-offset 7) 1.0)
            ; Texture
            (aset-float data (+ v-offset 8) (* (quot v 2) (/ 10.0 128.0)))
            (aset-float data (+ v-offset 9) (* xorv (/ 10.0 128.0)))))

        (aset-int data-indices (+ offset2 0) (+ index-offset 0))
        (aset-int data-indices (+ offset2 1) (+ index-offset 1))
        (aset-int data-indices (+ offset2 2) (+ index-offset 2))
        (aset-int data-indices (+ offset2 3) (+ index-offset 2))
        (aset-int data-indices (+ offset2 4) (+ index-offset 3))
        (aset-int data-indices (+ offset2 5) (+ index-offset 0))))

    (die-if-opengl-error "setup quad1")

    ; Create vertices buffer
    (.rewind ^FloatBuffer vertices-buffer)
    (.put ^FloatBuffer vertices-buffer ^floats data)
    (.flip ^FloatBuffer vertices-buffer)
    
    ; Create indices buffer
    (.put ^IntBuffer indices-buffer ^ints data-indices)
    (.flip ^IntBuffer indices-buffer)

    (let [vao-id (GL30/glGenVertexArrays)
          vbo-id (GL15/glGenBuffers)
          vboi-id (GL15/glGenBuffers)]
        (GL30/glBindVertexArray vao-id)
        (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo-id)
        ; TODO: Stream or static? Probably static since I'm not changing them.
        (GL15/glBufferData GL15/GL_ARRAY_BUFFER vertices-buffer GL15/GL_STATIC_DRAW) 
        (GL20/glVertexAttribPointer 0 4 GL11/GL_FLOAT false 40 0)
        (GL20/glVertexAttribPointer 1 4 GL11/GL_FLOAT false 40 16)
        (GL20/glVertexAttribPointer 2 2 GL11/GL_FLOAT false 40 32)
        ;(GL20/glEnableVertexAttribArray 0)
        (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER 0)
        (GL30/glBindVertexArray 0)

        (GL15/glBindBuffer GL15/GL_ELEMENT_ARRAY_BUFFER vboi-id)
        (GL15/glBufferData GL15/GL_ELEMENT_ARRAY_BUFFER indices-buffer GL15/GL_STATIC_DRAW)
        (GL15/glBindBuffer GL15/GL_ELEMENT_ARRAY_BUFFER 0)
        
        (die-if-opengl-error "setup quad")
        {:vao-id vao-id 
         :vbo-id vbo-id 
         :vboi-id vboi-id 
         :vertices-buffer vertices-buffer
         :indices-count (alength ^ints data-indices)})))


;(defn get-time []
  ;(/ (* (Sys/getTime) 1000.0) (float (Sys/getTimerResolution))))

(def cur-fps (atom 0))
(def last-fps (atom 0))
(def fps-counter (atom 0))

(defn init []
  (try
    (let [display-mode (new DisplayMode WIDTH HEIGHT)
          pixel-format (new PixelFormat)
          context-attributes (-> (new ContextAttribs 3 2) 
                                 (.withForwardCompatible true)
                                 (.withProfileCore true))]
      (Display/setDisplayMode display-mode)
      (Display/setResizable true)
      (Display/create pixel-format context-attributes)
      (GL11/glViewport 0 0 (Display/getWidth) (Display/getHeight))
      (reset! fps-counter 0)
      (reset! last-fps (get-time))
      (init-opengl))
    (catch LWJGLException e
      (println "LWJGL Exception occurred" (.getMessage e))))

  ; TODO: Remove these
  (GL11/glClearColor 0 1 1 0)
  (GL11/glViewport 0 0 (Display/getWidth) (Display/getHeight)))

(defn get-version []
  (GL11/glGetString GL11/GL_VERSION))

(defn update-fps [cur-time]
  (if (> (- cur-time @last-fps) 1000.0)
    (do
      (Display/setTitle (format "FPS: %d, GL: %s" @fps-counter (get-version)))
      (reset! cur-fps @fps-counter)
      (reset! fps-counter 0)
      (reset! last-fps cur-time) 
      ;(swap! last-fps + 1000.0)
      ))
  (swap! fps-counter inc))

(def matrix-buffer (BufferUtils/createFloatBuffer 16))

(defn process [res dt abs-time]
  (let [;[hero-x hero-y] @hero
        ;hero-entity (:entity @(place @hero))
        ;busy (:busy hero-entity)
        v (dosync (apply vector (for [x (range dim0) y (range dim1)] @(place [x y])))) 
        ]
    (dotimes [i dim0]
      (dotimes [j dim1]
        (FastAccess/setTexture data dim0 dim1 i j 0 1)
        (FastAccess/setColor data dim0 dim1 i j 0 1.0 1.0 0.4 1.0)
        (FastAccess/setTexture data dim0 dim1 i j 1 0)))

    ;(println hero-entity)
    ;(FastAccess/setTexture data dim0 dim1 hero-x hero-y 1 8) 

    (dotimes [i dim0]
      (dotimes [j dim1]
        (let [tile (nth v (+ (* i dim1) j))]
          (FastAccess/setTexture data dim0 dim1 i j 0 (:type tile))
          (when (and (not= (:entity-id tile) -1))
            (if (nil? (:entity-id tile))
              0;(println "ERROR" i j)
              (let [entity @(enemies (:entity-id tile))]
                (FastAccess/setTexture data dim0 dim1 i j 1 (:type entity))))))))

    ; Bind program
    (GL20/glUseProgram (int (-> res :shaders :p-id)))

    (let [matrix (-> res :matrices :projection)]
      (.store ^Matrix4f matrix ^FloatBuffer matrix-buffer))
    (.flip ^FloatBuffer matrix-buffer) 
    (GL20/glUniformMatrix4 (-> res :shaders :uni-proj-matrix) false matrix-buffer)

    ;(aset-float data 8 0.0)
    ;(aset-float data 9 0.0)
    ;(aset-float data 18 0.0)
    ;(aset-float data 19 (/ 10.0 128.0))
    ;(aset-float data 28 (/ 10.0 128.0))
    ;(aset-float data 29 (/ 10.0 128.0))
    ;(aset-float data 38 (/ 10.0 128.0))
    ;(aset-float data 39 0.0) 

    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER (-> res :quads :vbo-id))
    (let [vertices-buffer (-> res :quads :vertices-buffer)]
      (.rewind ^FloatBuffer vertices-buffer)
      (.put ^FloatBuffer vertices-buffer ^floats data)
      (.flip ^FloatBuffer vertices-buffer)

      (GL15/glBufferSubData GL15/GL_ARRAY_BUFFER 0 ^FloatBuffer vertices-buffer))
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER 0)

    (GL20/glUseProgram 0)

    (comment
      (if (or (not busy) (> abs-time busy))
        (cond 
          ; Right
          (Keyboard/isKeyDown Keyboard/KEY_RIGHT) 
          (send hero entity-rel-move :right abs-time) 
          ; Left 
          (Keyboard/isKeyDown Keyboard/KEY_LEFT) 
          (send hero entity-rel-move :left abs-time)
          ; Up
          (Keyboard/isKeyDown Keyboard/KEY_UP) 
          (send hero entity-rel-move :up abs-time) 
          ; Down
          (Keyboard/isKeyDown Keyboard/KEY_DOWN) 
          (send hero entity-rel-move :down abs-time))))))

(defn render [res]
  (let []

    ; For development only - execute GL commands from outside the thread
    (let [c @gl-commands]
      (when-not (empty? c)
        (import '(org.lwjgl.opengl GL11))
        (try
          (dorun (map eval c))
          (catch Exception e (prn "caught exception: " (.getMessage e))))
        (reset! gl-commands '())))

    (let [quad (:quads res)]
      (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)
      (GL11/glEnable GL11/GL_BLEND)
      (GL11/glDisable GL11/GL_DEPTH_TEST)
      
      (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
      
      (GL20/glUseProgram (-> res :shaders :p-id))

      (GL13/glActiveTexture GL13/GL_TEXTURE0)
      (let [tex (-> res :tex :tiles)]
        (.bind ^Texture tex)) 
      (GL11/glTexParameteri GL11/GL_TEXTURE_2D 
                            GL11/GL_TEXTURE_MAG_FILTER 
                            GL11/GL_NEAREST)
      (GL11/glTexParameteri GL11/GL_TEXTURE_2D 
                            GL11/GL_TEXTURE_MIN_FILTER 
                            GL11/GL_NEAREST)

      (GL30/glBindVertexArray (:vao-id quad))
      (GL20/glEnableVertexAttribArray 0)
      (GL20/glEnableVertexAttribArray 1)
      (GL20/glEnableVertexAttribArray 2)

      (GL15/glBindBuffer GL15/GL_ELEMENT_ARRAY_BUFFER (:vboi-id quad))

      (GL11/glDrawElements GL11/GL_TRIANGLES 
                           (int (:indices-count quad))
                           GL11/GL_UNSIGNED_INT 
                           0)

      (GL15/glBindBuffer GL15/GL_ELEMENT_ARRAY_BUFFER 0)
      (GL20/glDisableVertexAttribArray 0)
      (GL20/glDisableVertexAttribArray 1)
      (GL20/glDisableVertexAttribArray 2)
      (GL30/glBindVertexArray 0)

      (GL20/glUseProgram 0))))


(defn load-shader [filename shader-type]
  (let [shader-source ^String (slurp filename)
        shader-id (GL20/glCreateShader shader-type)]
    (GL20/glShaderSource (int shader-id) shader-source)
    (GL20/glCompileShader (int shader-id))
    (when (= GL11/GL_FALSE (GL20/glGetShaderi shader-id GL20/GL_COMPILE_STATUS))
      (prn "Could not compile shader."))
    shader-id))

(defn setup-matrices []
  (let [pmat (new Matrix4f)]
    (set! (.m00 pmat) (/ 2.0 (Display/getWidth)))
    (set! (.m30 pmat) -1.0)
    (set! (.m11 pmat) (/ 2.0 (Display/getHeight)))
    (set! (.m31 pmat) -1.0)
    (set! (.m22 pmat) 1.0)
    (set! (.m33 pmat) 1.0)
    {:projection pmat}))

(defn setup-shaders []
  (let [vs-id (load-shader "resources/vertex.glsl" GL20/GL_VERTEX_SHADER)
        fs-id (load-shader "resources/fragment.glsl" GL20/GL_FRAGMENT_SHADER)
        p-id (GL20/glCreateProgram)]
    (GL20/glAttachShader p-id vs-id)
    (GL20/glAttachShader p-id fs-id)
    (GL20/glBindAttribLocation p-id 0 "in_Position")
    (GL20/glBindAttribLocation p-id 1 "in_Color")
    (GL20/glBindAttribLocation p-id 2 "in_TextureCoord")
    (GL20/glLinkProgram p-id)
    (GL20/glValidateProgram p-id)
    (let [projection-matrix-location (GL20/glGetUniformLocation p-id "projectionMatrix")]
      ; TODO: Check error.
      (die-if-opengl-error "setup shaders")
      {:p-id p-id
       :vs-id vs-id
       :fs-id fs-id
       :uni-proj-matrix projection-matrix-location})))


(defn ^Texture texture-load [path]
  (try
    (TextureLoader/getTexture "PNG" (ResourceLoader/getResourceAsStream path))
    (catch IOException e
      (println "Failed to load texture " + e))))

(defn load-resources []
  (let [quads (setup-quads)
        shaders (setup-shaders)
        matrices (setup-matrices)
        resources {:tex {:tiles (texture-load "resources/atlas.png")
                         ;:entities (texture-load "resources/character.png")
                         ;:alphabet (texture-load "resources/alphabet.png")
                         }
                   :shaders shaders 
                   :matrices matrices 
                   :quads quads}]
    ; Setup all textures
    (die-if-opengl-error "Resources")
    (doseq [tex (vals (:tex resources))]
      (.bind ^Texture tex)
      (GL11/glTexParameteri GL11/GL_TEXTURE_2D 
                            GL11/GL_TEXTURE_MAG_FILTER 
                            GL11/GL_NEAREST)
      (GL11/glTexParameteri GL11/GL_TEXTURE_2D 
                            GL11/GL_TEXTURE_MIN_FILTER 
                            GL11/GL_NEAREST))
    (die-if-opengl-error "Setup textures")
    resources))

(defn game-loop [resources]
  (loop [last-time (- (get-time) 30)
         res resources]
    (if-not (Display/isCloseRequested)
      ; Rendering
      (let [cur-time (get-time)] 
        (process res (- cur-time last-time) cur-time)
        (render res)
        (update-fps cur-time)
        (Display/sync 60)
        (Display/update)
        (die-if-opengl-error "Uncaught OpenGL error!")
        ; If GL reload has been issued, we need to reload
        ; the resources
        (if @gl-reload-resources
          (do 
            (reset! gl-reload-resources false)
            (destroy-opengl res)
            (init-opengl)
            (let [new-res (load-resources)]
              (println "Reload resources" new-res)
              (recur cur-time new-res)))
          (recur cur-time res)))
      ; Closing and cleaning up
      (do
        (destroy-opengl res)
        (Display/destroy)))))

(defn run []
  (try
    (init)
    (let [res (load-resources)]
      (do
        (die-if-opengl-error "Going in")
        (game-loop res)))
    (catch Exception e 
      (println "caught exception: " (.getMessage e) (clojure.stacktrace/print-stack-trace e))))
  nil)

(defn start-thread []
  (let [thread (Thread. run)]
    (.start ^Thread thread)) )

(defn -main
  [& args]
  (start-thread))
