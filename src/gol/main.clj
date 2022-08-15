(ns gol.main
  (:require
   [nrepl.server :as nrepl]
   [gol.game :as game])

  (:import
   [org.jetbrains.skija BackendRenderTarget Canvas ColorSpace DirectContext FramebufferFormat Paint Rect Surface SurfaceColorFormat SurfaceOrigin]
   [org.lwjgl.glfw Callbacks GLFW GLFWErrorCallback]
   [org.lwjgl.opengl GL GL11]
   [org.lwjgl.system MemoryUtil]))

(set! *warn-on-reflection* true)

(defn color [^long l]
  (.intValue (Long/valueOf l)))

(def *rect-color (atom (color 0xFFCC3333)))

(def w 300)
(def h 300)

(defn draw [^Canvas canvas]
  (let [paint (doto (Paint.) (.setColor @*rect-color))]

    (game/iterate-state
      (fn [x y]
        (.drawRect canvas (Rect/makeXYWH x y 1 1) paint)))))

(defn display-scale [window]
  (let [x (make-array Float/TYPE 2)
        y (make-array Float/TYPE 2)]
    (GLFW/glfwGetWindowContentScale window x y)
    [(first x) (first y)]))

(defn -main [& args]
  (.set (GLFWErrorCallback/createPrint System/err))
  (GLFW/glfwInit)
  (GLFW/glfwWindowHint GLFW/GLFW_VISIBLE GLFW/GLFW_FALSE)
  (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GLFW/GLFW_TRUE)
  (let [width (* 2 w)
        height (* 2 h)
        window (GLFW/glfwCreateWindow width height "Skija LWJGL Demo" MemoryUtil/NULL MemoryUtil/NULL)]
    (GLFW/glfwMakeContextCurrent window)
    (GLFW/glfwSwapInterval 1)
    (GLFW/glfwShowWindow window)
    (GL/createCapabilities)

    (doto (Thread. #(clojure.main/main))
      (.start))

    (nrepl/start-server :port 7888)
    (println "nREPL server started at locahost:7888")

    (let [context (DirectContext/makeGL)
          fb-id   (GL11/glGetInteger 0x8CA6)
          [scale-x scale-y] (display-scale window)
          target  (BackendRenderTarget/makeGL (* scale-x width) (* scale-y height) 0 8 fb-id FramebufferFormat/GR_GL_RGBA8)
          surface (Surface/makeFromBackendRenderTarget context target SurfaceOrigin/BOTTOM_LEFT SurfaceColorFormat/RGBA_8888 (ColorSpace/getSRGB))
          canvas  (.getCanvas surface)]
      (.scale canvas 4 4)

      (game/random-state! w h)

      (loop []
        (when (not (GLFW/glfwWindowShouldClose window))
          (.clear canvas (color 0xFFFFFFFF))
          (let [layer (.save canvas)]
            (draw canvas)
            (.restoreToCount canvas layer))
          (.flush context)
          (GLFW/glfwSwapBuffers window)
          (GLFW/glfwPollEvents)
          (game/evolve-map!)
          (recur)))

      (Callbacks/glfwFreeCallbacks window)
      (GLFW/glfwHideWindow window)
      (GLFW/glfwDestroyWindow window)
      (GLFW/glfwPollEvents)

      (.close surface)
      (.close target)
      (.close context)

      (GLFW/glfwTerminate)
      (.free (GLFW/glfwSetErrorCallback nil))
      (shutdown-agents)
)))
