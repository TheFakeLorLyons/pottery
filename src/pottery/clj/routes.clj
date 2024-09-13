(ns pottery.clj.routes
  (:require [compojure.core :refer [defroutes POST GET]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.session.cookie :refer [cookie-store]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.cors :refer [wrap-cors]]
            [pottery.clj.handlers :as hnd]))

(defroutes app-routes
  (POST "/create-account" [] hnd/create-account)

  (GET "/generate-a-password" [] hnd/generate-a-password))

(def handler
  (-> app-routes
      (wrap-params)
      (wrap-cors :access-control-allow-origin  #".*"
                 :access-control-allow-methods [:get :post :delete :options])
      (wrap-session {:store (cookie-store)})
      (wrap-json-body)
      (wrap-json-response)))

(defn -main [& args]
  (run-jetty handler {:port 3000 :join? false}))

(comment (-main)

         (tap> "hello")

         (defn foo [n]
           (->> (range n)
                (filter odd?)
                (partition-all 2)
                (map second)
                (remove nil?)
                (drop 10)
                (reduce +)))

         (foo 70)

         (defonce server (atom nil))

         (defn start-server []
           (when @server
             (.stop @server))  ; Stop the existing server if it's running
           (reset! server (run-jetty #'handler {:port 3000 :join? false})))

         (defn stop-server []
           (when @server
             (.stop @server)
             (reset! server nil)))  ; Clear the server reference after stopping

         (defn restart-server []
           (stop-server)
           (start-server)))
