(ns pottery.clj.handlers
    (:require [clojure.data.csv :as csv]
              [clojure.edn :as edn]
              [clojure.java.io :as jio]
              [libpython-clj.python :as py]
              [libpython-clj.require :as py-req]
              [clojure.java.io :as io]
              [clojure.data.json :as json]))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                          ;                 IO                  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (defn create-object [request]
    (let [body (:body request)
          profile-name (get body "userProfileName")
          login-password (get body "userLoginPassword")
          user-profile (usr/create-account profile-name login-password)]
      (println "Received request body:" request)
      (println "Generated user-profile:" (:userProfileName user-profile) (:userLoginPassword user-profile))
      (if (and profile-name login-password)  ; Example condition: check if profile-name and login-password are non-nil
        {:status 200
         :headers {"Content-Type" "application/json"}
         :body user-profile}
        {:status 401
         :headers {"Content-Type" "application/json"}
         :body (cjson/write-str {:message "Login failed. Profile name or password mismatch."})})))
  

  
  ;; Initialize the Python interpreter
  (py/initialize!)
  
  ;; Import necessary Python modules
  (py-req/require 'scrapy)
  
  (defn run-scrapy-spider []
    (py/eval
     "import scrapy
  from scrapy.crawler import CrawlerProcess
  from solo_guitar_spider import SoloGuitarSpider
  
  process = CrawlerProcess()
  process.crawl(SoloGuitarSpider)
  process.start()
     "))
  
  (defn read-links []
    (with-open [r (io/reader "solo_guitar_links.json")]
      (json/read (slurp r) :key-fn keyword)))
  
  (defn process-solo-guitar-links []
    ;; Run the Scrapy spider
    (run-scrapy-spider)
    ;; Read and print the links
    (println (read-links)))
