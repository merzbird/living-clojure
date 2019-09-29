(ns cheshire-cat.handler
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :refer [wrap-json-response]]
            [ring.util.response :as rr]
            [hicc.views :as hv]
            ))

(defroutes app-routes
           (GET "/" [] (hv/main))
           (GET "/cheshire-cat" []
             (rr/response {:name "Cheshire Cat" :status :grinning}))
           (route/not-found "Not Found"))

(def app
  (-> app-routes
      (wrap-json-response)
      (wrap-defaults site-defaults)))

