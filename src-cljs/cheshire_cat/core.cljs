(ns cheshire-cat.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [enfocus.core :as ef]
            [enfocus.events :as ev]
            [enfocus.effects :as ee]))

(defn say-goodbye []
  (ef/at
    "#cat-name" (ee/fade-out 500)
    "#button1" (ee/fade-out 1000)
    "#status" (ee/fade-out 1500)
    ))

(defn ^:export init []
  (go
    (let [response (<! (http/get "/cheshire-cat"))
          body (:body response)]
      (ef/at "#cat-name" (ef/content (:name body)))
      (ef/at "#status" (ef/do->
                         (ef/content (:status body))
                         (ef/set-style :font-size "500%")))
      (ef/at "#button1" (ev/listen :click say-goodbye))
      )))
