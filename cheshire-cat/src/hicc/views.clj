(ns hicc.views
  (require
    [hiccup.page :refer [html5]]))

(defn main []
  (html5
    [:div
     [:h1 "Hello Hiccup Page!"]
     [:p "What would you like to do?"]
     [:p [:a {:href "./cat.html"} "Go to cat.html"]]
     ]))
