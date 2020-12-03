(ns roam-seven-gui-challenge.core
    (:require
      [reagent.core :as r]
      [reagent.dom :as d]))

;; -------------------------
;; Views

;; -------------------------
;; Seven-gui component 1

(def click-count (r/atom 0))

(defn counter []
  [:div 
   [:input {:disabled true :value (str "I've been clicked " @click-count " times!")}]
   [:button {:on-click #(swap! click-count inc)} "increment above number"]])

(defn component-1 []
  [:div {:class "counter"}
    [:h2 "The counter"]
    [counter]])

;; -------------------------
;; Seven gui Temperature converter

; business-logic

(comment
  am not super happy about rounding in the business logic as it is a rendering problem)
(defn celsius-to-fahrenheit [celsius]
  (Math/round (+ 32 (* celsius (/ 9 5)))))

(defn fahrenheit-to-celsius [fahrenheit]
  (Math/round (* (- fahrenheit 32) (/ 5 9))))

; state

(def celsius (r/atom 5))
(def fahrenheit (r/atom 41))

; gui helpers
(defn update-fahrenheit [e]
  (let [celsius-new (-> e .-target .-value)]
    (reset! celsius celsius-new)
    (reset! fahrenheit (celsius-to-fahrenheit celsius-new))))

(defn update-celsius [e]
  (let [fahrenheit-new (-> e .-target .-value)]
    (reset! fahrenheit fahrenheit-new)
    (reset! celsius (fahrenheit-to-celsius fahrenheit-new))))

(defn temperature-converter []
  [:div
   [:input {:value @celsius :on-change update-fahrenheit}]
   [:label "Celsius = "]
   [:input {:value @fahrenheit :on-change update-celsius}]
   [:label " Fahrenheit"]])

(defn component-2 []
  [:div {:class "temperature"}
   [:h2 "Temperature converter"]
   [temperature-converter]])

;; -------------------------
;; Seven gui main

(defn seven-gui-roam []
  [:div {:class "Seven-gui-roam"}
   [:h1 "Seven gui for Roam using ClojureScript and Reagent"]
   [component-1]
   [component-2]])
  
;; -------------------------
;; Initialize app

(defn mount-root []
  (d/render [seven-gui-roam] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
