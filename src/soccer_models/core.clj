(ns soccer-models.core
  (:gen-class)
  (:require 
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [incanter.optimize :as op]))

(defn log [x]
  (do (println x))
  x)

(defn lower-keyword [x]
  (keyword (clojure.string/lower-case x)))

(defn csv-data->maps [csv-data]
  (map zipmap
       (->> (first csv-data)       ;; First row is the header
            (map lower-keyword)    ;; Drop if you want string keys instead
            repeat)
	  (rest csv-data)))

(defn name-to-keyword [x]
  (-> x
       (clojure.string/replace #"\s+" "-")
       clojure.string/lower-case
       keyword))

(defn str-to-int [x]
  (Integer. x))

(defn parse-value [kv-map key f]
  (assoc kv-map key (f (key kv-map))))

(defn parse-teamnames [game]
  (-> game
      (parse-value :hometeam name-to-keyword)
      (parse-value :awayteam name-to-keyword)))

(defn parse-goals [game]
  (-> game
      (parse-value :fthg str-to-int)
      (parse-value :ftag str-to-int)))

(defn load-games [file-path]
  (->> file-path
       io/reader
       csv/read-csv
       csv-data->maps
       (map parse-teamnames)
       (map parse-goals)))
  
(defn unique-teams [games]
  (let [home-teams (map :hometeam games)
        away-teams (map :awayteam games)]
    (distinct (concat home-teams away-teams))))

(defn map-zeroes [keys]
  (->> keys
       count
       (#(repeat % 0))
       (zipmap keys)))

(defn init-x [games]
  (let [teams (unique-teams games)]
    (-> teams
        count  ; n-teams
        (* 2)  ; account for off and def
        (+ 1)  ; add room for hfa parameter
        (repeat 0))))

; Need to make :off and :def *maps*
(defn array->params [teams x]
  (let [n-teams (count teams)]
    (hash-map :off (->> x (take n-teams) (zipmap teams)),
              :def (->> x (drop n-teams) (take n-teams) (zipmap teams))
              :hfa (->> x (take-last 1) first))))

(defn exp [x]
  (Math/pow Math/E x))

(defn factorial [n]
  (reduce * (range 1 (inc n))))

(defn poisson-lpmf [lambda k]
  (->> (Math/pow lambda k)
       (* (exp (* lambda -1)))
       (#(/ % (factorial k)))
       Math/log))

(defn calculate-rate [params o-team d-team hfa-fun]
  (let [o-param (o-team (:off params))
        d-param (d-team (:def params))]
    (-> (hfa-fun (+ o-param d-param) (:hfa params))
        exp)))

(defn calculate-rates [params game]
  (let [ht (:hometeam game)
        at (:awayteam game)]
    (hash-map :home-goals (:fthg game), 
              :away-goals (:ftag game),
              :home-rate (calculate-rate params ht at +)
              :away-rate (calculate-rate params ht at -))))

(defn home-log-like [rates]
  (let [hg (:home-goals rates)
        hr (:home-rate rates)]
    (poisson-lpmf hr hg)))

(defn away-log-like [rates]
  (let [ag (:away-goals rates)
        ar (:away-rate rates)]
    (poisson-lpmf ar ag)))

(defn log-like-game [params game]
  (->> game
       (calculate-rates params)
       (#(+ (home-log-like %1) (away-log-like %1)))))

(defn neg-log-like [params games]
  (->> games
       (map #(log-like-game params %))
       (map #(* -1 %))
       (reduce +)))

(defn objective-function [x games]
  (let [teams (unique-teams games)]
    (->> x
        (log)
        (array->params teams)
        (#(neg-log-like % games)))))

(defn fit [games]
  (let [teams (unique-teams games)
        f (fn [x] (objective-function x games))]
    (->> games
         init-x
         (op/minimize f))))

(def games (load-games "data/2016.csv"))
(def teams (unique-teams games))
(def params (->> games init-x (array->params teams)))
(neg-log-like params games)