(ns frd-char-creator.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [taoensso.timbre :as log]
              ))

;; -------------------------
;; Constants
(def skillsList
  {:Body [:Endurance :Strength :Threshold :Toughness]
   :Dexterity [:Aim :Ledgerdemain :Speed :Stealth]
   :Intelligence [:Profession1 :Profession2 :Proficiency1 :Proficiency2]
   :Savvy [:AnimalKen :Dodge :Perception :Social]
   }
  )

(def fortuneSkillsList
  {:Fortune [:Bonus :Capacity :Return]
   }
  )

;; -------------------------
;; State
(def appState
  (atom
    {:suit :Body
     :skills (apply merge {}
                    (apply merge {} (for [stat (keys skillsList)] {stat {:value 1}}))
                    (map (fn [[k v]] (apply merge {} (for [skill v] {skill {:parent k :value 1}})))
                         skillsList
                         )
                    )
     :fortuneSkills (apply merge {}
                           (apply merge {} (for [stat (keys fortuneSkillsList)] {stat {:value 1}}))
                           (map (fn [[k v]] (apply merge {} (for [skill v] {skill {:parent k :value 1}})))
                                fortuneSkillsList
                                )
                           )
     :maximumSkill 5
     :totalSpent 0
     }
    )
  )

;; -------------------------
;; Helper Functions
(defn- bound-integer
  "Given a lower and upper, bound n to it"
  [^Integer lower ^Integer upper ^integer n]
  (log/trace "bound-integer. lower:" lower "upper:" upper "n:" n)
  ;; If n is below lower, raise it
  (->
    (max lower
         ;; If n is above upper, lower it
         (min upper
              n
              )
         )
    (#(if (= n %)
        %
        (do
          (log/trace "bound-integer. n changed from" n "to" %)
          %
          )
        ))
    )
  )

(defn- check-skills
  "Checks no skill is above its parent"
  [as]
  (update-in
    as [:skills]
    (fn [skillMap]
      (apply merge {}
             (for [[k {:keys [parent value] :as v}] skillMap]
               ^{:key k}
               {k (assoc v
                         :value
                         (bound-integer 0
                                        (if parent
                                          (get-in skillMap [parent :value])
                                          (:maximumSkill as))
                                        value))}
               )
             )
      )
    )
  )

(defn- calculate-total-points
  "Calculates total points spent"
  [as]
  (log/info "calculate-total-points called")
  (let [suit (:suit as)
        total
        (apply +
               (map (fn [[k {:keys [parent value] :as v}]]
                      (if parent
                        ;; Skills
                        (* (reduce + (range (inc value)))
                           (if (= parent suit)
                             ;; Is suit
                             20
                             ;; Is not suit
                             30
                             )
                           )
                        ;; Stat
                        (* (reduce + (range (inc value)))
                           (if (= k suit)
                             ;; Is suit stat
                             40
                             ;; Is non-suit stat
                             50
                             )
                           )
                        )
                      )
                    (:skills as)
                    )
               )
        ]
    (log/info "Total spent is:" total)
    (assoc as :totalSpent total)
    )
  )

(defn- update-skills
  "Checks no skill is above its parent, then calculates totalSpent"
  [as]
  (log/info "Update-skills called")
  (-> as
      check-skills
      calculate-total-points
      )
  )

(defn- modify-skill
  "Modifies a skill rating by n"
  [skill n]
  (log/info "modify-skill" skill n)
  (swap! appState update-in [:skills skill :value]
         (comp
           update-skills
           ;; Less than 0? Cap it
           (fn [i] (max 0
                        ;; Moving past maximumSkill? Cap it
                        (min (+ n i)
                             (:maximumSkill @appState)
                             )
                        )
             )
           )
         )
  )

(defn- get-skill
  "Gets the value of a skill"
  [^Keyword skill]
  (get-in @appState [:skills skill :value])
  )

(defn- set-skill
  [skill n]
  (log/info "set-skill" skill n)
  (swap! appState
         (comp
           update-skills
           #(assoc-in %
                      [:skills skill :value]
                      (max 0
                           (min n
                                (:maximumSkill @appState)
                                )
                           )
                      )
           )
         )
  )

(defn- dec-skill
  "Decrements a skill by one, to a minimum of 0"
  [skill]
  (modify-skill skill -1))

(defn- inc-skill
  [skill]
  (modify-skill skill 1))

(defn- skill-slider-component
  [^Keyword k]
  [:div {:class "btn-group"}
   (doall (for [n (range (inc (:maximumSkill @appState)))]
            ^{:key n}
            [:span {:class (if (= n (get-in @appState [:skills k :value]))
                             "btn btn-info btn-sm"
                             "btn btn-default btn-sm"
                             )
                    :onClick #(set-skill k n)
                    }
             n
             ]
            )
          )
   ]
  )

;; -------------------------
;; Components
(defn skill-component
  "Renders a single component"
  [^Keyword skill]
  (fn []
    [:span
     [:h4 (name skill)]
     [skill-slider-component skill]
     ]
    )
  )

(defn stat-component
  "Renders a statistic, along with its associated skills"
  [^Keyword stat]
  (fn []
    [:div
     {:class "well"}
     [:h3 {:class (if (= (:suit @appState) stat)
                    "btn btn-default btn-success"
                    "btn btn-default "
                    )
           :onClick #(do (swap! appState assoc :suit stat)
                         (swap! appState update-skills)
                         )
           }
      (name stat)
      ]
     [skill-slider-component stat]
    (doall (for [k (stat skillsList)]
             ^{:key k}
             [skill-component k]
             )
           )
     ]
    )
  )

(defn derived-component
  "Displays stats about the current character"
  []
  (fn []
    [:table {:class "table table-striped table-hover "}
     [:tbody
      [:tr>td "Health Points: " (+ 10 (get-skill :Body) (get-skill :Endurance))]
      [:tr>td "Fortune Points: " 4]
      [:tr>td "Avoidance: " (apply + 7 (map get-skill [:Dexterity :Speed :Dodge]))]
      [:tr>td "Shrug: " (apply + 6 (map get-skill [:Body :Toughness]))]
      [:tr>td "Movement: " (+ 6 (get-skill :Dexterity) (get-skill :Speed)) " m/round"]
      [:tr>td "Spent XP: " (:totalSpent @appState)]
      ;; Slider
      (if (> (:totalSpent @appState) 1000)
        ;; Negative amount left
        [:tr>td "Negative XP: " (- 1000 (:totalSpent @appState))
         [:div {:class "progress"}
          [:div {:class "progress-bar progress-bar-danger"
                 :style {:width (-> @appState
                                    :totalSpent
                                    ;; Totalspent is >1000, get the surplus
                                    (#(- % 1000))
                                    ;; Cap it at 1000
                                    (min 1000)
                                    ;; Divide by 1000
                                    (/ 1000)
                                    ;; We have a decimal amount, now convert to percentage
                                    (* 100)
                                    ;; Remove decimal
                                    int
                                    ;; Add percentage
                                    (str "%")
                                    )
                         }
                 }
           ]
          ]
         ]
        ;; Fine
        [:tr>td "Remaining XP: " (- 1000 (:totalSpent @appState))
         [:div {:class "progress"}
          [:div {:class "progress-bar"
                 :style {:width (str (int (* (/ (- 1000 (:totalSpent @appState)) 1000) 100)) "%")}
                 }
           ]
          ]
         ]
        )
      ]
     ]
    )
  )

;; -------------------------
;; Views

(defn home-page []
  [:div
   [:h2 "Welcome to frd-char-creator"]
   [:table
    [:tbody
     (doall (map (fn [stat] ^{:key stat} [:td [stat-component stat]]) (keys skillsList)))
     [derived-component]
     ]
    ]
   ]
   )

(defn about-page []
  [:div [:h2 "About frd-char-creator"]
   [:div [:a {:href "/"} "go to the home page"]]])

(defn current-page []
     [:div
      [(session/get :current-page)]
      ]
    )

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about-page))

;; -------------------------
;; Initialize app

(swap! appState update-skills)

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
