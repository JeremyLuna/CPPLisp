; cant map transpile, use lambda instead
(defmacro transpile [& cpp_in_list]
  (cond
    (empty? cpp_in_list) ""
    :else (let [token (first cpp_in_list)]
            (cond
              (string? token) (str "\"" (name token) "\"")
              (symbol? token) (name `~token)
              (keyword? token) (name `~token)
              (list? token) (cond
                              (empty? token) ""
                              :else (case (first token)
                                      + `(clojure.string/join "+" (map #(transpile %) (quote ~(rest token))))
                                      - (clojure.string/join "-" (map transpile (rest token)))
                                      statement (clojure.string/join `(~@(map transpile (rest token)) ";"))))
              :else token))))

(defn unit_test []
  [(transpile)
   (transpile hi)
   (transpile "hi")
   (transpile def)
   (transpile (+ "hi" "hi"))
   ;(transpile '(+ a a))
   ;(transpile '(+ 2 2))
  ])

(unit_test)
