(ns mg.dt.maltese.article)

(defn add [word]
  (cond
    (= word "skiet") "is-skiet" ; exception
    (re-matches #"(żb|żv|xk|sf|sk|sp|st).*" word) (str "l-i" word)
    (re-matches #"(l|m|n|r)[bcċdfgġhħjklmnpqrstvwxzż].*" word) (str "l-i" word)
    (re-matches #"(għ|h|a|e|i|ie|o|u).*" word) (str "l-" word)
    (re-matches #"(c|ċ|d|n|r|s|t|x|ż|z).*" word) (str "i" (first word) "-" word)
    :else (str "il-" word)))

(comment
  (and
   ;; exceptions
   (= (add "skiet") "is-skiet")
   ;; special rules
   (= (add "żvantaġġ") "l-iżvantaġġ")
   (= (add "żball") "l-iżball")
   (= (add "xkupa") "l-ixkupa")
   (= (add "storja") "l-istorja")
   (= (add "sptar") "l-isptar")
   (= (add "sfida") "l-isfida")
   (= (add "skont") "l-iskont")
   (= (add "mgħarfa") "l-imgħarfa")
   (= (add "ljun") "l-iljun")
   (= (add "njam") "l-injam")
   (= (add "rħula") "l-irħula")
   ;; common rules
   (= (add "ajruplan") "l-ajruplan")
   (= (add "bagalja") "il-bagalja")
   (= (add "ċavetta") "iċ-ċavetta")
   (= (add "dar") "id-dar")
   (= (add "elmu") "l-elmu")
   (= (add "ftira") "il-ftira")
   (= (add "gomma") "il-gomma")
   (= (add "ġnien") "il-ġnien")
   (= (add "hena") "l-hena")
   (= (add "ħelu") "il-ħelu")
   (= (add "għalliem") "l-għalliem")
   (= (add "ilma") "l-ilma")
   (= (add "iehor") "l-iehor")
   (= (add "jott") "il-jott")
   (= (add "karta") "il-karta")
   (= (add "lampa") "il-lampa")
   (= (add "mara") "il-mara")
   (= (add "nar") "in-nar")
   (= (add "ors") "l-ors")
   (= (add "papra") "il-papra")
   (= (add "qattus") "il-qattus")
   (= (add "re") "ir-re")
   (= (add "sultan") "is-sultan")
   (= (add "tieqa") "it-tieqa")
   (= (add "umbrella") "l-umbrella")
   (= (add "vapur") "il-vapur")
   (= (add "wiċċ") "il-wiċċ")
   (= (add "xemx") "ix-xemx")
   (= (add "żrinġ") "iż-żrinġ")
   (= (add "ziju") "iz-ziju")))
