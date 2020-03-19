Hér eru skrár og kóði á bak við vefsíðuna [bgautijonsson.shinyapps.io/COVID_Dashboard](https://bgautijonsson.shinyapps.io/COVID_Dashboard/) og annar kóði sem ég hef verið að fikta við.

# Gögn

Ég vinn með tvenns konar gögn:

* Heimsgögn frá ECDC (European CDC). Þau sæki ég með Make_ECDC_Data.R skránni, sem leitar á vefsíðunni hvort komið hafa ný gögn í dag og ef ekki sækir hún gögn gærdagsins. Ég nota þessi gögn því ég hef fundið skrýtnar villur í *JHU* gögnunum, eins og t.d. 5 dauðsföll á Íslandi 16. mars.

* Íslandsgögn frá Landlækni og Almannavörnum, sem ég hef sjálfur sótt og geymi í google drive í slóð: https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA sæki svo gögnin með googlesheets4 pakkanum og skrifa út þær skrár sem ég tel mig munu vilja nota.

# App

Vefsíðan er öll sett upp í COVID_Dashboard möppunni, nánar tiltekið app.R skránni. Ég hef ekki komið mér í að commenta vel kóðann þar, en ef einhver hefur áhuga á að taka við viðhaldi á appinu hafið endilega samband og við förum yfir þetta.

Todo: Skipta út gömlu Ísland síðunni fyrir nýjum flipa með betri spám.

# Líkanasmíð

Forspár okkar byggjast eins og er á Logistic vexti uppsafnaðra smita, getum svo seinna módelað hvað gerist eftir að hápunkti er náð. Spábil fáum við með beitingu delta aðferðarðinnar með tölulegri diffrun og hermun. Ég er líka að vinna í svipuðu stigskiptu Bayesísku líkani sem samnýtir gögn víðs vegar að úr heiminum. Sú úrvinnsla finnst Í Stan/Logistic.

# Forspá

Skráin `Make_Iceland_Preds.R` reiknar spár og hermir skiptingu í aldurshópa ásamt álagi á spítalann. Skilar niðurstöðum í Data/ möppuna sem preds_cumulative.csv, preds_active.csc, simulations_cumulative.csv og simulations_active.csv. Svo er ég að vinna í að setja upp parametric report í möppunni Simulation_Report/. Líklegast verður það einhvers konar Shiny_Prerendered RMarkdown document með möguleika um að hlaða niður töflum osf.


Kær kveðja,
Brynjólfur Gauti Jónsson
Tölfræðiráðgjöf Heilbrigðisvísindasviðs Háskóla Íslands