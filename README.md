Hér eru skrár og kóði á bak við vefsíðuna [bgautijonsson.shinyapps.io/COVID_Dashboard](https://bgautijonsson.shinyapps.io/COVID_Dashboard/) og annar kóði sem ég hef verið að fikta við.

# Gögn

Ég vinn með tvenns konar gögn:

* Heimsgögn frá ECDC (European CDC). Þau sæki ég með Make_ECDC_Data.R skránni, sem leitar á vefsíðunni hvort komið hafa ný gögn í dag og ef ekki sækir hún gögn gærdagsins. Ég nota þessi gögn því ég hef fundið skrýtnar villur í *JHU* gögnunum, eins og t.d. 5 dauðsföll á Íslandi 16. mars.

* Íslandsgögn frá Landlækni og Almannavörnum, sem ég hef sjálfur sótt og geymi í google drive í slóð: https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA sæki svo gögnin með googlesheets4 pakkanum og skrifa út þær skrár sem ég tel mig munu vilja nota.

# App

Vefsíðan er öll sett upp í COVID_Dashboard möppunni, nánar tiltekið app.R skránni. Ég hef ekki komið mér í að commenta vel kóðann þar, en ef einhver hefur áhuga á að taka við viðhaldi á appinu hafið endilega samband og við förum yfir þetta.

# Líkanasmíð

Ég hef verið að skoða líkanasmíð aðalelga í möppunni Stan, en þar nota ég líkindafræðilega forritunarmálið Stan til að skilgreina Bayesísk líkön og kalla svo á kóðann úr R. Endilega notfærið ykkur kóða sem þar er að finna ef þið viljið.

Í möppunni Modeing var ég að leika mér að *Chain Binomial* líkaninu og er sá kóði bara á *fíflast* stigi.


Kær kveðja,
Brynjólfur Gauti Jónsson
Tölfræðiráðgjöf Heilbrigðisvísindasviðs Háskóla Íslands