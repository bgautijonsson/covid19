# We are making changes to the repo today. Getting organized, and trying to add some english comments.


Hér eru skrár og kóði á bak við vefsíðuna [bgautijonsson.shinyapps.io/COVID_Dashboard](https://bgautijonsson.shinyapps.io/COVID_Dashboard/), [covid.hi.is](covid.hi.is) og annar kóði sem ég hef verið að fikta við.

# Gögn

Ég vinn með tvenns konar gögn:

* Heimsgögn frá ECDC (European CDC).

* Íslandsgögn frá Landlækni og Almannavörnum, sem ég hef sjálfur sótt og geymi í google drive í slóð: https://docs.google.com/spreadsheets/d/1xgDhtejTtcyy6EN5dbDp5W3TeJhKFRRgm6Xk0s0YFeA sæki svo gögnin með googlesheets4 pakkanum og skrifa út þær skrár sem ég tel mig munu vilja nota.

**Todo**

* Vinna með gögn skipt niður á svæði á Íslandi
* [John Snow cholera-esque data product](https://blogs.cdc.gov/publichealthmatters/2017/03/a-legacy-of-disease-detectives/)

# Forspárlíkan

Forspárlíkanið eins og það er nú er staðsett í Stan/Logistic/Hierarchical_Logistic_Cases.stan. Ferlið til að meta líkan og fá forspár er:

1. `Make_Stan_Data.R`
2. `Make_Stan_Model.R`
3. `Make_Stan_Predictions.R`

Skýrsla og myndir koma svo með því að keyra `Simulation_Report/Make_Simulation_Report.R`.

**Todo** 

* Bæta við Íslensk gögn um spítalalegur og smitatíðni eftir aldurshópum
* Vantar spatial hluta í líkanið
* Gera mögulegt að bera saman mismunandi forspár frá mismunandi dögum í appinu (sanity check)

# About the model

A report I wrote about the model can be found at: https://rpubs.com/bgautijonsson/588811

# dashboard

Vefsíðan bgautijonsson.shinyapps.io/COVID_Dashboard er öll sett upp í `dashboard` möppunni, nánar tiltekið `dashboard/app.R` skránni. Ég hef ekki komið mér í að commenta vel kóðann þar, en ef einhver hefur áhuga á að taka við viðhaldi á appinu hafið endilega samband og við förum yfir þetta.

**Update:** Vefsíðan mun núna færa sig yfir á [https://covid.rhi.hi.is/shiny/dashboard/](https://covid.rhi.hi.is/shiny/dashboard/). Ég mun halda áfram með mína eigin hýsingu til vonar og vara.