library(rmarkdown)
library(stringr)
cur_date <- as.character(Sys.Date())

if (!file.exists(cur_date)) {
    dir.create(cur_date)
    dir.create(str_c(cur_date, "/Figures"))
}

Sys.sleep(1)

out_path <- str_c(cur_date, "/Simulation_Report_", cur_date, ".html")

render("Simulation_Report.Rmd", output_format = "html_document", output_file = out_path)
