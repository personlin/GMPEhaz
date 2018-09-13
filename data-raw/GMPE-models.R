GMPEhaz.model <- readxl::read_xlsx("data-raw/GMPE_list.xlsx")
save(GMPEhaz.model, file="data/GMPEhaz-model.rda")
