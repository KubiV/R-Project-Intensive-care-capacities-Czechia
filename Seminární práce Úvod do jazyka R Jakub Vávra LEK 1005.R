# Úvod do skriptovacího jazyka R LS 2023
# Jakub Vávra LEK 1005
# Seminární práce - Kapacity intenzivní péče 2. vlna COVID-19

library(rnaturalearth)
library(ggplot2)
library(sf)
library(dplyr)

# Výběr dat
my_date = "2020-09-01" # Výběr data
# 2020-09-01 až 2021-03-24
my_variable = "luzka_aro_jip_kapacita_volna_covid_pozitivni"  #Výběr sledované hodnoty 
# Výběr: ecmo_kapacita_volna	ecmo_kapacita_celkem	upv_kapacita_volna	upv_kapacita_celkem	crrt_kapacita_volna	crrt_kapacita_celkem	ihd_kapacita_volna	ihd_kapacita_celkem	luzka_aro_jip_kapacita_celkem	luzka_aro_jip_kapacita_volna_covid_pozitivni	luzka_aro_jip_kapacita_volna_covid_negativni	luzka_standard_kyslik_kapacita_celkem	luzka_standard_kyslik_kapacita_volna_covid_pozitivni	luzka_standard_kyslik_kapacita_volna_covid_negativni	ventilatory_prenosne_kapacita_volna	ventilatory_prenosne_kapacita_celkem	ventilatory_operacni_sal_kapacita_volna	ventilatory_operacni_sal_kapacita_celkem

# Přečtení obsahu textového souboru
table = readLines("https://dip.mzcr.cz/api/v1/kapacity-intenzivni-pece-vlna-2.csv", warn = FALSE) # https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19

# Nahrazení textů kvůli párování údajů k příslušnému kraji
new_table = gsub("kraj_nuts_kod", "nazev_kraje", table)
new_table = gsub(my_variable, "sledovana_hodnota", new_table)
new_table = gsub("CZ042", "CZ-US", new_table)
new_table = gsub("CZ051", "CZ-LI", new_table)
new_table = gsub("CZ041", "CZ-KA", new_table)
new_table = gsub("CZ032", "CZ-PL", new_table)
new_table = gsub("CZ031", "CZ-JC", new_table)
new_table = gsub("CZ052", "CZ-KR", new_table)
new_table = gsub("CZ071", "CZ-OL", new_table)
new_table = gsub("CZ053", "CZ-PA", new_table)
new_table = gsub("CZ080", "CZ-MO", new_table)
new_table = gsub("CZ064", "CZ-JM", new_table)
new_table = gsub("CZ072", "CZ-ZL", new_table)
new_table = gsub("CZ063", "CZ-VY", new_table)
new_table = gsub("CZ020", "CZ-ST", new_table)
new_table = gsub("CZ010", "CZ-PR", new_table)

# Uložení upraveného textu do souboru a odstranění přechodných tabulek/proměnných table
writeLines(new_table, "upravena_tabulka.csv") # Uloží do nového souboru
rm(table)
rm(new_table)

my_data = read.csv( #Načtení dat z nového souboru
  file = "kapacity-intenzivni-pece-vlna-2-upravene.csv",
  header = TRUE,
  sep = ",",
  colClasses = "character"
)

my_data[, "datum"] = as.Date(
  my_data[, "datum"], # Nastavení proměnné jako datum
  format = "%Y-%m-%d" 
) # class(my_data[,1]) # Spustit pouze pro kontrolu

my_data[, "sledovana_hodnota"] = as.numeric( # Nastavení/Ujištění se, že je hodnota numeric
  my_data[, "sledovana_hodnota"]
)

my_final_data = my_data[my_data$datum == my_date, c("nazev_kraje", "sledovana_hodnota")] # Výběr finálních dat pro graf



# Načtení geografických dat pro Českou republiku a kraje
world = ne_countries(scale = "medium", returnclass = "sf")
czechia = world[world$name == "Czech Republic", ]
kraje = ne_states(country = "Czech Republic", returnclass = "sf")

# Funkce pro nahrazení hodnoty v tabulce kraje, je nutné kvůli funkci aes(), která bere v potaz pouze hodnoty v původní načtené tabulce, proto byl vybrán sloupec s nepotřebnými údaji a po párování za pomoci zkratky jsou data nahrazena
change_value = function(iso_desired, diss_new) {
  kraje$diss_me[kraje$iso_3166_2 == iso_desired] <<- diss_new
}

# Postupná kontrola textových řetězců a nahrazení hodnot
for (i in 1:nrow(my_final_data)) {
  iso_desired = my_final_data$nazev_kraje[i]
  my_value = my_final_data$sledovana_hodnota[i]
  
  if (iso_desired %in% kraje$iso_3166_2) {
    diss_new = my_value
    change_value(iso_desired, diss_new)
  }
}

# Tvorba popisku do grafu podle sledované hodnoty
legend = gsub("_", " ", my_variable) 

# Tvorba názvu do nadpisu a názvu výsledného souboru
main_title = paste("Kapacity intenzivní péče 2. vlna COVID-19, ČR kraje", format(as.Date(my_date), "%d. %B %Y"))

# Vytvořit mapový objekt s vybarvenými kraji
map = ggplot() +
  geom_sf(data = czechia, fill = "lightgray", color = "black") +
  geom_sf(data = kraje, aes(fill = diss_me), color = "black") +  
  scale_fill_viridis_c(legend) +
  coord_sf(xlim = c(12, 19), ylim = c(48, 51.5)) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  labs(x = "Zeměpisná délka", y = "Zeměpisná šířka") +
  ggtitle(main_title) +
  theme(plot.margin = margin(10, 10, 10, 10))

# Zobrazit mapu
print(map)

# Uložit mapu do souboru PNG
ggsave(paste(main_title, legend, ".png"), plot = map)