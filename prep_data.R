# siehe "renv.lock" für Versionen
library("here")
library("tidyverse")
library("rvest")

# Kandidatendaten (Landeswahlleiter) ----
kandi <- read_csv2(here("data_in", "LW22_Bewerber.csv")) %>%
  select(wkreis_nr = `Wahlkreisnummer`,
         partei = `Partei/Einzelbewerber`,
         titel = Titel,
         nname = Nachname,
         vname = Vorname,
         geb_jahr = `Geburtsjahr`,
         ges = `Geschlecht (1 = männlich - 2 = weiblich)`,
         beruf = `Beruf`,
         wohnort = `Wohnort`,
         listenplatz = `Landeslistenplatz`) %>%
  mutate(ges = fct_recode(as.character(ges), "m" = "1", "w" = "2"),
         wkreis_nr = as.numeric(str_replace(wkreis_nr, "-", "")),
         listenplatz = as.numeric(str_replace(listenplatz, "-", "")),
         alter = 2022 - geb_jahr,
         alter_kat = factor(case_when(
           between(alter, 15, 24) ~ "15-24 J.",
           between(alter, 25, 34) ~ "25-34 J.",
           between(alter, 35, 44) ~ "35-44 J.",
           between(alter, 45, 54) ~ "45-54 J.",
           between(alter, 55, 64) ~ "55-64 J.",
           between(alter, 65, 74) ~ "65-74 J.",
           alter >= 75 ~ "75 J. und älter",
           TRUE ~ NA_character_
         ), levels = c("15-24 J.", "25-34 J.", "35-44 J.", "45-54 J.", "55-64 J.",
                       "65-74 J.", "75 J. und älter")),
         name = paste(vname, nname),
         name = case_when(
           name == "Markus Herbert Weske" ~ "Markus Weske",
           name == "Christos Katzidis" ~ "Christos Georg Katzidis",
           TRUE ~ name)) %>%
  filter(partei %in% c("CDU", "SPD", "FDP", "AfD", "GRÜNE", "DIE LINKE"))

# Wahlergebnis (Landeswahlleiter) ----
url <- "https://www.wahlergebnisse.nrw/landtagswahlen/2022/index.shtml"
html <- read_html(url)

# extrahiere Information über Direktwahlsieger für 128 Wahlkreise
get_winner <- function(x){
  win <- html %>%
    html_elements(paste0("#wk", x)) %>%
    html_children() %>%
    html_attr("class")
  
  df <- tibble(wkreis_nr = x, winner = win)
}

direkt_gewinner <- lapply(1:128, get_winner) %>%
  bind_rows() %>%
  mutate(partei = str_sub(winner, start = 8),
         partei = str_to_upper(partei),
         partei = if_else(partei == "GRUENE", "GRÜNE", partei),
         wahl = "Direkt") %>%
  select(-winner)

# Anzahl Sitze insgesamt nach vorl. Endergebnis
sitze_liste <- tibble(partei = c("CDU", "SPD", "GRÜNE", "FDP", "AfD"),
                      sitze = c(77, 56, 39, 12, 12)) %>%
  # von Anzahl Gesamtsitze werden Sitze durch Direktmandate abgezogen
  mutate(sitze = if_else(partei == "CDU",
                         sitze - nrow(filter(direkt_gewinner, partei == "CDU")),
                         sitze),
         sitze = if_else(partei == "SPD",
                         sitze - nrow(filter(direkt_gewinner, partei == "SPD")),
                         sitze),
         sitze = if_else(partei == "GRÜNE",
                         sitze - nrow(filter(direkt_gewinner, partei == "GRÜNE")),
                         sitze))

# gewählte Listenkandidaten (nach Abzug Direktkandidaten)
liste_gewinner <- kandi %>%
  select(wkreis_nr, partei, listenplatz) %>%
  anti_join(direkt_gewinner, by = c("wkreis_nr", "partei")) %>%
  left_join(sitze_liste, by = "partei") %>%
  arrange(partei, listenplatz) %>%
  group_by(partei) %>%
  mutate(wahl = if_else(row_number() <= sitze, "Liste", NA_character_)) %>%
  filter(!is.na(wahl)) %>%
  select(-sitze, -wkreis_nr)

# Landtag ab 2017 (Wikipedia) ----
url <- "https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Landtages_Nordrhein-Westfalen_(17._Wahlperiode)"
html <- read_html(url) 
html %>%
  html_nodes(css = "table")

MdL_2017 <- html %>%
  html_nodes(css = "table") %>%
  nth(5) %>%
  html_table() %>%
  select(
    name = `Mitglied des Landtages`,
    geb_jahr = `geb.`,
    wkreis_2017_bez = Landtagswahlkreis,
    erststimm_perc = `Erststimmen in %`) %>%
  mutate(MdL_2017 = "Ja",
         erststimm_perc = as.numeric(str_replace(erststimm_perc, ",", ".")),
         stimme = factor(if_else(wkreis_2017_bez == "Landesliste",
                                 "zweitstimme",
                                 "erststimme")),
         wkreis_2017_bez = if_else(wkreis_2017_bez == "Landesliste",
                                   NA_character_,
                                   wkreis_2017_bez))

# kombiniere Datensätze ----
kandi_landtag <- kandi %>%
  left_join(MdL_2017, by = c("geb_jahr", "name")) %>%
  mutate(MdL_2017 = as_factor(if_else(is.na(MdL_2017), "Nein", MdL_2017))) %>%
  left_join(direkt_gewinner, by = c("wkreis_nr", "partei")) %>%
  left_join(liste_gewinner, by = c("partei", "listenplatz")) %>%
  mutate(
    wahl = factor(case_when(
      wahl.x == "Direkt" ~ "Direkt",
      wahl.y == "Liste" ~ "Liste",
      is.na(wahl.x) & is.na(wahl.y) ~ "nicht\nge-\nwählt"),
      levels = c("Direkt", "Liste", "nicht\nge-\nwählt")),
    partei = fct_relevel(partei, "CDU", "SPD", "FDP", "AfD", "GRÜNE", "DIE LINKE")) %>%
  select(-wahl.x, -wahl.y)

# speichere Datensatz (RDS + CSV)
saveRDS(kandi_landtag, here("results", "nrw_landtag_2022_kandidierende.rds"))
write_excel_csv2(kandi_landtag, here("results", "nrw_landtag_2022_kandidierende.csv"))

# Alter NRW-Bevölkerung, inkl. 15- bis 17-Jährigen (Statistische Ämter) ----
bev_alter <- read_csv2(here("data_in", "12411-04-02-4-B.csv"),
                       skip = 7, n_max = 79,
                       col_select = c(3, 4, 5, 6),
                       col_names = FALSE) %>%
  mutate(alter = if_else(X3 != "unter 1 Jahr",
                         as.numeric(str_extract(X3, "[1-9]+[0-9]?")),
                         0),
         alter_kat = factor(case_when(
           between(alter, 15, 24) ~ "15-24 J.",
           between(alter, 25, 34) ~ "25-34 J.",
           between(alter, 35, 44) ~ "35-44 J.",
           between(alter, 45, 54) ~ "45-54 J.",
           between(alter, 55, 64) ~ "55-64 J.",
           between(alter, 65, 74) ~ "65-74 J.",
           alter >= 75 ~ "75 J. und älter",
           TRUE ~ NA_character_
         ), levels = c("15-24 J.", "25-34 J.", "35-44 J.", "45-54 J.", "55-64 J.",
                       "65-74 J.", "75 J. und älter"))) %>%
  filter(!is.na(alter_kat)) %>%
  group_by(alter_kat) %>%
  summarize(absolut = sum(X4)) %>%
  ungroup() %>%
  mutate(anteil = absolut / sum(absolut)) %>%
  select(alter_kat, anteil)

# speichere Datensatz (RDS + CSV)
saveRDS(bev_alter, here("results", "nrw_bevoelkerung_alter_u18.rds"))
write_excel_csv2(bev_alter, here("results", "nrw_bevoelkerung_alter_u18.csv"))

# Alter NRW-Bevölkerung, ab 18 Jahren (Statistische Ämter) ----
bev_alter_ab18 <- read_csv2(here("data_in", "12411-04-02-4-B.csv"),
                       skip = 7, n_max = 79,
                       col_select = c(3, 4, 5, 6),
                       col_names = FALSE) %>%
  mutate(alter = if_else(X3 != "unter 1 Jahr",
                         as.numeric(str_extract(X3, "[1-9]+[0-9]?")),
                         0),
         alter_kat = factor(case_when(
           between(alter, 18, 24) ~ "18-24",
           between(alter, 25, 34) ~ "25-34",
           between(alter, 35, 44) ~ "35-44",
           between(alter, 45, 54) ~ "45-54",
           between(alter, 55, 64) ~ "55-64",
           alter >= 65 ~ "65+",
           TRUE ~ NA_character_
         ), levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))) %>%
  filter(!is.na(alter_kat)) %>%
  group_by(alter_kat) %>%
  summarize(absolut = sum(X4)) %>%
  ungroup() %>%
  mutate(anteil = absolut / sum(absolut)) %>%
  select(alter_kat, anteil)

# speichere Datensatz (RDS + CSV)
saveRDS(bev_alter_ab18, here("results", "nrw_bevoelkerung_alter_ab18.rds"))
write_excel_csv2(bev_alter_ab18, here("results", "nrw_bevoelkerung_alter_ab18.csv"))
