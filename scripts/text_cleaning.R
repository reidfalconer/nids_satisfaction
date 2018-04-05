# METADATA ====
# Description: Text cleaning 
# Created: 2018-04-03 (Reid Falconer)
# Updated: 2018-04-03 (Reid Falconer)
# Reviewed: 

# SUMMARY: 

df$happier <- str_replace_all(df$happier, fixed(" "), "_")
df$theft <- str_replace_all(df$theft, fixed(" "), "_")
df$watersource <- str_replace_all(df$watersource, fixed(" "), "_")
df$watersource <- str_replace_all(df$watersource, fixed("/"), "_")
df$watersource <- str_replace_all(df$watersource, fixed("-"), "") 
df$watersource <- str_replace_all(df$watersource, fixed("(tap)"), "")
df$toilet <- str_replace_all(df$toilet, fixed(" "), "_")
df$landline <- str_replace_all(df$landline, fixed(" "), "_")
df$landline <- str_replace_all(df$landline, fixed("-"), "")
df$dc <- str_replace_all(df$dc, fixed(" "), "_")
df$area <- str_replace_all(df$area, fixed(" "), "_")
df$province_birth <- str_replace_all(df$province_birth, fixed(" "), "_")
df$hopeful <- str_replace_all(df$hopeful, fixed(" "), "_")
df$hopeful <- str_replace_all(df$hopeful, fixed("_-_(3-4_days)"), "")
df$hopeful <- str_replace_all(df$hopeful, fixed("_-_(5-7_days)"), "")
df$hopeful <- str_replace_all(df$hopeful, fixed("_-_(less_than_1_day)"), "")
df$hopeful <- str_replace_all(df$hopeful, fixed("_-_(1-2_days)"), "")
df$religion <- str_replace_all(df$religion, fixed(" "), "_")
df$health_status <- str_replace_all(df$health_status, fixed(" "), "_")
df$roofs <- str_replace_all(df$roofs, fixed(" "), "_")
df$roofs <- str_replace_all(df$roofs, fixed("/"), "_")
df$roofs <- str_replace_all(df$roofs, fixed("-"), "") 
df$walls <- str_replace_all(df$walls, fixed(" "), "_")
df$walls <- str_replace_all(df$walls, fixed("/"), "_")
df$walls <- str_replace_all(df$walls, fixed("-"), "") 
df$marital <- str_replace_all(df$marital, fixed("-"), "") 
df$marital <- str_replace_all(df$marital, fixed(" "), "_") 
df$marital <- str_replace_all(df$marital, fixed("/"), "") 
df$rank1 <- str_replace_all(df$rank1, fixed(" "), "_") 
df$rank1 <- str_replace_all(df$rank1, fixed(":"), "") 
df$rank2 <- str_replace_all(df$rank2, fixed(" "), "_") 
df$rank2 <- str_replace_all(df$rank2, fixed(":"), "") 
df$rank3 <- str_replace_all(df$rank3, fixed(" "), "_") 
df$rank3 <- str_replace_all(df$rank3, fixed(":"), "") 
df$rank4 <- str_replace_all(df$rank4, fixed(" "), "_") 
df$rank4 <- str_replace_all(df$rank4, fixed(":"), "") 



