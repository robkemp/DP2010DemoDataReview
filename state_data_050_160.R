library(readr)
library(tidyverse)

### Pop Variables ###

long_050=read_csv("long_dp1_050.csv")
long_160=read_csv("long_dp1_160.csv")

long_050_wa=long_050%>%
  mutate(statefips=str_sub(gisjoin, 2,3))%>%
  filter(statefips=="53")%>%
  select(-statefips)

long_160_wa=long_160%>%
  mutate(statefips=str_sub(gisjoin, 2,3))%>%
  filter(statefips=="53")%>%
  select(-statefips)

long_050_160_wa=bind_rows(long_160_wa, long_050_wa)

write_csv(long_050_160_wa, "long_dp1_050and160_WA.csv")

## Housing Variable ##
######Note: does overwrite variables from above to keep memory needs lower.

long_050=read_csv("long_dp14_050.csv")
long_160=read_csv("long_dp14_160.csv")


long_050_wa=long_050%>%
  mutate(statefips=str_sub(gisjoin, 2,3))%>%
  filter(statefips=="53")%>%
  select(-statefips)

long_160_wa=long_160%>%
  mutate(statefips=str_sub(gisjoin, 2,3))%>%
  filter(statefips=="53")%>%
  select(-statefips)

long_050_160_wa=bind_rows(long_160_wa, long_050_wa)

write_csv(long_050_160_wa, "long_dp14_050and160_WA.csv")

### Names ####
wa_names=(long_050_160_wa$name_dp)
write.csv(wa_names, "PlaceAndCountyNames_DP2010DemonstrationProducts_WA.csv", row.names = FALSE)
