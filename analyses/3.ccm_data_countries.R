##dowload the data


#################DONT RUN THIS AGAIN, THE DF IS SAVED#################################33

# Downlad Species national indices from Brlik et al. (2021)

df <- data.table::fread('https://zenodo.org/record/4590199/files/national_indices2017.csv?download=1')

# Pass from wide to long format

df <- reshape2::melt(df, id.vars=c("species","euring_code","scheme","type"))
df <- maditr::dcast(df, species+euring_code+scheme+variable~type, fun.aggregate = sum)
df <- df[,c("euring_code","species","scheme","variable","index","se")]
names(df) <- c("Code","Species","CountryGroup","Year","Index","Index_SE")
df <- droplevels(na.omit(df))
df$Species <- as.factor(df$Species)
df$CountryGroup <- as.factor(df$CountryGroup)
df$Year <- as.numeric(as.character(df$Year))



# Load data from the EU Bird Directive Reporting (see Supplementary material for more details)

abd <- data.table::setDT(read.table("data/Abundance_data_PECBMS.txt", header = T, sep="\t")) 

#preparing the data

# Get species and coutry names

S <- levels(df$Species)
C <- levels(df$CountryGroup)

# Update species names

diff_name <- merge(data.frame(sp=levels(as.factor(abd$Species)),num=1),data.frame(sp=levels(as.factor(df$Species)),num2=2), by="sp",all=T)
diff_name

df$Species <- as.character(df$Species)
df$Species[df$Species=="Carduelis cannabina"] <- "Linaria cannabina"
df$Species[df$Species=="Carduelis chloris"] <- "Chloris chloris"
df$Species[df$Species=="Carduelis flammea"] <- "Acanthis flammea"
df$Species[df$Species=="Carduelis spinus"] <- "Spinus spinus"
df$Species[df$Species=="Corvus corone+cornix"] <- "Corvus corone"
df$Species[df$Species=="Delichon urbica"] <- "Delichon urbicum"
df$Species[df$Species=="Dendrocopos medius"] <- "Dendrocoptes medius"
df$Species[df$Species=="Dendrocopos minor"] <- "Dryobates minor"
df$Species[df$Species=="Hippolais pallida"] <- "Iduna pallida"
df$Species[df$Species=="Hirundo daurica"] <- "Cecropis daurica"
df$Species[df$Species=="Hirundo rupestris"] <- "Ptyonoprogne rupestris"
df$Species[df$Species=="Miliaria calandra"] <- "Emberiza calandra"
df$Species[df$Species=="Parus ater"] <- "Periparus ater"
df$Species[df$Species=="Parus caeruleus"] <- "Cyanistes caeruleus"
df$Species[df$Species=="Parus cristatus"] <- "Lophophanes cristatus"
df$Species[df$Species=="Parus montanus"] <- "Poecile montanus"
df$Species[df$Species=="Parus palustris"] <- "Poecile palustris"
df$Species[df$Species=="Saxicola torquata"] <- "Saxicola torquatus"
df$Species[df$Species=="Serinus citrinella"] <- "Carduelis citrinella"
df$Species[df$Species=="Tetrao tetrix"] <- "Lyrurus tetrix"
df$Species <- as.factor(df$Species)

##merge relative abundances


# Generate abundance estimates (for a given year)

for (i in 1:nrow(abd)){
  abd[i,estimate := round((EnvStats::geoMean(c(Count_min,Count_max),na.rm=T))*2)]
}

# Create sub-estimates for Belgium and Germany regions
# assuming populations to be uniformely distributed across these countries
# frac corresponds to the fraction surface of the region

subd <- data.table::data.table(reg = c("Belgium-Brussels", "Belgium-Wallonia", "Germany East", "Germany West"), 
                   frac = c(161/30528, 16901/30528, 108333/357022, 248577/357022))
Country_subd <- abd[Country=="Belgium" | Country=="Germany"][rep(1:340,each=2)]
Country_subd[,Country := rep(subd[,reg],170)]

# estimating pop size from the percentage of total area of the country covered by the region

for (i in 1:4){
  Country_subd[Country==subd[,reg][i],Count_min := Count_min*subd[,frac][i]]
  Country_subd[Country==subd[,reg][i],Count_max := Count_max*subd[,frac][i]]
}
for (i in 1:680){
  Country_subd[i,estimate := round((EnvStats::geoMean(c(Count_min,Count_max),na.rm=T))*2)]
}

# adding Belgium and Germany regions estimates to the main abundance data.frame

abd <- rbind(abd,Country_subd)
data.table::setorder(abd,Species,Country)

# removing unrealised species-country combinations

abd <- abd[is.na(Count_min) == FALSE]

# Convertion [Relative => Absolute] abundance time series

for(i in 1:nrow(subd)){
  df$Index[which(df$CountryGroup==subd$reg[i])] <- df$Index[which(df$CountryGroup==subd$reg[i])]*subd$frac[i]
  df$Index_SE[which(df$CountryGroup==subd$reg[i])] <- df$Index_SE[which(df$CountryGroup==subd$reg[i])]*subd$frac[i]
}

df_belge <-data.table::data.table(droplevels(subset(df, CountryGroup %in% c("Belgium-Brussels","Belgium-Wallonia"))) |>
                       dplyr::group_by(Code, Species, Year) |> dplyr::summarize(Index=sum(Index),Index_SE=sum(Index_SE)))
                       
df_belge <- data.table::data.table(df_belge[,1:2],CountryGroup=rep("Belgium",nrow(df_belge)),df_belge[,3:5])

df_germany <- data.table::data.table(droplevels(subset(df, CountryGroup %in% c("Germany East","Germany West"))) |>
                       dplyr::group_by(Code, Species, Year) |> dplyr::summarize(Index=sum(Index),Index_SE=sum(Index_SE)))

df_germany <- data.table::data.table(df_germany[,1:2],CountryGroup=rep("Germany",nrow(df_germany)),df_germany[,3:5])

df <- rbind(droplevels(subset(df, !(CountryGroup %in% c("Belgium-Brussels","Belgium-Wallonia","Germany East","Germany West")))),df_belge, df_germany)

df[,start_year := min(Year), by=c("Species","CountryGroup")]
df[,end_year := max(Year), by=c("Species","CountryGroup")]

for (i in levels(df$Species)){

  for (j in levels(df$CountryGroup)){
  
    # defining the reference year for the computation of a weighing factor
    
    if (dim(df[Species==i & CountryGroup==j])[1]!=0){
    
      # if the last year of absolute abundance estimate falls within the abundance time span
      # the abs. abund. ref year matches that of the time series
      
      if (abd[Species==i & Country==j, Year_end] <= df[Species==i & CountryGroup==j, end_year][1] &
          abd[Species==i & Country==j, Year_end] >= df[Species==i & CountryGroup==j, start_year][1]){
        Yref <- abd[Species==i & Country==j, Year_end]
        popsize <- "ok"
      }
      
      # if the last year of absolute abundance estimate falls after the abundance time span
      # the abs. abund. ref year is the last of the time series
      
      if (abd[Species==i & Country==j, Year_end] > df[Species==i & CountryGroup==j, end_year][1]){
        Yref <- df[Species==i & CountryGroup==j, end_year][1]
        popsize <- "later"
      }
      
      # if the last year of absolute abundance estimate falls before the abundance time span
      # the abs. abund. ref year is the first of the time series
      
      if (abd[Species==i & Country==j, Year_end] < df[Species==i & CountryGroup==j, start_year][1]){
        Yref <- df[Species==i & CountryGroup==j, start_year][1]
        popsize <- "earlier"
      }
      
      # computation of a weighing factor for the ref year
      
      WF <- abd[Species==i & Country==j, estimate]/df[Species==i & CountryGroup==j & Year==Yref, Index]
      
      # convertion using the weighing factor
      
      df[Species==i & CountryGroup==j, Abd := round(Index*WF)]
      df[Species==i & CountryGroup==j, SE_Abd := round(Index_SE*WF)]
      df[Species==i & CountryGroup==j, ref_year := Yref]
      df[Species==i & CountryGroup==j, estimation := popsize]
    }
  }
}

# particular case of collapsed populations

df[Species=="Galerida cristata" & CountryGroup=="Czech Republic",c("Abd","SE_Abd") := 0]

# Merge index and abundance

df_pop <- droplevels(subset(df, Year %in% c(1980:2016)))
df_pop <- droplevels(subset(df_pop, Species %in% levels(droplevels(subset(df_pop, start_year<=1981))$Species)))
df_pop <- droplevels(df_pop[!which(df_pop$Species=="Passer domesticus" & df_pop$CountryGroup %in% levels(df_pop$CountryGroup)[23]),])


#### IM GONNA EXPORT THE DF AS STAN DOES APPEAR TO DO IT. 

utils::write.csv(df, "./data/df_clean_birds.csv")

#######################################################################################33333333
#START HERE
#################################################################33

# Select time-series
df <- readr::read_csv("data/df_clean_birds.csv")
country_data <- readr::read_csv("data/country_data.csv")


country_data_urb <- data.frame(year=2009:2016,country_data[1:8,])
country_data_urb[country_data_urb==0]<-NA
country_data_urb2 <- reshape2::melt(country_data_urb, id.vars="year")
names(country_data_urb2)[3] <- "urb"

country_data_temp <- data.frame(year=2007:2016,country_data[68:77,])
country_data_temp2 <- reshape2::melt(country_data_temp, id.vars="year")
names(country_data_temp2)[3] <- "temp"

country_data_hico <- data.frame(year=2007:2016,country_data[103:112,])
country_data_hico[country_data_hico==0]<-NA
country_data_hico2 <- reshape2::melt(country_data_hico, id.vars="year")
names(country_data_hico2)[3] <- "hico"

country_data_forest <- data.frame(year=c(2007:2016),country_data[c(133:142),])
country_data_forest2 <- reshape2::melt(country_data_forest, id.vars="year")
names(country_data_forest2)[3] <- "forest"

# Group

country_data_press <- merge(country_data_temp2, country_data_urb2, by=c("variable","year"),all.x=T)
country_data_press <- merge(country_data_press, country_data_hico2, by=c("variable","year"))
country_data_press <- merge(country_data_press, country_data_forest2, by=c("variable","year"))

df_press <- as.data.frame(df)

press <- country_data_press
press$country <- as.character(press$variable)
press$variable <- NULL
press$country[press$country=="Czech.Republic"] <- "Czech Republic"
press$country[press$country=="UK"] <- "United Kingdom"
press$country[press$country=="Ireland"] <- "Republic of Ireland"
df_press2 <- merge(press, df_press[,c("Species","CountryGroup","Year","Index","Index_SE","Abd")], by.x=c("country","year"), by.y=c("CountryGroup","Year"), all.x=T)
df_press2 <- droplevels(df_press2[which(df_press2$Index >= df_press2$Index_SE),])

# Specify country or species with not enough data (at least 5 year with data, an non null index over the period and a change in pressure through time)

to_remove <- data.frame(na.omit(df_press2) |> dplyr::group_by(Species, country) |> dplyr::summarize(count=dplyr::n()))
df_press3 <- merge(df_press2,to_remove, by=c("Species","country"))
to_remove2 <- data.frame(na.omit(df_press2) |> dplyr::group_by(Species, country) |> dplyr::summarize(sum_ab=sum(Index)))
df_press3 <- merge(df_press3,to_remove2, by=c("Species","country"))
df_press3 <- df_press3[order(df_press3$Species, df_press3$country, df_press3$year),]

# Detrend when needed  #THIS IS RELEVANT FOR MY TIME SERIES< SEE THE DETREND

## now, this is new, Im gonna have only two cpuntires


df_press4 <- data.frame(droplevels(df_press3[df_press3$count > 4 & df_press3$sum_ab > 20 & df_press3$country!="Luxembourg",]) |> dplyr::group_by(Species, country) |> dplyr::mutate(temp_std=detrend_data(temp), urb_std=detrend_data(urb), hico_std=detrend_data(hico), forest_std=detrend_data(forest), Index_std=detrend_data(Index), Abd_std=detrend_data(Abd)))
