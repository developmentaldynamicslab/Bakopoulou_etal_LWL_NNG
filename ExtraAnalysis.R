### Extra Analysis 
### Will need to run until line 352 from the Complete_Analysis script 


etdata3_model_whole$window <- ifelse(etdata3_model_whole$Timestamp %in%  -4000:0, "Before",
                                     ifelse(etdata3_model_whole$Timestamp %in% 300:89760, "After",0))

etdata3_model_whole <-etdata3_model_whole %>%
  filter(window != 0)

LookMaterial <- etdata3_model_whole %>% 
  dplyr::group_by(ID,Utrial,OCDI_2_10,window,Gender) %>% 
  filter(Look_Material == TRUE) %>%
  summarise(TotalLookMaterial= ((max(Timestamp)-min(Timestamp))))%>% 
  ungroup()

LookShape <- etdata3_model_whole %>% 
  dplyr::group_by(ID,Utrial,OCDI_2_10,window,Gender) %>% 
  filter(Look_Shape == TRUE) %>%
  summarise(TotalLookShape= ((max(Timestamp)-min(Timestamp))))%>% 
  ungroup()

LookExemplar <- etdata3_model_whole %>% 
  dplyr::group_by(ID,Utrial,OCDI_2_10,window,Gender) %>% 
  filter(Look_Up == TRUE) %>%
  summarise(TotalLookExemplar= ((max(Timestamp)-min(Timestamp))))%>% 
  ungroup()

Look_dataTotals <- left_join(LookExemplar, LookShape , by = c("ID","Gender","OCDI_2_10","Utrial","window"), keep = FALSE) %>% left_join(., LookMaterial , by=c("ID","Gender","OCDI_2_10","Utrial","window")) 

Look_dataTotals[is.na(Look_dataTotals)] <- 0

Look_dataTotals$TotalLook <- Look_dataTotals$TotalLookExemplar+Look_dataTotals$TotalLookShape+Look_dataTotals$TotalLookMaterial
