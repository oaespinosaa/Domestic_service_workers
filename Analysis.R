# Libraries and clear workspace
library(tidyverse)
library(readxl)
rm(list = ls());gc()

#................................ Read data ............................... ####
# Deflation
IPC <- read_excel("Data/IPC_Indices.xlsx")
Anno_base <- 2023
ipc_anual <- IPC %>% pivot_longer(cols = -Mes,names_to = "year", values_to = "IPC") %>% 
  filter(!is.na(IPC)) %>% group_by(year) %>% 
  summarise(IPC = mean(IPC),.groups = 'drop') %>%
  mutate(IPC_base = IPC[year == Anno_base]) %>%
  mutate(Factor_multiplicacion = IPC_base/IPC, 
         year = as.numeric(year)) %>% select(year,Factor_multiplicacion)

# Regions
regions_ER <- c("Amazon-Orinoquía","Bogotá-Cundinamarca",
                "Caribbean","Central","Oriental","Pacific")
tabreg <- data.frame(REGION = c('Amazonia_Orinoquia','Bogota_Cundinamarca','Caribe',
                                'Central','Oriental','Pacifico'),
                     region = regions_ER)

regions <- read_excel("Data/Regiones.xlsx") %>% 
  group_by(COD_DPTO,NAME_DPTO,REGION) %>% summarise(.groups = 'drop') %>%
  left_join(tabreg,by = 'REGION') %>% select(-REGION)

# GEIH 2014-2023
geih <- readRDS("Data/GEIH_2014_2023.rds") %>% select(-Factor_multiplicacion) %>%
  left_join(ipc_anual,by = 'year') %>%
  mutate(P6500_defl = as.numeric(P6500)*Factor_multiplicacion,
         P6750_defl = as.numeric(P6750)*Factor_multiplicacion,
         P550_defl = as.numeric(P550)*Factor_multiplicacion,
         age_group = cut(as.numeric(P6040),breaks = c(15,27,60,100),right = F,
                         labels = c('15-26','27-59','>=60')),
         COD_DPTO = as.numeric(DPTO),
         P6080m = ifelse(is.na(P6080),9,ifelse(P6080 == 4,5,P6080))) %>%
  left_join(regions,by = c('COD_DPTO'))
labs_geo <- c(paste0(regions_ER[-1],'/Urban'),paste0(regions_ER[1],'/Urban'),
              paste0(regions_ER[-1],'/Rural'))

# Domestic service
# P6430: 3. Domestic worker
# CIIU: 9700 activities of private households as employers of domestic staff
geih_serv <- geih %>% filter(P6430 == 3|RAMA4D_R4 == '9700')

#............................ End read data ............................... ####

#................................ Results ................................. ####
lista_excel <- list()
# -----------------------------------| Number | --------------------------------
geih_serv %>% group_by(year) %>% summarise(n = sum(FEX_C),.groups = 'drop')
# ---------------------------------- | Sex | -----------------------------------
(lista_excel$Sex <-
   geih_serv %>% group_by(year,region,CLASE,P6020) %>% summarise(n = sum(FEX_C),.groups = 'drop') %>% 
   left_join(geih_serv %>% group_by(year,region,CLASE) %>% 
               summarise(tot = sum(FEX_C),.groups = 'drop')) %>% 
   bind_rows(geih_serv %>% group_by(year,P6020) %>% summarise(n = sum(FEX_C),region = 'National',.groups = 'drop') %>% 
               left_join(geih_serv %>% group_by(year) %>% 
                           summarise(tot = sum(FEX_C),region = 'National',.groups = 'drop'))) %>%
   mutate(prop = n/tot*100,zona = ifelse(CLASE == 1,'Urban','Rural'),
          P6020 = factor(P6020,labels = c('Male','Female'))) %>% 
   select(year,region,zona,P6020,prop) %>% arrange(year,region,zona) %>% 
   pivot_wider(names_from = year,values_from = prop) %>%
   arrange(region,zona,P6020))
# ----------------------------- | Age group| -----------------------------------
geih_serv %>% group_by(year,region,CLASE,age_group) %>% 
  summarise(n = sum(FEX_C),.groups = 'drop') %>% 
  left_join(geih_serv %>% group_by(year,region,CLASE) %>% 
              summarise(tot = sum(FEX_C),.groups = 'drop')) %>% 
  bind_rows(geih_serv %>% group_by(year,age_group) %>% 
              summarise(n = sum(FEX_C),region = 'National',.groups = 'drop') %>% 
              left_join(geih_serv %>% group_by(year) %>% 
                          summarise(tot = sum(FEX_C),region = 'National',.groups = 'drop'))) %>%
  mutate(prop = n/tot*100,zona = ifelse(CLASE == 1,'Urban','Rural'),
         label = ifelse(region == 'National','National',paste0(region,'/',zona)),
         label = factor(label,levels = c('National',labs_geo)),
         age_group = factor(age_group,levels = c('15-26','27-59','>=60'),
                            labels = c('15-26','27-59','≥60'))) %>%
  ggplot(aes(x = year,y = prop,fill = age_group)) + facet_wrap(label~.,scales = 'free_y',nrow = 2)+
  geom_col()+ scale_x_continuous(breaks = 2014:2023) +
  labs(x = 'Year',y = 'Distribution of domestic service workers (%)',fill = '') +
  theme_minimal(base_size = 18) + theme(legend.position = 'bottom',
                                        axis.text.x = element_text(angle = 90,vjust = 0.3),
                                        strip.text = element_text(size = 14))
# ggsave('../Resultados/Age_group_ENG.png',width = 50,height = 20,units = 'cm')

# -------------------------- | Social security| --------------------------------
# P6100: to which of the following social health security regimes are you affiliated?
# P6920: are you currently contributing to a pension fund?
# P6990: Are you affiliated with a professional risk insurance company (ARP) through 
# a company or individually (for work accidents, occupational diseases, etc.)?
pregsSS <- c('health','FP','ARL')
for (ss in pregsSS){
  if (ss == 'health'){geih_ss <- geih_serv %>% filter(P6100!=9,!is.na(P6100))}
  if (ss == 'FP'){geih_ss <- geih_serv %>% filter(P6920!=2,!is.na(P6920))}
  if (ss == 'ARL'){geih_ss <- geih_serv %>% filter(P6990==1)}
  lista_excel[[paste0('SS_',ss)]] <-
    geih_ss %>% group_by(year,region,CLASE) %>% summarise(n = sum(FEX_C),.groups = 'drop') %>% 
    left_join(geih_serv %>% group_by(year,region,CLASE) %>% 
                summarise(tot = sum(FEX_C),.groups = 'drop')) %>% 
    bind_rows(geih_ss %>% group_by(year) %>% summarise(n = sum(FEX_C),region = 'National',.groups = 'drop') %>% 
                left_join(geih_serv %>% group_by(year) %>% 
                            summarise(tot = sum(FEX_C),region = 'National',.groups = 'drop'))) %>%
    mutate(prop = n/tot*100,zona = ifelse(CLASE == 1,'Urban','Rural')) %>% 
    select(year,region,zona,prop) %>% 
    pivot_wider(names_from = year,values_from = prop,values_fill = 0) %>%
    arrange(region,zona)
}  
# ----------------------------- | Informality| ---------------------------------
geih_ie <- geih_serv %>% filter(IE == 1)
geih_ie %>% group_by(year,region,CLASE) %>% summarise(n = sum(FEX_C),.groups = 'drop') %>% 
  left_join(geih_serv %>% group_by(year,region,CLASE) %>% 
              summarise(tot = sum(FEX_C),.groups = 'drop')) %>% 
  bind_rows(geih_ie %>% group_by(year) %>% summarise(n = sum(FEX_C),region = 'National',.groups = 'drop') %>% 
              left_join(geih_serv %>% group_by(year) %>% 
                          summarise(tot = sum(FEX_C),region = 'National',.groups = 'drop'))) %>%
  mutate(prop = n/tot*100,zona = ifelse(CLASE == 1,'Urban','Rural'),
         region = factor(region,levels = c('National',regions_ER)),
         zona = ifelse(region == 'National','National',zona),
         zona = factor(zona,levels = c('Urban','Rural','National'))) %>%
  ggplot(aes(x = year,y = prop,fill = zona)) +
  facet_wrap(region~.,nrow = 2)+
  geom_col(position = 'dodge')+ scale_x_continuous(breaks = 2014:2023) +
  labs(x = 'Year',y = 'Informal domestic service workers (%)',fill = '') +
  theme_minimal(base_size = 18) + theme(legend.position = 'bottom',
                                        axis.text.x = element_text(angle = 90,vjust = 0.2),
                                        strip.text = element_text(size = 14))
# ggsave('../Resultados/Informality_ENG.png',width = 50,height = 20,units = 'cm')
# ---------------------------- | Education | -----------------------------------
geih_serv %>% group_by(year,region,CLASE,P6210m) %>% 
  summarise(n = sum(FEX_C),.groups = 'drop') %>% 
  left_join(geih_serv %>% group_by(year,region,CLASE) %>% 
              summarise(tot = sum(FEX_C),.groups = 'drop')) %>% 
  bind_rows(geih_serv %>% group_by(year,P6210m) %>% 
              summarise(n = sum(FEX_C),region = 'National',.groups = 'drop') %>% 
              left_join(geih_serv %>% group_by(year) %>% 
                          summarise(tot = sum(FEX_C),region = 'National',.groups = 'drop'))) %>%
  mutate(prop = n/tot*100,zona = ifelse(CLASE == 1,'Urban','Rural'),
         P6210m = factor(P6210m,labels = c('None or\npreschool','Primary','Secondary',
                                           'Upper\nsecondary','Higher or\nuniversity')),
         label = ifelse(region == 'National','National',paste0(region,'/',zona)),
         label = factor(label,levels = c('National',labs_geo))) %>%
  ggplot(aes(x = year,y = prop,fill = P6210m)) + facet_wrap(label~.,scales = 'free_y',nrow = 2)+
  geom_col()+ scale_x_continuous(breaks = 2014:2023) +
  labs(x = 'Year',y = 'Distribution of domestic service workers (%)',fill = '') +
  theme_minimal(base_size = 18) + theme(legend.position = 'bottom',
                                        axis.text.x = element_text(angle = 90,vjust = 0.3),
                                        strip.text = element_text(size = 14))
# ggsave('../Resultados/Education_ENG.png',width = 50,height = 20,units = 'cm')
# ------------------------------- | Income | -----------------------------------
# P6500: Before deductions, how much did … earn last month in this job? 
# Monthly value COP (include tips and commission and exclude per diem and in-kind payments)
geih_ing <- geih_serv %>% mutate(rango_ingreso = cut(P6500_defl,
                                                     breaks = c(0,5e5,1e6,1.5e6,5e6),right = F,
                                                     labels = c(1:4)),
                                 rango_ingreso = ifelse(is.na(rango_ingreso),5,rango_ingreso))

geih_ing %>% group_by(year,region,CLASE,rango_ingreso) %>% summarise(n = sum(FEX_C),.groups = 'drop') %>% 
  left_join(geih_ing %>% group_by(year,region,CLASE) %>% 
              summarise(tot = sum(FEX_C),.groups = 'drop')) %>% 
  bind_rows(geih_ing %>% group_by(year,rango_ingreso) %>% summarise(n = sum(FEX_C),region = 'National',.groups = 'drop') %>% 
              left_join(geih_serv %>% group_by(year) %>% 
                          summarise(tot = sum(FEX_C),region = 'National',.groups = 'drop'))) %>%
  mutate(prop = n/tot*100,zona = ifelse(CLASE == 1,'Urban','Rural'),
         rango_ingreso = factor(rango_ingreso,labels = c('< COP 500 thousand',
                                                         'COP 500 thousand - < 1 million',
                                                         'COP 1 - < 1.5 millions','≥ COP 1.5 millions','Not report'))) %>%
  mutate(label = ifelse(region == 'National','National',paste0(region,'/',zona)),
         label = factor(label,levels = c('National',labs_geo))) %>%
  ggplot(aes(x = year,y = prop,fill = rango_ingreso)) + facet_wrap(label~.,scales = 'free_y',nrow = 2)+
  geom_col()+ scale_x_continuous(breaks = 2014:2023) +
  labs(x = 'Year',y = 'Distribution of domestic service workers (%)',fill = '') +
  theme_minimal(base_size = 18) + theme(legend.position = 'bottom',
                                        axis.text.x = element_text(angle = 90,vjust = 0.3),
                                        strip.text = element_text(size = 14))
# ggsave('../Resultados/Income_ENG.png',width = 50,height = 20,units = 'cm')

geih_serv %>% filter(!is.na(P6500_defl)) %>% mutate(zona = ifelse(CLASE == 1,'Urban','Rural')) %>%
  bind_rows(geih_serv %>% filter(!is.na(P6500_defl)) %>% mutate(region = 'National')) %>%
  mutate(label = ifelse(region == 'National','National',paste0(region,'/',zona)),
         label = factor(label,levels = c('National',labs_geo))) %>%
  group_by(year,label) %>% mutate(n = sum(FEX_C)) %>% ungroup %>%
  mutate(vm = P6500_defl*FEX_C/n) %>% group_by(year,label) %>%
  mutate(promw = sum(vm)) %>% ungroup %>% mutate(year = factor(year)) %>%
  ggplot(aes(x = P6500_defl/1e6,col = year)) + 
  facet_wrap(label~.,scales = 'free_y',nrow = 2) + geom_density() + 
  geom_point(aes(x = promw/1e6,y = 0)) +
  # geom_vline(aes(xintercept = promw/1e6,col = year)) +
  labs(x = 'Income (in millions COP)',y = 'Density',col = '') +
  theme_minimal(base_size = 18) + theme(legend.position = 'bottom',
                                        strip.text = element_text(size = 14))
# ggsave('../Resultados/Income_Density_ENG.png',width = 50,height = 20,units = 'cm')

# -------------------------- | House services| ---------------------------------
# P4030S1: Electricity
# P4030S2: Natural gas
# P4030S3: Sewerage
# P4030S4: Garbage collection
# P4030S5: Water supply
pregs = paste0('P4030S',1:5)
for (p in pregs){
  geih_hs <- geih_serv %>% filter_at(vars(p),all_vars(.== 1))
  
  lista_excel[[paste0('HS_',p)]] <- geih_hs %>% group_by(year,region,CLASE) %>% summarise(n = sum(FEX_C),.groups = 'drop') %>% 
    left_join(geih_serv %>% group_by(year,region,CLASE) %>% 
                summarise(tot = sum(FEX_C),.groups = 'drop')) %>% 
    bind_rows(geih_hs %>% group_by(year) %>% summarise(n = sum(FEX_C),region = 'National',.groups = 'drop') %>% 
                left_join(geih_serv %>% group_by(year) %>% 
                            summarise(tot = sum(FEX_C),region = 'National',.groups = 'drop'))) %>%
    mutate(prop = n/tot*100,zona = ifelse(CLASE == 1,'Urban','Rural')) %>% 
    select(year,region,zona,prop) %>% 
    pivot_wider(names_from = year,values_from = prop,values_fill = 0) %>%
    arrange(region,zona)
}

# ------------------------------ | Housing | -----------------------------------
geih_serv %>% group_by(year,region,CLASE,P5090m) %>% 
  summarise(n = sum(FEX_C),.groups = 'drop') %>% 
  left_join(geih_serv %>% group_by(year,region,CLASE) %>% 
              summarise(tot = sum(FEX_C),.groups = 'drop')) %>% 
  bind_rows(geih_serv %>% group_by(year,P5090m) %>% 
              summarise(n = sum(FEX_C),region = 'National',.groups = 'drop') %>% 
              left_join(geih_serv %>% group_by(year) %>% 
                          summarise(tot = sum(FEX_C),region = 'National',.groups = 'drop'))) %>%
  mutate(prop = n/tot*100,zona = ifelse(CLASE == 1,'Urban','Rural'),
         P5090m = factor(P5090m,labels=c('Owned','Rented','Usufruct','Other')),
         label = ifelse(region == 'National','National',paste0(region,'/',zona)),
         label = factor(label,levels = c('National',labs_geo))) %>%
  ggplot(aes(x = year,y = prop,fill = P5090m)) + facet_wrap(label~.,scales = 'free_y',nrow = 2)+
  geom_col()+ scale_x_continuous(breaks = 2014:2023) +
  labs(x = 'Year',y = 'Distribution of domestic service workers (%)',fill = '') +
  theme_minimal(base_size = 18) + theme(legend.position = 'bottom',
                                        axis.text.x = element_text(angle = 90,vjust = 0.3),
                                        strip.text = element_text(size = 14))
# ggsave('../Resultados/Housing_ENG.png',width = 50,height = 20,units = 'cm')
# ----------------------------- | Children | -----------------------------------
geih_serv %>% group_by(year,region,CLASE,cnchild) %>% 
  summarise(n = sum(FEX_C),.groups = 'drop') %>% 
  left_join(geih_serv %>% group_by(year,region,CLASE) %>% 
              summarise(tot = sum(FEX_C),.groups = 'drop')) %>% 
  bind_rows(geih_serv %>% group_by(year,cnchild) %>% 
              summarise(n = sum(FEX_C),region = 'National',.groups = 'drop') %>% 
              left_join(geih_serv %>% group_by(year) %>% 
                          summarise(tot = sum(FEX_C),region = 'National',.groups = 'drop'))) %>%
  mutate(prop = n/tot*100,zona = ifelse(CLASE == 1,'Urban','Rural'),
         cnchild = factor(ifelse(is.na(cnchild),'0',cnchild),levels = c(0:3,'>=4'),
                          labels = c(0:3,'≥4')),
         label = ifelse(region == 'National','National',paste0(region,'/',zona)),
         label = factor(label,levels = c('National',labs_geo))) %>%
  ggplot(aes(x = year,y = prop,fill = cnchild)) + facet_wrap(label~.,scales = 'free_y',nrow = 2)+
  geom_col()+ scale_x_continuous(breaks = 2014:2023) +
  labs(x = 'Year',y = 'Distribution of domestic service workers (%)',fill = '') +
  theme_minimal(base_size = 18) + theme(legend.position = 'bottom',
                                        axis.text.x = element_text(angle = 90,vjust = 0.3),
                                        strip.text = element_text(size = 14))
# ggsave('../Resultados/Children_ENG.png',width = 50,height = 20,units = 'cm')
# ------------------------------- | Status | -----------------------------------
geih_serv %>% group_by(year,region,CLASE,P6070m) %>% 
  summarise(n = sum(FEX_C),.groups = 'drop') %>% 
  left_join(geih_serv %>% group_by(year,region,CLASE) %>% 
              summarise(tot = sum(FEX_C),.groups = 'drop')) %>% 
  bind_rows(geih_serv %>% group_by(year,P6070m) %>% 
              summarise(n = sum(FEX_C),region = 'National',.groups = 'drop') %>% 
              left_join(geih_serv %>% group_by(year) %>% 
                          summarise(tot = sum(FEX_C),region = 'National',.groups = 'drop'))) %>%
  mutate(prop = n/tot*100,zona = ifelse(CLASE == 1,'Urban','Rural'),
         P6070m = factor(P6070m,labels=c('Living with a partner','Married','Separated','Single','Not report')),
         label = ifelse(region == 'National','National',paste0(region,'/',zona)),
         label = factor(label,levels = c('National',labs_geo))) %>%
  ggplot(aes(x = year,y = prop,fill = P6070m)) + facet_wrap(label~.,scales = 'free_y',nrow = 2)+
  geom_col()+ scale_x_continuous(breaks = 2014:2023) +
  labs(x = 'Year',y = 'Distribution of domestic service workers (%)',fill = '') +
  theme_minimal(base_size = 18) + theme(legend.position = 'bottom',
                                        axis.text.x = element_text(angle = 90,vjust = 0.3),
                                        strip.text = element_text(size = 14))
# ggsave('../Resultados/Status_ENG.png',width = 50,height = 20,units = 'cm')

# ------------------------------- | Ethnic | -----------------------------------
(lista_excel$Ethnic <- geih_serv %>% filter(year >= 2021) %>%
   group_by(year,region,CLASE,P6080m) %>% 
   summarise(n = sum(FEX_C),.groups = 'drop') %>% 
   left_join(geih_serv %>% group_by(year,region,CLASE) %>% 
               summarise(tot = sum(FEX_C),.groups = 'drop')) %>% 
   bind_rows(geih_serv %>% filter(year >= 2021) %>% group_by(year,P6080m) %>% 
               summarise(n = sum(FEX_C),region = 'National',.groups = 'drop') %>% 
               left_join(geih_serv %>% group_by(year) %>% 
                           summarise(tot = sum(FEX_C),region = 'National',.groups = 'drop'))) %>%
   mutate(prop = n/tot*100,zona = ifelse(CLASE == 1,'Urban','Rural'),
          P6080m = factor(P6080m,labels = c('Indigenous','Romani','Raizal','Afro-colombian or Palenquero','Mestizo'))) %>%
   select(year,region,zona,P6080m,prop) %>% 
   pivot_wider(names_from = year,values_from = prop,values_fill = 0) %>%
   arrange(region,zona,P6080m))

# Results ####
# list_excel
#............................ End results ................................. ####