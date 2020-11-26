library(tidyverse)
library(acid)
library(ggtext)
library(glue)

rm(list=ls())

A<-data.frame(matrix(nrow = 5*20, ncol = 15))
colnames(A)<-c("Renta","Noms","CCAA",2007:2018)
A[,1]<-rep(c("Renta de Mercado","Gini 1.1","Gini 1.2","Renta Bruta","Renta Disponible"),20)
A[,2]<-rep(c("Gini 1.0: Renta de Mercado","Gini 1.1: Renta Mercado+Transf. (no desempleo/pensiones)","Gini 1.2: 1.1+Desempleo","Gini 2: Renta Bruta (1.2+Pensiones)","Gini 3: Renta Disponible (Renta Bruta-Impuestos Directos)"),20)

for(i in 8:19){
  
  R<-read_csv(paste("esudb",i,"r.csv", sep=""))
  H<-read_csv(paste("esudb",i,"h.csv", sep=""))
  D<-read_csv(paste("esudb",i,"d.csv", sep=""))
  P<-read_csv(paste("esudb",i,"p.csv", sep=""))
  
  
  #Creamos una nueva variable a partir del identificador de persona
  R<-R %>%
    mutate(HB030=RB030%/%100)
  
  #juntamos los ficheros R, H y D
  
  RHD <- R %>%
    left_join(y=H, by = "HB030")
  
  D<-D %>%
    rename(HB030 = `DB030`)
  
  
  RHD<- RHD%>%
    left_join(y=D, by= "HB030")
  
  
  RHD<-RHD %>%
    mutate(IngresEquivalent = vhRentaa/HX240)
  
  
  #Ara juntam P i H
  P<-P %>%
    mutate(HB030=PB030%/%100)
  
  PH <- P %>%
    left_join(y=H, by = "HB030")
  
  PH<-PH %>%
    mutate(IngresEquivalent=vhRentaa/HX240)
  
  
  #Limpiamos
  rm(list = setdiff(ls(), c("RHD", "PH", "A","i")))
  
  
  # Calculos ----------------------------------------------------------------
  Adultos<-PH%>%mutate(filaU=1)%>%group_by(HB030)%>%summarise(Adultos=sum(filaU))
  PH <- PH %>%
    left_join(y=Adultos, by = "HB030")
  
  
  VariablesGini<-PH%>%dplyr::select(IdentificadorHogar=HB030,
                                    IdentificadorPersona=PB030,
                                    Adultos,
                                    RentaDisponibleHogarINE=HY020,
                                    IngresEquivalent,
                                    SalarioBruto=PY010G,
                                    SalarioEspecie=PY021G,
                                    Autonomos=PY050G,
                                    BInversiones=HY090G,
                                    BAlquiler=HY040G,
                                    PensionesPrivadas=PY080G,
                                    Rentamenores=HY110G,
                                    TransfJubilacion=PY100G,
                                    TransfSupervivencia=PY110G,
                                    AyudaNinnos=HY050G,
                                    AsistenciaSocial=HY060G,
                                    AyudaVivir=HY070G,
                                    TransfDesempleo=PY090G,
                                    TransfInvalidez=PY130G,
                                    TransfEstudios=PY140G,
                                    TransfEnfermedad=PY120G,
                                    TransfOtrosHogares=HY080G,
                                    IPatrimonio=HY120G,
                                    IRPFCotizaciones=HY140G,
                                    AOtrosHogares=HY130G)
  
  VariablesGini<-VariablesGini%>%mutate(Rentademercado=SalarioBruto+SalarioEspecie+Autonomos+(BInversiones/Adultos)+(BAlquiler/Adultos)+PensionesPrivadas+(Rentamenores/Adultos),
                                        RentaBrutaSinPensiones=Rentademercado+(AyudaNinnos/Adultos)+(AsistenciaSocial/Adultos)+(AyudaVivir/Adultos)+TransfEnfermedad+TransfInvalidez+TransfEstudios+TransfDesempleo+(TransfOtrosHogares/Adultos),
                                        RentaBrutaSinPensionesniDesempleo=Rentademercado+(AyudaNinnos/Adultos)+(AsistenciaSocial/Adultos)+(AyudaVivir/Adultos)+TransfEnfermedad+TransfInvalidez+TransfEstudios+(TransfOtrosHogares/Adultos),
                                        RentaBruta=Rentademercado+(TransfJubilacion+TransfSupervivencia)+(AyudaNinnos/Adultos)+(AsistenciaSocial/Adultos)+(AyudaVivir/Adultos)+TransfEnfermedad+TransfInvalidez+TransfEstudios+TransfDesempleo+(TransfOtrosHogares/Adultos),
                                        Rentadisponible=RentaBruta-((IPatrimonio+IRPFCotizaciones+AOtrosHogares)/Adultos))
  
  
  VariablesHogGini<-VariablesGini%>%group_by(IdentificadorHogar)%>%summarise(RentaDisponible=sum(Rentadisponible),
                                                                             RentaBrutaSinPensiones=sum(RentaBrutaSinPensiones),
                                                                             RentaBrutaSinPensionesniDesempleo=sum(RentaBrutaSinPensionesniDesempleo),
                                                                             RentaMercado=sum(Rentademercado),
                                                                             RentaBruta=sum(RentaBruta))
  
  R1<-RHD%>%dplyr::select(Ponderacio=RB050,
                          IdentificadorHogar=HB030,
                          IngresEquivalentINE=IngresEquivalent,
                          vhRentaa,
                          HX240,
                          CCAA=DB040)
  Gini <- R1 %>%
    left_join(y=VariablesHogGini, by = "IdentificadorHogar")
  Gini$CCAA<-factor(Gini$CCAA)
  
  #Para cada CCAA calculamos todos los Ginis para aquel año
  for(j in 1:length(levels(Gini$CCAA))){
  
    GiniA<-Gini%>%filter(CCAA==levels(Gini$CCAA)[[j]])%>%mutate(IngresEquivalentDisponible= RentaDisponible/HX240,
                      IngresEquivalentMercado=RentaMercado/HX240,
                      IngresEquivalentBruto=RentaBruta/HX240,
                      IngresEquivalentBrutoSinPensiones=RentaBrutaSinPensiones/HX240,
                      IngresEquivalentBrutoSinPensionesniDesempleo=RentaBrutaSinPensionesniDesempleo/HX240)
  
  
  
  A[j*5,(i-4)]<-weighted.gini(GiniA$IngresEquivalentDisponible, w = GiniA$Ponderacio)[[1]]
  A[j*5-4,i-4]<-weighted.gini(GiniA$IngresEquivalentMercado, w = GiniA$Ponderacio)[[1]]
  A[j*5-1,i-4]<-weighted.gini(GiniA$IngresEquivalentBruto, w = GiniA$Ponderacio)[[1]]
  A[j*5-2,i-4]<-weighted.gini(GiniA$IngresEquivalentBrutoSinPensiones, w = GiniA$Ponderacio)[[1]]
  A[j*5-3,i-4]<-weighted.gini(GiniA$IngresEquivalentBrutoSinPensionesniDesempleo, w = GiniA$Ponderacio)[[1]]
  
  for(k in 0:4){
  A[j*5-k,3]<-levels(Gini$CCAA)[[j]]}
  }
  
  #Calculamos para Esp
  GiniE<-Gini%>%mutate(IngresEquivalentDisponible= RentaDisponible/HX240,
                       IngresEquivalentMercado=RentaMercado/HX240,
                       IngresEquivalentBruto=RentaBruta/HX240,
                       IngresEquivalentBrutoSinPensiones=RentaBrutaSinPensiones/HX240,
                       IngresEquivalentBrutoSinPensionesniDesempleo=RentaBrutaSinPensionesniDesempleo/HX240)
  
  
  
  A[100,(i-4)]<-weighted.gini(GiniE$IngresEquivalentDisponible, w = GiniE$Ponderacio)[[1]]
  A[96,i-4]<-weighted.gini(GiniE$IngresEquivalentMercado, w = GiniE$Ponderacio)[[1]]
  A[99,i-4]<-weighted.gini(GiniE$IngresEquivalentBruto, w = GiniE$Ponderacio)[[1]]
  A[98,i-4]<-weighted.gini(GiniE$IngresEquivalentBrutoSinPensiones, w = GiniE$Ponderacio)[[1]]
  A[97,i-4]<-weighted.gini(GiniE$IngresEquivalentBrutoSinPensionesniDesempleo, w = GiniE$Ponderacio)[[1]]
  
  for(k in 96:100){
    A[k,3]<-"España"}
}


A<-A%>%mutate(CCAA=fct_collapse(CCAA,
                             "Galicia"="ES11",
                             "Asturias"="ES12",
                             "Cantabria"="ES13",
                             "País Vasco"="ES21",
                             "Navarra"="ES22",
                             "La Rioja"="ES23",
                             "Aragón"="ES24",
                             "Madrid"="ES30",
                             "Castilla y León"="ES41",
                             "Castilla La Mancha"="ES42",
                             "Extremadura"="ES43",
                             "Cataluña"="ES51",
                             "C. Valenciana"="ES52",
                             "Illes Balears"="ES53",
                             "Andalucía"="ES61",
                             "Murcia"="ES62",
                             "Ceuta"="ES63",
                             "Melilla"="ES64",
                             "Canaries"="ES70"))
B<-A%>%filter(!is.na(CCAA))
B<-B%>%gather("Anys","Valors",4:15)

B<-B%>%mutate(Color=ifelse(CCAA=="España",1,0))
B$Color<-factor(B$Color)
save(B,file="www/B.Rda")



