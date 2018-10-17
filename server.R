#---predict plotly-----
#library(mice) # imputation
library("ggplot2")
library("dplyr")
library("caret")
library("gridExtra")
#library(MASS)
library("leaps")
library("relaimpo")
library("mgcv")
library("randomForest")
library("rpart")
library("rpart.plot")
library("lattice")
library("plotly")
library("shiny")
library("graphics")
library("stats")

t1 <- read.csv("./data/MBTI.csv")

t_MBTI <- t1 %>%
  mutate(ENFJ = E.N.F.J*X, ENFP = E.N.F.P*X, ENTJ =  E.N.T.J*X,	ENTP=E.N.T.P*X,	ESFJ=E.S.F.J*X,	ESFP=E.S.F.P*X,
         ESTJ=E.S.T.J*X,	ESTP=E.S.T.P*X,	INFJ=I.N.F.J*X,	INFP=I.N.F.P*X,	INTJ=I.N.T.J*X,	
         INTP=I.N.T.P*X,	ISFJ= I.S.F.J*X,ISFP=I.S.F.P*X,	ISTJ=I.S.T.J*X,	ISTP=I.S.T.P*X) %>%
  select(X,ENFJ,ENFP,ENTJ,ENTP,ESFJ,ESFP,ESTJ,ESTP,INFJ,INFP,INTJ,INTP,ISFJ,ISFP,ISTJ,ISTP)#OK
#t_MBTI <- data.frame(t_MBTI)
#write.csv(t_MBTI,"D:/its-table/t_MBTI.csv")
#t_MB <- read.csv("D:/its-table/t_MBTI.csv")
#t_MB

#X ENFj     ENFP ENTJ ENTP ESFJ     ESFP ESTJ ESTP INFJ INFP INTJ INTP ISFJ ISFP     ISTJ ISTP
#1 1    0 0.000000    0    0    0 0.000000    0    0    0    0    0    0    0    0 2.771395    0

pl_MBTI <- plot_ly(data = t_MBTI, x = ~X, y = ~ENFJ, type = 'bar', name = 'ENFJ',
                   marker = list(color = 'blue')) %>%
  add_trace(y = ~ENFP, name = 'ENFP', marker = list(color = 'red')) %>%
  add_trace(y = ~ENTJ, name = 'ENTJ', marker = list(color = 'green')) %>%
  add_trace(y = ~ENTP, name = ' ESFJ', marker = list(color = 'brown')) %>%
  add_trace(y = ~ESFJ, name = 'ENFP', marker = list(color = 'orange')) %>%
  add_trace(y = ~ESFP , name = 'ESFP ', marker = list(color = 'grey')) %>%
  add_trace(y = ~ESTJ, name = 'ESTJ', marker = list(color = 'black')) %>%
  add_trace(y = ~ESTP, name = 'ESTP', marker = list(color = 'orange')) %>%
  add_trace(y = ~INFJ , name = 'INFJ ', marker = list(color = 'grey')) %>%
  add_trace(y = ~INFP, name = 'INFP', marker = list(color = 'darkblue')) %>%
  add_trace(y = ~INTJ, name = 'INTJ', marker = list(color = 'cyan')) %>%
  add_trace(y = ~INTP, name = 'INTP', marker = list(color = 'black')) %>%
  add_trace(y = ~ISFJ, name = 'ISFJ', marker = list(color = 'blue')) %>%
  add_trace(y = ~ISFP, name = 'ISFP', marker = list(color = 'black')) %>%
  add_trace(y = ~ISTJ, name = 'ISTJ', marker = list(color = 'blue')) %>%
  add_trace(y = ~ISTP, name = 'ISTP', marker = list(color = 'black')) %>%
  layout(title = 'MBTI Influence IPK',
         xaxis = list(
           title = 'IPK',
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')),
         yaxis = list(
           title = 'IP grade',
           titlefont = list(
             size = 16,
             color = 'rgb(107, 107, 107)'),
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')))
#legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', 
#              bordercolor = 'rgba(255, 255, 255, 0)'),
#barmode = 'group', bargap = 0.15, bargroupgap = 0.1)
pl_MBTI

#----1.Table data MBTI

t_t_MBTI <- t_MBTI[6:39,]
dim(t_t_MBTI)
#[1] 34 17

head(t_t_MBTI)
#          X     ENFJ ENFP     ENTJ ENTP     ESFJ     ESFP ESTJ     ESTP INFJ INFP INTJ INTP ISFJ ISFP ISTJ     ISTP
#6  3.009684 0.000000    0 0.000000    0 0.000000 3.009684    0 0.000000    0    0    0    0    0    0    0 0.000000
#7  3.010029 0.000000    0 3.010029    0 0.000000 0.000000    0 0.000000    0    0    0    0    0    0    0 0.000000
#8  3.023214 0.000000    0 0.000000    0 0.000000 0.000000    0 0.000000    0    0    0    0    0    0    0 3.023214
t_t_t_MBTI <- t_t_MBTI %>%
  summarise(ENFJ=sum(ENFJ)/3.4,ENFP=sum(ENFP)/3.4,ENTJ=sum(ENTJ)/3.4,ENTP=sum(ENTP)/3.4,ESFJ=sum(ESFJ)/3.4,ESFP=sum(ESFP)/3.4,
            ESTJ=sum(ESTJ)/3.4,ESTP=sum(ESTP)/3.4,INFJ=sum(INFJ)/3.4,INFP=sum(INFP)/3.4,INTJ=sum(INTJ)/3.4,
            INTP=sum(INTP)/3.4,ISFJ=sum(ISFJ)/3.4,ISFP=sum(ISFP)/3.4,ISTJ=sum(ISTJ)/3.4,ISTP=sum(ISTP)/3.4) 
#select(X,ENFJ,ENFP,ENTJ,ENTP,ESFJ,ESFP,ESTJ,ESTP,INFJ,INFP,INTJ,INTP,ISFJ,ISFP,ISTJ,ISTP)#OK
t_t_t_MBTI         
d_MBTI <- data.frame(X=c("ENFJ", "ENFP", "ENTJ", "ENTP",  "ESFJ", "ESFP", 
                         "ESTJ", "ESTP",  "INFJ",  "INFP",  "INTJ",  "INTP","ISFJ","ISFP","ISTJ","ISTP"),
                     MBTI=c(0.9062451, 3.863284, 0.8853025, 1.900008, 0.8962008, 1.792219, 2.857455, 1.844434, 
                            0.9512516, 1.831057, 0.9824825, 0.9878576, 2.846259, 0.9723115, 7.699002, 0.8891807))
d_MBTI
g_MBTI <- ggplot(d_MBTI,aes(x=X,y=MBTI))+
  geom_bar(stat="identity")+
  ggtitle("Prosentase pengaruh MBTI thd  IPK >= 3")+
  labs(y="Prosentase")+
  theme(axis.text.x = element_text(angle=90))
g_MBTI  

###---TOT N (sum of N1~N20)-----belum

#t_com <- table(rfl_predict, test$Competitive.Spirit)

#head(t_com)
#rfl_predict        0 2.5 5 7.5
#2.77139533333334 1   0 0   0
#2.906109         0   1 0   0

#write.csv(t_com,"D:/data-upload/Competitive.csv")
tc <- read.csv("./data/Competitive.csv")
tc1 <- tc %>%
  mutate(X0 = X0*X,  X2.5 = X2.5*X,  X5 = X5*X, X7.5 = X7.5*X) %>%
  select(X,X0,X2.5,X5,X7.5)
#write.csv(tc1,"D:/its-table/tc1.csv")
#tc12 <- read.csv("D:/its-table/tc1.csv")

pl_competitive <- plot_ly(data = tc1 , x = ~X, y = ~X0, type = 'bar', name = 'X0',
                          marker = list(color = 'blue')) %>%
  add_trace(y = ~X2.5, name = 'X2.5', marker = list(color = 'red')) %>%
  add_trace(y = ~X5, name = 'X5', marker = list(color = 'green')) %>%
  add_trace(y = ~X7.5, name = 'X7.5', marker = list(color = 'orange')) %>%
  layout(title = 'Competitive sepirit for IPK Influence ',
         xaxis = list(
           title = "",
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')),
         yaxis = list(
           title = 'IP grade',
           titlefont = list(
             size = 16,
             color = 'rgb(107, 107, 107)'),
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')))
#legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', 
#             bordercolor = 'rgba(255, 255, 255, 0)'),
#barmode = 'group', bargap = 0.5, bargroupgap = 0.1)
pl_competitive

###---2.Table Competitive  Spirit

t_CS <- tc1[6:39,]
dim(t_CS)
#[1] 34  5
head(t_CS)
#          X       X0     X2.5 X5     X7.5
#6  3.009684 0.000000 3.009684  0 0.000000
#7  3.010029 0.000000 0.000000  0 3.010029
#8  3.023214 0.000000 0.000000  0 3.023214
t_t_CS <- t_CS %>%
  summarise(X0=sum(X0)/3.4,  X2.5=sum(X2.5)/3.4, X=sum(X5)/3.4, X7.5=sum(X7.5)/3.4)

d_t_CS <- data.frame(X=c("X0", "X2.5", "X5", "X7.5"), Competitive.Spirit=c(1.835561, 13.11995, 10.58741, 6.561626))

g_CS <- ggplot(d_t_CS,aes(x=X,y=Competitive.Spirit))+
  geom_bar(stat="identity")+
  ggtitle("Prosentase pengaruh Competitive.Spirit thd IPK >= 3")+
  labs(y="Prosentase")
g_CS

###---Work Ethic
#table(rfl_predict, test$Work.Ethic #belum
#      table(rfl_predict, test$Creativity) #belum

###--3- Ambition
#t_amb <- table(rfl_predict, test$Ambition)
#write.csv(t_amb,"D:/data-upload/Amb.csv")
#t2a <- read.csv("./data/Amb.csv")

#         X X0 X2.5 X5 X7.5
#1 2.771395  0    1  0    0
#2 2.906109  1    0  0    0
#3 2.943185  0    0  1    0

#t2a1 <- t2a %>%
#  mutate(X0 = X0*X,  X2.5 = X2.5*X,  X5 = X5*X, X7.5 = X7.5*X) %>%
#  select(X,X0,X2.5,X5,X7.5)
#t2a1
#t2a1 <- data.frame(t2a1)
#---Pengaruh  Ambition thd IPK

#hist_amb <- tr2tbl %>%
# ggplot(aes(x=X))+
# geom_bar(stat = "identity")

#grafik ambition thd IPK, urut 1-39

#write.csv(t2a1,"D:/its-table/Ambition.csv")
tr2tbl <- read.csv("./data/Ambition.csv")

pl_ambition_tr2tbl <- plot_ly(tr2tbl , x = ~X, y = ~X0, type = 'bar', name = 'X0',
                              marker = list(color = 'blue')) %>%
  add_trace(y = ~X2.5, name = 'X2.5', marker = list(color = 'red')) %>%
  add_trace(y = ~X5, name = 'X5', marker = list(color = 'green')) %>%
  add_trace(y = ~X7.5, name = 'X7.5', marker = list(color = 'orange')) %>%
  layout(title = 'Ambition for IPK Influence ',
         xaxis = list(
           title = "",
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')),
         yaxis = list(
           title = 'IP grade',
           titlefont = list(
             size = 16,
             color = 'rgb(107, 107, 107)'),
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')))
#legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', 
#             bordercolor = 'rgba(255, 255, 255, 0)'),
#barmode = 'group', bargap = 0.5, bargroupgap = 0.1)
pl_ambition_tr2tbl


#-----Manage
#table(rfl_predict, test$Manage)# belum

####----4-Pilihan Jurusan

#test.pred.forest <- predict(rfl,test)
#t_pil <- table(rfl_predict, test$Pilihan.jur)

#write.csv(t_pil, "D:/data-upload/Pilihan.jur.csv")
tb2 <- read.csv("./data/Pilihan.jur.csv")

tb2_r <- tb2 %>%
  mutate(sppc=sesuai.pilihan.pertama.dan.cocok*X,spptc= sesuai.pilihan.pertama.tapi.tidak.cocok*X,
         bppc=bukan.pilihan.pertama.tapi.cocok*X,bpptc=bukan.pilihan.pertama.dan.tidak.cocok*X) %>%
  select(X,sppc,spptc,bppc,bpptc)
#head(tb2_r)

#write.csv(tb2_r,"D:/its-table/tb2_r.csv")
#tb_pil <- read.csv("D:/its-table/tb2_r.csv")
#head(tb_pil)
#  X     sppc    spptc     bppc bpptc
#1 1 0.000000 0.000000 3.107164     0
#2 2 3.358638 0.000000 0.000000     0

pl_piljur  <- plot_ly(data = tb2_r , x = ~X, y = ~sppc, type = 'bar', name = 'sesuai.pilihan.pertama.dan.cocok',
                      marker = list(color = 'blue')) %>%
  add_trace(y = ~spptc, name = 'sesuai.pilihan.pertama.tapi.tidak.cocok', marker = list(color = 'red')) %>%
  add_trace(y = ~bppc, name = 'bukan.pilihan.pertama.tapi.cocok', marker = list(color = 'green')) %>%
  add_trace(y = ~bpptc, name = 'bukan.pilihan.pertama.dan.tidak.cocok', marker = list(color = 'orange')) %>%
  layout(title = 'Pilihan jurusan Influence IPK',
         xaxis = list(
           title = "",
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')),
         yaxis = list(
           title = 'IP grade',
           titlefont = list(
             size = 16,
             color = 'rgb(107, 107, 107)'),
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')))
#legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)',
#              bordercolor = 'rgba(255, 255, 255, 0)'),
#barmode = 'group', bargap = 0.5, bargroupgap = 0.1)
pl_piljur

###---3.Table Pilihan Jurusan
t_PJ <- tb2_r[6:39,]
dim(t_PJ)
head(t_PJ)
head(t_PJ)
#X sppc    spptc     bppc    bpptc
#6  3.009684    0 0.000000 3.009684 0.000000
#7  3.010029    0 0.000000 3.010029 0.000000
#8  3.023214    0 0.000000 3.023214 0.000000
t_t_PJ <- t_PJ %>%
  summarise(sppc=sum(sppc)/3.4, spptc=sum(spptc)/3.4,  bppc=sum(bppc)/3.4, bpptc=sum(bpptc)/3.4)

d_t_t_PJ <- data.frame(X=c("Sesuai plilihan pertama dan cocok", 
                           "Sesuai pilihan pertama tapi tdk cocok", 
                           "Bukan pilihan pertama dan cocok", 
                           "Bukan pilihan pertama tapi tdk cocok"), 
                       y=c(15.29552, 3.670552, 12.23223, 0.9062451))

g_PJ <- ggplot(d_t_t_PJ,aes(x=X,y=y))+
  geom_bar(stat="identity")+
  ggtitle("Prosentase pengaruh Pil Jurusan thd IPK >= 3")+
  labs(y="Prosentase", X="Jalur Masuk")+
  theme(axis.text.x = element_text(angle=45))
g_PJ
#bukan pilihan pertama dan tidak cocok bukan pilihan pertama tapi cocok
#2.77139533333334                                     0                                1
#2.906109 0 0
#t_pil1 <- t_pil %>%
#  mutate(bpptc = bukan.pilihan.pertama.dan.tidak.cocok, bppc = bukan.pilihan.pertama.tapi.cocok,
#        sppc = sesuai.pilihan.pertama.dan.cocok, spptc = sesuai.pilihan.pertama.tapi.tidak.cocok)

#Analisa_piljur <- t_pil %>%
#  mutate(bpptc = sum(bukan.pilihan.pertama.dan.tidak.cocok),bppc = sum(bukan.pilihan.pertama.tapi.cocok),
#        sppc = sum(sesuai.pilihan.pertama.dan.cocok), spptc = sum(sesuai.pilihan.pertama.tapi.tidak.cocok)) %>%
# mutate(bpptc= bpptc/39, bppc = bppc/39, sppc = sppc/39, spptc = spptc/39 ) %>%


#----  Analisa Pilihan Jurusan mahasiswa
#1.bukan.pilihan.pertama.dan.tidak.cocok = 0.02564103
#2.bukan.pilihan.pertama.tapi.cocok = 0.3589744
#3.sesuai.pilihan.pertama.dan.cocok = 0.4615385
#4.sesuai.pilihan.pertama.tapi.tidak.cocok =  0.1538462

###----4-Masuk melalui jalur

#rfl_predict <- predict(rfl, test)
#t11a <- table(rfl_predict, test$Masuk.melalui.jalur)
#t12 <- write.csv(t11a , "D:/data-upload/Masuk.melalui.jalur.csv")
tbmskjlr <- read.csv("./data/Masuk.melalui.jalur.csv")

#write.csv( t_mskjlr, "D:/its-table/tbmskjlr.csv")
#tbmskjlr <- read.csv("D:/its-table/tbmskjlr.csv")

tbm1 <- tbmskjlr%>%
  mutate(Mandiri=Mandiri*X, SBMPTN=SBMPTN*X, SNMPTN=SNMPTN*X) %>%
  select(X,Mandiri,SBMPTN,SNMPTN)
#tbm1

#write.csv(tbm1,"D:/its-table/tbm11.csv")

#tbm11 <- read.csv("D:/its-table/tbm11.csv")

pl_jrlmsk  <- plot_ly(data = tbm1 , x = ~X, y = ~Mandiri, type = 'bar', 
                      name = 'Masuk melalui jlr Mandiri',
                      marker = list(color = 'blue')) %>%
  add_trace(y = ~SBMPTN, name = 'Masuk melalui SBMPTN', marker = list(color = 'orange')) %>%
  add_trace(y = ~SNMPTN, name = 'Masuk melalui SNMPTN', marker = list(color = 'black')) %>%
  layout(title = 'Pengaruh cara masuk PT thd IPK',
         xaxis = list(
           title = "",
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')),
         yaxis = list(
           title = 'IP grade',
           titlefont = list(
             size = 16,
             color = 'rgb(107, 107, 107)'),
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')))
#legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)',
#              bordercolor = 'rgba(255, 255, 255, 0)'),
#barmode = 'group', bargap = 0.5, bargroupgap = 0.1)
pl_jrlmsk

###---4.Table Masuk Melalui Jalur
t_MMJ <- tbm1[6:39,]
dim(t_MMJ)
head(t_MMJ)
t_MMJ
#X  Mandiri   SBMPTN   SNMPTN
#6  3.009684 0.000000 3.201899 0.000000
#7  3.010029 0.000000 2.771395 0.000000
#8  3.023214 0.000000 3.009684 0.000000
t_t_MMJ1 <- t_MMJ %>%
  summarise(Mandiri=sum(Mandiri)/3.4, SBMPTN=sum(SBMPTN)/3.4,  SNMPTN=sum( SNMPTN)/3.4)

d_t_t_MMJ1 <- data.frame(X=c("Mandiri", "SBMPTN" , "SNMPTN"), y=c(6.65994, 8.497831, 17.46023))
g_MMJ <- ggplot(d_t_t_MMJ1,aes(x=X,y=y))+
  geom_bar(stat="identity")+
  ggtitle("Prosentase pengaruh Jalur masuk thd IPK >= 3")+
  labs(y="Prosentase", X="Jalur Masuk")
g_MMJ


#---5-histogram Rencana.stl.lulus
#renc <- read.csv("D:/its/tt_1.csv")
#ren_hist <- renc %>%
#  select(IPK, Rencana.stl.lulus) %>%
#  group_by(Rencana.stl.lulus) 
#summarise(n = n()) %>%
#mutate(prop = n / sum(n)) 

#head(ren_hist)
# A tibble: 3 x 3
#Rencana.stl.lulus          n   prop
#  <fctr>                <int>  <dbl>
#1 Bekerja di perusahaan    88 0.682 
#2 Bekerja sendiri          11 0.0853
#3 Melanjutkan S2           30 0.233

#p_hist_rsll  <- ren_hist  %>%
#  ggplot(aes(x=Rencana.stl.lulus))+
#geom_bar()+
#  ggtitle("Rencana setelah lulus")+
#  theme(axis.text = element_text(angle=90))
#plot(p_hist_rsll)


###----Apakah pilihan jurusan berpengaruh thd IPK


###---Jalur Masuk PT: Mandiri,SBMPTN,SNMPTN

#p_jlrmsk <- tbm1 %>%
#  ggplot(aes(x=J))+
# geom_bar()+
#  ggtitle("Jalur masuk masuk, Mandiri,SBMPTN,SNMPTN")+
#  theme(axis.text = element_text(angle=90))

#----UJI terhadap IPK
#test.pred.forest <- predict(rfl,test)
#mskjlr <- table(rfl_predict, test$Masuk.melalui.jalur)
#write.csv(mskjlr, "D:/its-table/mskjlr.csv")
#mskjlr1 <- read.csv("D:/its-table/mskjlr.csv")
#t_mskjlr <- mskjlr1 %>%
#  mutate(Mandiri=Mandiri*rfl_predict, SBMPTN=SBMPTN*rfl_predict, SNMPTN=SNMPTN*rfl_predict ) %>%
#  select(Mandiri,SBMPTN,SNMPTN)

###----6-Rencana setelah lulus

#table(rfl_predict, test$Rencana.stl.lulus)

#rfl_predict        bukan pilihan pertama dan tidak cocok bukan pilihan pertama tapi cocok
#2.77139533333334                                     0                                1
#2.906109                                             0                                0
#rfl_predict <- predict(rfl, test)
#tb_ren <- table(rfl_predict, test$Rencana.stl.lulus)
#tbrenc <- write.csv(tb_ren, "D:/data-upload/Rencana.stl.lulus.csv")
tbrenc1 <- read.csv("./data/Rencana.stl.lulus.csv")
tbrenc1

tbr <- tbrenc1 %>%
  mutate(BP= Bekerja.di.perusahaan*X, BS= Bekerja.sendiri*X, S2=Melanjutkan.S2*X) %>%
  select(X,BP,BS,S2)

#write.csv(tb1, "D:/its-table/tbrenc.csv")
#tb_renc <- read.csv("D:/its-table/tbrenc.csv")

pl_renc  <- plot_ly(data = tbr , x = ~X, y = ~BP, type = 'bar', name = 'Bekerja.di.perusahaan',
                    marker = list(color = 'blue')) %>%
  add_trace(y = ~BS, name = 'Bekerja.sendiri', marker = list(color = 'orange')) %>%
  add_trace(y = ~S2, name = 'Melanjutkan S2', marker = list(color = 'black')) %>%
  layout(title = 'Pengaruh Rencana setelah lulus thd IPK',
         xaxis = list(
           title = "",
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')),
         yaxis = list(
           title = 'IP grade',
           titlefont = list(
             size = 16,
             color = 'rgb(107, 107, 107)'),
           tickfont = list(
             size = 14,
             color = 'rgb(107, 107, 107)')))
#legend = list(x = 0, y = 1, bgcolor = 'rgba(255, 255, 255, 0)', 
#              bordercolor = 'rgba(255, 255, 255, 0)'),
#barmode = 'group', bargap = 0.5, bargroupgap = 0.1)
pl_renc

###---5.Table Rencana setelah lulus

t_RSL <-tbr[6:39,]
dim(t_RSL)[1] 
#[1] 34  4

head(t_RSL)
head(t_RSL)
#          X       BP       BS       S2
#6  3.009684 3.009684 0.000000 0.000000
#7  3.010029 0.000000 3.010029 0.000000
#8  3.023214 3.023214 0.000000 0.000000
t_RSL1 <- t_RSL %>%
  summarise(BP=sum(BP)/3.4, BS=sum(BS)/3.4, S2=sum(S2)/3.4)

d_t_RSL1 <- data.frame(X=c("Bekerja di persh", "Bekerja Sendiri" , "Melanjutkan S2"), 
                       Rencana_STLL=c(22.75434, 2.687151, 6.663061))
g_RSL <- ggplot(d_t_RSL1,aes(x=X,y=Rencana_STLL))+
  geom_bar(stat="identity")+
  ggtitle("Prosentase pengaruh Rencana Stl Lulus thd IPK >= 3")+
  labs(y="Prosentase")+
  theme(axis.text.x = element_text(angle=90))
g_RSL
######---- Histogram Pilihan jurusan

#ipk_tt_1 <- read.csv("./data/tt_1.csv")
#ipk_1 <- ipk_tt_1 %>%
#  select(IPK, Pilihan.jur) %>%
# group_by(Pilihan.jur)

#p_hist_piljur <- ggplot(ipk_1, aes(x=Pilihan.jur))+
#  geom_bar()+
# ggtitle("Pilihan Jurusan")+
# theme(axis.text = element_text(angle=90))
#p_hist_piljur #-7-Histogram pilihan jurusan
######

###-----histogram Masuk.melalui.jalur

#tmskjl <- read.csv("./data/tt_1.csv")
#jlm <- tmskjl %>%
# select(IPK, Masuk.melalui.jalur) %>%
# group_by(Masuk.melalui.jalur)
#head(jlm)
#p_hist_mskjlr  <- jlm  %>%
#p_hist_mskjlr <- ggplot(jlm, aes(x=Masuk.melalui.jalur))+
#  geom_bar()+
#  ggtitle("Masuk Melalui Jalur: Mandiri,SBMPTN,SNMPTN")+
#  theme(axis.text = element_text(angle=90))
#p_hist_mskjlr     

####--3. Histogram Rencana setelah lulus
#renc <- read.csv("./data/tt_1.csv")
#ren_hist <- renc %>%
# select(IPK, Rencana.stl.lulus) %>%
# group_by(Rencana.stl.lulus)  
#summarise(n = n()) %>%
#mutate(prop = n / sum(n)) 
#p_hist_rsll <- ggplot(ren_hist,aes(x=Rencana.stl.lulus))+
# geom_bar()+
# ggtitle("Rencana setelah lulus")+
# theme(axis.text = element_text(angle=90))
#p_hist_rsll
###--3. Rencana_Setelah_Lulus

MBTI <- t_t_MBTI %>%
  select(X,ENFJ,ENFP,ENTJ,ENTP,ESFJ,ESFP,ESTJ,ESTP,INFJ,INFP,INTJ,INTP,ISFJ,ISFP,ISTJ,ISTP)
MBTI

Competitive_Spirit <- t_CS %>%
  select(X, X0, X2.5, X5, X7.5)
Competitive_Spirit

Pilihan_Jurusan <- t_PJ %>%
  select(X, sppc, spptc,  bppc, bpptc)
Pilihan_Jurusan

Masuk_Melalui_Jalur <- t_MMJ %>%
  select(X, Mandiri, SBMPTN, SNMPTN)
Masuk_Melalui_Jalur

Rencana_Setelah_Lulus <- t_RSL %>%
  select(X, BP, BS, S2)
Rencana_Setelah_Lulus 

server <- function(input, output) {
  output$trendPlot <- renderPlotly({
    if (input$plot_type == "MBTI vs IP") {
      pl_MBTI
    }else if (input$plot_type == "Competitive vs IP") {
      pl_competitive
    }else if (input$plot_type == "Ambition vs IP") {
      pl_ambition_tr2tbl
    }else if (input$plot_type == "Pilihan Jurusan VS IP") {
      pl_piljur
    }else if (input$plot_type == "Masuk Melalui Jalur vs IP") {
      pl_jrlmsk
    }else if (input$plot_type == "Rencana setelah lulus VS IP") {
      pl_renc
    }else if (input$plot_type == "Prediksi MBTI") {
      g_MBTI
    }else if (input$plot_type == "Prediksi Competitive Spirit") {
      g_CS
    }else if (input$plot_type == "Prediksi Pilihan Jurusan") {
      g_PJ
    } else if (input$plot_type == "Prediksi Masuk jalur") {
      g_MMJ
    }else if (input$plot_type == "Prediksi Rencana Setelah Lulus") {
      g_RSL
    }
  })
  
  output$MBTI <- renderDataTable({
    MBTI}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$Competitive_Spirit <- renderDataTable({
    Competitive_Spirit}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$Pilihan_Jurusan <- renderDataTable({
    Pilihan_Jurusan}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$Masuk_Melalui_Jalur <- renderDataTable({
    Masuk_Melalui_Jalur}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
  output$Rencana_Setelah_Lulus <- renderDataTable({
    Rencana_Setelah_Lulus}, options = list(aLengthMenu = c(5, 30, 50), iDisplayLength = 5))
}