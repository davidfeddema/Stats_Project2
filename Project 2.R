#PROBLEM 1
getdata = function(x) read.table(file=paste("http://www.umich.edu/~dnoll/BME503/",x,sep=""), header=T)
baseball = getdata("baseball.txt")
attach(baseball)
par(mfrow=c(2,2))
boxplot(c(baseball[2], baseball[3]), main = "2014 MLB Season Statistics Boxplot")
boxplot(c(baseball[4],baseball[5],baseball[9],baseball[15]), main = "2014 MLB Season Statistics Boxplot")
boxplot(c(baseball[6],baseball[8],baseball[11],baseball[13]), main = "2014 MLB Season Statistics Boxplot")
boxplot(c(baseball[7],baseball[10],baseball[12],baseball[14]), main = "2014 MLB Season Statistics Boxplot")
bb_corr = cor(baseball[,seq(2,15)])
write.csv(bb_corr, "corr_1.csv")

runs1.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+IBB+HBP+SF+SB+CS)
summary(runs1.lm)

qqnorm(residuals(runs1.lm), main="QQ Plot of Residuals")
qqline(residuals(runs1.lm))
qqnorm(baseball[,15], main="QQ Plot of Runs")
qqline(baseball[,15])


par(mfrow=c(3,4))
plot(baseball[,5], residuals(runs1.lm), ylab="Residuals", xlab="Singles",main="Singles Residuals")
abline(h=0, lty = 1, lwd = 3)
plot(baseball[,6], residuals(runs1.lm), ylab="Residuals", xlab="Doubles",main="Doubles Residuals")
abline(h=0, lty = 1, lwd = 3)
plot(baseball[,7], residuals(runs1.lm), ylab="Residuals", xlab="Triples",main="Triples Residuals")
abline(h=0, lty = 1, lwd = 3)
plot(baseball[,8], residuals(runs1.lm), ylab="Residuals", xlab="Home Runs",main="Home Runs Residuals")
abline(h=0, lty = 1, lwd = 3)
plot(baseball[,9], residuals(runs1.lm), ylab="Residuals", xlab="Unintentional Walks",main="Unintentional Walks Residuals")
abline(h=0, lty = 1, lwd = 3)
plot(baseball[,10], residuals(runs1.lm), ylab="Residuals", xlab="Intentional Walks",main="Intentional Walks Residuals")
abline(h=0, lty = 1, lwd = 3)
plot(baseball[,11], residuals(runs1.lm), ylab="Residuals", xlab="Hit by Pitch",main="Hit By Pitch Residuals")
abline(h=0, lty = 1, lwd = 3)
plot(baseball[,12], residuals(runs1.lm), ylab="Residuals", xlab="Sacrifice Flies",main="Sacrifice Flies Residuals")
abline(h=0, lty = 1, lwd = 3)
plot(baseball[,13], residuals(runs1.lm), ylab="Residuals", xlab="Stolen Bases",main="Stolen Bases Residuals")
abline(h=0, lty = 1, lwd = 3)
plot(baseball[,14], residuals(runs1.lm), ylab="Residuals", xlab="Caught Stealing",main="Caught Stealing Residuals")
abline(h=0, lty = 1, lwd = 3)
plot(baseball[,15], residuals(runs1.lm), ylab="Residuals", xlab="Runs",main="Runs Residuals")
abline(h=0, lty = 1, lwd = 3)



library(stats4)
BIC(runs1.lm)

runs2.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+IBB+HBP+SF+SB)
runs3.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+IBB+HBP+SF+CS)
runs4.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+IBB+HBP+SB+CS)
runs5.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+IBB+SF+SB+CS) #lowest BIC
runs6.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+HBP+SF+SB+CS)
runs7.lm = lm(R ~ 0+X1B+X2B+X3B+HR+IBB+HBP+SF+SB+CS)
runs8.lm = lm(R ~ 0+X1B+X2B+X3B+BB+IBB+HBP+SF+SB+CS)
runs9.lm = lm(R ~ 0+X1B+X2B+HR+BB+IBB+HBP+SF+SB+CS)
runs10.lm = lm(R ~ 0+X1B+X3B+HR+BB+IBB+HBP+SF+SB+CS)
runs11.lm = lm(R ~ 0+X2B+X3B+HR+BB+IBB+HBP+SF+SB+CS)
BIC(runs2.lm)
BIC(runs3.lm)
BIC(runs4.lm)
BIC(runs5.lm)
BIC(runs6.lm)
BIC(runs7.lm)
BIC(runs8.lm)
BIC(runs9.lm)
BIC(runs10.lm)
BIC(runs11.lm)

runs12.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+IBB+SF+SB) #took runs5 and removed another variable
runs13.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+IBB+SF+CS)
runs14.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+IBB+SB+CS)
runs15.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+SF+SB+CS) #lowest BIC
runs16.lm = lm(R ~ 0+X1B+X2B+X3B+HR+IBB+SF+SB+CS)
runs17.lm = lm(R ~ 0+X1B+X2B+X3B+BB+IBB+SF+SB+CS)
runs18.lm = lm(R ~ 0+X1B+X2B+HR+BB+IBB+SF+SB+CS)
runs19.lm = lm(R ~ 0+X1B+X3B+HR+BB+IBB+SF+SB+CS)
runs20.lm = lm(R ~ 0+X2B+X3B+HR+BB+IBB+SF+SB+CS)
BIC(runs12.lm)
BIC(runs13.lm)
BIC(runs14.lm)
BIC(runs15.lm)
BIC(runs16.lm)
BIC(runs17.lm)
BIC(runs18.lm)
BIC(runs19.lm)
BIC(runs20.lm)

runs21.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+SF+SB)#took runs15 and removed another variable
runs22.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+SF+CS)#lowest BIC
runs23.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+SB+CS)
runs24.lm = lm(R ~ 0+X1B+X2B+X3B+HR+SF+SB+CS)
runs25.lm = lm(R ~ 0+X1B+X2B+X3B+BB+SF+SB+CS)
runs26.lm = lm(R ~ 0+X1B+X2B+HR+BB+SF+SB+CS)
runs27.lm = lm(R ~ 0+X1B+X3B+HR+BB+SF+SB+CS)
runs28.lm = lm(R ~ 0+X2B+X3B+HR+BB+SF+SB+CS)
BIC(runs21.lm)
BIC(runs22.lm)
BIC(runs23.lm)
BIC(runs24.lm)
BIC(runs25.lm)
BIC(runs26.lm)
BIC(runs27.lm)
BIC(runs28.lm)

runs29.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+SF)#took runs22 and removed another variable
runs30.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB+CS)
runs31.lm = lm(R ~ 0+X1B+X2B+X3B+HR+SF+CS)
runs32.lm = lm(R ~ 0+X1B+X2B+X3B+BB+SF+CS)
runs33.lm = lm(R ~ 0+X1B+X2B+HR+BB+SF+CS)
runs34.lm = lm(R ~ 0+X1B+X3B+HR+BB+SF+CS)
runs35.lm = lm(R ~ 0+X2B+X3B+HR+BB+SF+CS)
BIC(runs29.lm)#lowest BIC
BIC(runs30.lm)
BIC(runs31.lm)
BIC(runs32.lm)
BIC(runs33.lm)
BIC(runs34.lm)
BIC(runs35.lm)

runs36.lm = lm(R ~ 0+X1B+X2B+X3B+HR+BB)#took runs29 and removed another variable
runs37.lm = lm(R ~ 0+X1B+X2B+X3B+HR+SF)
runs38.lm = lm(R ~ 0+X1B+X2B+X3B+BB+SF)
runs39.lm = lm(R ~ 0+X1B+X2B+HR+BB+SF)
runs40.lm = lm(R ~ 0+X1B+X3B+HR+BB+SF)
runs41.lm = lm(R ~ 0+X2B+X3B+HR+BB+SF)
BIC(runs36.lm)#lowest BIC
BIC(runs37.lm)
BIC(runs38.lm)
BIC(runs39.lm)
BIC(runs40.lm)
BIC(runs41.lm)

runs42.lm = lm(R ~ 0+X1B+X2B+X3B+HR)#took runs36 and removed another variable
runs43.lm = lm(R ~ 0+X1B+X2B+X3B+BB)
runs44.lm = lm(R ~ 0+X1B+X2B+HR+BB)
runs45.lm = lm(R ~ 0+X1B+X3B+HR+BB)
runs46.lm = lm(R ~ 0+X2B+X3B+HR+BB)
BIC(runs42.lm)
BIC(runs43.lm)
BIC(runs44.lm)
BIC(runs45.lm)
BIC(runs46.lm)

#runs36.lm has the lowest BIC overall (singles, doubles, triples, home runs, walks => just getting on base are main factors)
summary(runs36.lm)

step(lm(R ~ 0+X1B+X2B+X3B+HR+BB+IBB+HBP+SF+SB+CS), direction="backward")


#PROBLEM 2
getdata = function(x) read.table(file=paste("http://www.umich.edu/~dnoll/BME503/",x,sep=""), header=T)
players = getdata("players.txt")
attach(players)
players
RV = RBI + R
RR = RV/PA
players = cbind(players,RV)
players = cbind(players,RR)

Rp = 0.16037*X1B + 0.096242*X2B + 1.20950*X3B + 1.00580*HR + 0.15345*BB
players = cbind(players,Rp)

Runs.p = matrix(0,length(Name),2)
Runs.p[,1]=Name
Runs.p[,2]=Rp
write.csv(R, "R.csv")

rv.rr = matrix(0,length(Name),3)
rv.rr[,1]=playerid
rv.rr[,2]=RV
rv.rr[,3]=RR
colnames(rv.rr)=c('playerid','RV','RR')
rv.rr

rv.rr[order(rv.rr[,2],rv.rr[,1],rv.rr[,2],decreasing=TRUE),]
rv.rr[order(rv.rr[,3],rv.rr[,1],rv.rr[,2],decreasing=TRUE),]

TopRV = Name[match(c(10155,1744,1887,1908,2434,12861,4949,1177,6195,5038,4106), playerid)]
TopRR = Name[match(c(10155,2151,1744,4949,1887,1908,15676,393,13110,2434,12861), playerid)]
TopRV
TopRR


#PROBLEM 3
RV.aov = aov(RV ~ Position)
anova(RV.aov)

contrasts(players$Position) = c(-1,-1,-1,-1,-1,-1,8,-1,-1)
contrasts(players$Position) = contr.sum
Position1.con.lm = lm(RV ~ Position, data=players)
summary(Position1.con.lm)

pairwise.t.test(RV, Position, p.adj="bon")


#PROBLEM 4
League.AL = subset(players, players$League=="AL")
League.NL = subset(players, players$League=="NL")
League.AL.1B = subset(League.AL, League.AL$Position=="1B")
League.AL.2B = subset(League.AL, League.AL$Position=="2B")
League.AL.3B = subset(League.AL, League.AL$Position=="3B")
League.AL.SS = subset(League.AL, League.AL$Position=="SS")
League.AL.C = subset(League.AL, League.AL$Position=="C")
League.AL.LF = subset(League.AL, League.AL$Position=="LF")
League.AL.CF = subset(League.AL, League.AL$Position=="CF")
League.AL.RF = subset(League.AL, League.AL$Position=="RF")
League.AL.DH = subset(League.AL, League.AL$Position=="DH")
League.NL.1B = subset(League.NL, League.NL$Position=="1B")
League.NL.2B = subset(League.NL, League.NL$Position=="2B")
League.NL.3B = subset(League.NL, League.NL$Position=="3B")
League.NL.SS = subset(League.NL, League.NL$Position=="SS")
League.NL.C = subset(League.NL, League.NL$Position=="C")
League.NL.LF = subset(League.NL, League.NL$Position=="LF")
League.NL.CF = subset(League.NL, League.NL$Position=="CF")
League.NL.RF = subset(League.NL, League.NL$Position=="RF")
League.NL.DH = subset(League.NL, League.NL$Position=="DH")
mean(League.AL.1B$RV)
mean(League.AL.2B$RV)
mean(League.AL.3B$RV)
mean(League.AL.SS$RV)
mean(League.AL.C$RV)
mean(League.AL.LF$RV)
mean(League.AL.CF$RV)
mean(League.AL.RF$RV)
mean(League.AL.DH$RV)
mean(League.NL.1B$RV)
mean(League.NL.2B$RV)
mean(League.NL.3B$RV)
mean(League.NL.SS$RV)
mean(League.NL.C$RV)
mean(League.NL.LF$RV)
mean(League.NL.CF$RV)
mean(League.NL.RF$RV)
mean(League.NL.DH$RV)
mean(League.NL$RV)
mean(League.AL$RV)
P.1B = subset(players, players$Position=="1B")
P.2B = subset(players, players$Position=="2B")
P.3B = subset(players, players$Position=="3B")
P.SS = subset(players, players$Position=="SS")
P.C = subset(players, players$Position=="C")
P.LF = subset(players, players$Position=="LF")
P.CF = subset(players, players$Position=="CF")
P.RF = subset(players, players$Position=="RF")
P.DH = subset(players, players$Position=="DH")
mean(P.1B$RV)
mean(P.2B$RV)
mean(P.3B$RV)
mean(P.SS$RV)
mean(P.C$RV)
mean(P.LF$RV)
mean(P.CF$RV)
mean(P.RF$RV)
mean(P.DH$RV)

RV.league.aov = aov(RV ~ League)
anova(RV.league.aov)
RR.league.aov = aov(RR ~ League)
anova(RR.league.aov)
RV.g1.aov = aov(RV ~ Position==c("C","SS","2B","CF"))
anova(RV.g1.aov)
RR.g1.aov = aov(RR ~ Position==c("C","SS","2B","CF"))
anova(RR.g1.aov)
RV.g2.aov = aov(RV ~ Position==c("1B","3B","RF","LF"))
anova(RV.g2.aov)
RR.g2.aov = aov(RR ~ Position==c("1B","3B","RF","LF"))
anova(RR.g2.aov)


G1.1 = subset(players, players$Position==c("C","SS"))
G1.2 = subset(players, players$Position==c("2B","CF"))
G1 = rbind(G1.1,G1.2)
G1

G2.1 = subset(players, players$Position==c("1B","3B"))
G2.2 = subset(players, players$Position==c("RF","LF"))
G2 = rbind(G2.1,G2.2)
G2

G1rv.aov = aov(G1$RV ~ G1$Position)
anova(G1rv.aov)
RR.aov = aov(RR ~ Position)
anova(RR.aov)



Rv.position.aov = aov(RV ~ Position==c("1B","3B","RF","SS"))
anova(Rv.position.aov)


Position==c("1B","3B","RF","SS")
Position

interaction.plot(RV,,ssb, lty=1, col=c(2,3), lwd=3, cex.axis=1.4, cex.lab=1.4)

Position[1:146] == c("C", "SS", "2B","CF")
Position  =="C" OR "SS"
?|

for (i in 1:length(Position) ) {
  if (Position[i] == "C") {
    Group[i] = 1
  }
  else if (Position[i] == "SS") {
    Group[i] = 1
  }
  else if (Position[i] == "2B") {
    Group[i] = 1
  }
  else if (Position[i] == "CF") {
    Group[i] = 1
  }
  else {
    Group[i] = 2
  }
}
Group
players = cbind(players,Group)

players

RV1.aov = aov(RV ~ League*Group)
anova(RV1.aov)
RVm = median(RV)

HOOP = matrix(0,length(RV),1)
for (i in 1:length(RV) ) {
  if (RV[i] > RVm) {
    HOOP[i] = 1
  }
  else {
    HOOP[i] = 2
  }
}
HOOP
players = cbind(players,HOOP)
players

hoop.aov = aov(HOOP ~ League*Group)
anova(hoop.aov)

Npo1 = subset(League.NL, League.NL$HOOP=="1")
Npo2 = subset(League.NL, League.NL$HOOP=="2")
Apo1 = subset(League.AL, League.AL$HOOP=="1")
Apo2 = subset(League.AL, League.AL$HOOP=="2")
hoop1 = subset(players, players$HOOP=="1")
hoop2 = subset(players, players$HOOP=="2")
length(Npo1$RV)
length(Npo2$RV)
length(Apo1$RV)
length(Apo2$RV)
length(hoop1$RV)
length(hoop2$RV)



#PROBLEM 5

Age.lm = lm(RV ~ Age)
summary(Age.lm)
Age.aov = aov(RV ~ Age)
anova(Age.aov)

par(mfrow=c(1,1))
hist(Age)
plot(Age, RV, main = "RV vs Age", xlab = "Age", ylab = "RV")
abline(Age.lm)


DH1 = subset(players, players$Position=="DH")
DH1
hist(players$HR)
hist(DH1$HR,xlim = c(0,40))
?xlim


runs.aov = aov(R ~ X3B*HR)
anova(runs.aov)


barplot(R, names.arg = Team, las = 3)


PO = matrix(0,length(Team),1)
for (i in 1:length(Team) ) {
  if (Team[i] == "Cardinals") {
    PO[i] = 1
  }
  else if (Team[i] == "Dodgers") {
    PO[i] = 1
  }
  else if (Team[i] == "Giants") {
    PO[i] = 1
  }
  else if (Team[i] == "Pirates") {
    PO[i] = 1
  }
  else if (Team[i] == "Tigers") {
    PO[i] = 1
  }
  else if (Team[i] == "Athletics") {
    PO[i] = 1
  }
  else if (Team[i] == "Royals") {
    PO[i] = 1
  }
  else if (Team[i] == "Angels") {
    PO[i] = 1
  }
  else if (Team[i] == "Orioles") {
    PO[i] = 1
  }
  else if (Team[i] == "Nationals") {
    PO[i] = 1
  }
  else {
    PO[i] = 0
  }
}
baseball = cbind(baseball, PO)
PO.1 = subset(baseball, baseball$PO == 1)
PO.2 = subset(baseball, baseball$PO == 0)
t.test(PO.1$R, PO.2$R, alternative = "greater")


?t.test

t.test(R,PO)
PO.aov = aov(R ~ PO)
anova(PO.aov)
baseball
