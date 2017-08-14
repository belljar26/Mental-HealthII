MH2016 <- read.csv("MH2016.csv")
View(MH2016)
summary(MH2016)
str(MH2016)
MH2016 <- read.csv("MH2016.csv")
View(MH2016)
summary(MH2016)
str(MH2016)
levels(MH2016$Gender)
MH2016$Gender[(MH2016$Gender == "m") |
                (MH2016$Gender == "M") |
                (MH2016$Gender == "Ml")|
                (MH2016$Gender == "maile") |
                (MH2016$Gender == "male") |
                (MH2016$Gender == "MALE") |
                (MH2016$Gender == "Male.") |
                (MH2016$Gender == "Malr") |
                (MH2016$Gender == "Man")] <- "Male"
MH2016$Gender[(MH2016$Gender == "woman")|
                (MH2016$Gender == "Woman")] <- "Female"
MH2016$Gender[(MH2016$Gender== "female")] <- "Female"
MH2016$Gender[(MH2016$Gender == " ") |
                (MH2016$Gender == "AFAB") |
                (MH2016$Gender == "Agender") |
                (MH2016$Gender == "Androgynous") |
                (MH2016$Gender == "Bigender") |
                (MH2016$Gender == "Cis female")|
                (MH2016$Gender == "cis male") |
                (MH2016$Gender == "Cis male") |
                (MH2016$Gender == "Cis Male") |
                (MH2016$Gender == "cis man") |
                (MH2016$Gender == "cis-woman") |
                (MH2016$Gender == "cisdude") |
                (MH2016$Gender == "Cisgender Female") |
                (MH2016$Gender =="Enby")|
                (MH2016$Gender == "Female assigned a birth")|
                (MH2016$Gender == "Female or Multi-Gender Femme")|
                (MH2016$Gender == "female-bodied; no feelings about gender")|
                (MH2016$Gender == "Fluid")|
                (MH2016$Gender== "Genderfluid")|
                (MH2016$Gender== "Genderfluid (born female") |
                (MH2016$Gender == "Genderflux demi-girl")|
                (MH2016$Gender == "genderqueer") |
                (MH2016$Gender == "Genderqueer") |
                (MH2016$Gender == "genderqueer woman") |
                (MH2016$Gender == "Male (cis)")|
                (MH2016$Gender == "Male (trans, FtM)") |
                (MH2016$Gender == "male 9:1 female, roughly")|
                (MH2016$Gender == "Male/genderqueer") |
                (MH2016$Gender == "mtf") |
                (MH2016$Gender == "N/A") |
                (MH2016$Gender == "nb masculine") |
                (MH2016$Gender == "non-binary")|
                (MH2016$Gender == "Nonbinary") |
                (MH2016$Gender== "none of your business")|
                (MH2016$Gender == "Other/Transfeminine") |
                (MH2016$Gender =="Queer")|
                (MH2016$Gender == "Transgender woman")|
                (MH2016$Gender == "Transitioned, M2F") |
                (MH2016$Gender == "Unicorn")] <- "Other"
             
MH2016$Gender[(MH2016$Gender== "Sex is male")|
                (MH2016$Gender == "I'm a man why didn't you make this a drop down question. You should of asked sex? And I would of answered yes please. Seriously how much text can this take?")] <- "Male"


MH2016$Gender[(MH2016$Gender == "f")|
                (MH2016$Gender == "F")|
                (MH2016$Gender == "fem")|
                (MH2016$Gender == "female")|
                (MH2016$Gender == "fm")|
                (MH2016$Gender == "female/woman")]<- "Female"
MH2016$Gender[(MH2016$Gender == "male")|
                (MH2016$Gender== "man")] <- "Male"
write_csv(MH2016, "MH2016.csv")
levels(MH2016$Gender)
MH2016$Gender[(MH2016$Gender== "Cis female") |
                (MH2016$Gender == "Cis-woman")|
                (MH2016$Gender == "human") |
                (MH2016$Gender == "Human") |
                (MH2016$Gender== "Genderfluid (born female)")] <- "Other"
MH2016$Gender[(MH2016$Gender == "Female (props for making this a freeform field, though)")|
                (MH2016$Gender== "I identify as female") |
                (MH2016$Gender== "Female assigned at birth")] <- "Female"
MH2016$Gender <- as.character(MH2016$Gender)
write_csv(MH2016, "MH2016.csv")
MH2016 <- read_csv("MH2016.csv")
View(MH2016)
MH2016$Gender[(MH2016$Gender== "female") |
                (MH2016$Gender == "I identify as female.")]<- "Female"
MH2016$Gender[(MH2016$Gender == "mail") |
                (MH2016$Gender == "male")|
                (MH2016$Gender == "M|")|
                (MH2016$Gender == "Dude")]<- "Male"
levels(MH2016$Gender)
levels(MH2016$FamilyHX)
MH2016$FamilyHX <- as.character(MH2016$FamilyHX)
# clean up spaces 
MH2016$FamilyHX[(MH2016$FamilyHX== "I don't know" )] <- "DK"
MH2016$FamilyHX[is.na(MH2016$FamilyHX)]<- "DK"
MH2016$CurrentCondition <- as.factor(MH2016$CurrentCondition)

levels(MH2016$CurrentCondition)
## MH Benefits clean column
MH2016$MHBenefits[(MH2016$MHBenefits == "Not eligible for coverage / N/A")] <- "NE"
MH2016$MHBenefits[(MH2016$MHBenefits == "I don't know")] <- "DK"

mean(MH2016$Age)
#mean age = 34 SD = 11.29
sd(MH2016$Age)



summary(MH2016$Age)
which.max(MH2016$Age)
MH2016[565,]
MH2016$Age[(MH2016$Age == 323)] <- 32
## most likely age = 32
summary(MH2016$Age)
which.max(MH2016$Age)
MH2016$Age[(MH2016$Age == 99)] <- 33
## replace with median
summary(MH2016$Age)
tapply(MH2016$Age, MH2016$Country, mean, na.rm= TRUE)
table(MH2016$Tech)
table(MH2016$MHNegCareer)
Tech_MH <- subset(MH2016, !Tech == 0)
View(Tech_MH)
boxplot(Tech_MH$Age ~Tech_MH$WorkCountry)

(588 + 563 + 105)/ (588 + 147 + 30 +563 + 105)
# 87.6 % believe that a mental health disorder might or definitely would harm career
write_csv(MH2016, "MH2016.csv")
write_csv(Tech_MH, "Tech_MH.csv")
