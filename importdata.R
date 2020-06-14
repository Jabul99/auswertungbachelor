library(tidyverse)

colorSingleBar <- "steelblue"
colorMan <-"#0703f0"
colorWoman <- #c74dc2"
  colorGenderNot <- #85796f
  colorsGender <- c("#0703f0","#c74dc2", "#85796f")  

dat <- read.csv2('D:\\Desktop\\BIT\\Bachelor\\06_Auswertung\\Angepasste Tabelle.csv')
q1_Suchmaschine <-  subset(dat, select=c("Nummer", "X01_1", "X01_2", "X01_3", "X01_4", "X01_5", "X01_6", "X01b"))
q1_Suchmaschine <-  q1_Suchmaschine %>% rename(google = X01_1, bing = X01_2, yahoo = X01_3, search.ch = X01_4 , escosia.org = X01_5, Andere =X01_6, TextAndere = X01b)

levels = c("nie", "selten", "monatlich","wöchentlich","nahezu täglich")
q1_SuchmaschineAngepasst <- q1_Suchmaschine %>% subset(select =c(Nummer, google, bing, yahoo, search.ch, escosia.org, Andere)) %>% 
  mutate(
    google = case_when(
      google == 1 ~ levels[1], 
      google == 2 ~ levels[2], 
      google == 3 ~ levels[3],
      google == 4 ~ levels[4],
      google == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    bing = case_when(
      bing == 1 ~ levels[1], 
      bing == 2 ~ levels[2], 
      bing == 3 ~ levels[3],
      bing == 4 ~ levels[4],
      bing == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    yahoo = case_when(
      yahoo == 1 ~ levels[1], 
      yahoo == 2 ~ levels[2], 
      yahoo == 3 ~ levels[3],
      yahoo == 4 ~ levels[4],
      yahoo == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    search.ch = case_when(
      search.ch == 1 ~ levels[1], 
      search.ch == 2 ~ levels[2], 
      search.ch == 3 ~ levels[3],
      search.ch == 4 ~ levels[4],
      search.ch == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    escosia.org = case_when(
      escosia.org == 1 ~ levels[1], 
      escosia.org == 2 ~ levels[2], 
      escosia.org == 3 ~ levels[3],
      escosia.org == 4 ~ levels[4],
      escosia.org == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    Andere = case_when(
      Andere == 1 ~ levels[1], 
      Andere == 2 ~ levels[2], 
      Andere == 3 ~ levels[3],
      Andere == 4 ~ levels[4],
      Andere == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels))

q1_SuchmaschineAngepasst <- q1_SuchmaschineAngepasst %>% gather(google, bing, yahoo, search.ch, escosia.org, Andere, key = Suchmaschine, value = Frequenz)

q1_SuchmaschineAngepasst$Suchmaschine <- as.factor(q1_SuchmaschineAngepasst$Suchmaschine)
q1_SuchmaschineAngepasst$Frequenz <- as.factor(q1_SuchmaschineAngepasst$Frequenz)
q1_SuchmaschineAngepasst$Suchmaschine <- factor(q1_SuchmaschineAngepasst$Suchmaschine, levels = c("google", "bing", "yahoo","search.ch","escosia.org", "Andere"))
q1_SuchmaschineAngepasst$Frequenz <- factor(q1_SuchmaschineAngepasst$Frequenz, levels = c("nie", "selten", "monatlich","wöchentlich","nahezu täglich"))

q2_bezahlteLinks <- subset(dat, select=c("Nummer", "X2"))
q2_bezahlteLinks <- q2_bezahlteLinks %>% rename(ZweiteSeite = X2)

levels = c("0 bis 20%", "21 bis 40%", "41 bis 60%","61 bis 80%","81 bis 100%")

q2_bezahlteLinksAngepasst <- q2_bezahlteLinks  %>% subset(select =c(Nummer, ZweiteSeite)) %>% 
  mutate(
    ZweiteSeite = case_when(
      ZweiteSeite == 1 ~ levels[1], 
      ZweiteSeite == 2 ~ levels[2], 
      ZweiteSeite == 3 ~ levels[3],
      ZweiteSeite == 4 ~ levels[4],
      ZweiteSeite == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels))


q3_bezahlteLinks <- subset(dat, select=c("Nummer", "X3"))
q3_bezahlteLinks <- q3_bezahlteLinks %>% rename(ProzentBezahlteLinks = X3)

levels = c("0 bis 20%", "21 bis 40%", "41 bis 60%","61 bis 80%","81 bis 100%")

q3_bezahlteLinksAngepasst <- q3_bezahlteLinks  %>% subset(select =c(Nummer, ProzentBezahlteLinks)) %>% 
  mutate(
    ProzentBezahlteLinks = case_when(
      ProzentBezahlteLinks == 1 ~ levels[1], 
      ProzentBezahlteLinks == 2 ~ levels[2], 
      ProzentBezahlteLinks == 3 ~ levels[3],
      ProzentBezahlteLinks == 4 ~ levels[4],
      ProzentBezahlteLinks == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels))

q4_bezahlteLinks <- subset(dat, select=c("Nummer", "X4"))
q4_bezahlteLinks <- q4_bezahlteLinks %>% rename(ProzentKaufBezahlteLinks = X4)

levels = c("0 bis 20%", "21 bis 40%", "41 bis 60%","61 bis 80%","81 bis 100%")

q4_bezahlteLinksAngepasst <- q4_bezahlteLinks  %>% subset(select =c(Nummer, ProzentKaufBezahlteLinks)) %>% 
  mutate(
    ProzentKaufBezahlteLinks = case_when(
      ProzentKaufBezahlteLinks == 1 ~ levels[1], 
      ProzentKaufBezahlteLinks == 2 ~ levels[2], 
      ProzentKaufBezahlteLinks == 3 ~ levels[3],
      ProzentKaufBezahlteLinks == 4 ~ levels[4],
      ProzentKaufBezahlteLinks == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels))


q5_bezahlteLinks <- subset(dat, select=c("Nummer","X5"))
q5_bezahlteLinks <- q5_bezahlteLinks %>% rename(ZweckKauf = X5)

levels = c("privat", "geschaeftlich", "beides")

q5_bezahlteLinksAngepasst <- q5_bezahlteLinks  %>% subset(select =c(Nummer, ZweckKauf)) %>% 
  mutate(
    ZweckKauf = case_when(
      ZweckKauf == 1 ~ levels[1], 
      ZweckKauf == 2 ~ levels[2], 
      ZweckKauf == 3 ~ levels[3],
      TRUE ~  levels[1]) %>% factor(levels))


q6_VewendungSozialMedia <- subset(dat, select=c("Nummer", "X06_1", "X06_2", "X06_3", "X06_4", "X06_5", "X06_6", "X06_7", "X06_8", "X06b"))
q6_VewendungSozialMedia <-  q6_VewendungSozialMedia %>% rename(Facebook = X06_1, Instagram = X06_2, YouTube = X06_3, Twitter = X06_4 , LinkedIn = X06_5, Spnapchat =X06_6, TikTok =X06_7, Andere =X06_8, AndereText = X06b)

levels = c("nie", "selten", "monatlich","wöchentlich","nahezu täglich")
q6_VewendungSozialMediaAngepasst <- q6_VewendungSozialMedia %>% subset(select =
                                                                         
                                                                         c(Facebook, Instagram, YouTube, Twitter, LinkedIn, Spnapchat, TikTok, Andere)) %>% 
  mutate(
    Facebook = case_when(
      Facebook == 1 ~ levels[1], 
      Facebook == 2 ~ levels[2], 
      Facebook == 3 ~ levels[3],
      Facebook == 4 ~ levels[4],
      Facebook == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    Instagram = case_when(
      Instagram == 1 ~ levels[1], 
      Instagram == 2 ~ levels[2], 
      Instagram == 3 ~ levels[3],
      Instagram == 4 ~ levels[4],
      Instagram == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    YouTube = case_when(
      YouTube == 1 ~ levels[1], 
      YouTube == 2 ~ levels[2], 
      YouTube == 3 ~ levels[3],
      YouTube == 4 ~ levels[4],
      YouTube == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    Twitter = case_when(
      Twitter == 1 ~ levels[1], 
      Twitter == 2 ~ levels[2], 
      Twitter == 3 ~ levels[3],
      Twitter == 4 ~ levels[4],
      Twitter == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    LinkedIn = case_when(
      LinkedIn == 1 ~ levels[1], 
      LinkedIn == 2 ~ levels[2], 
      LinkedIn == 3 ~ levels[3],
      LinkedIn == 4 ~ levels[4],
      LinkedIn == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    Spnapchat = case_when(
      Spnapchat == 1 ~ levels[1], 
      Spnapchat == 2 ~ levels[2], 
      Spnapchat == 3 ~ levels[3],
      Spnapchat == 4 ~ levels[4],
      Spnapchat == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    TikTok = case_when(
      TikTok == 1 ~ levels[1], 
      TikTok == 2 ~ levels[2], 
      TikTok == 3 ~ levels[3],
      TikTok == 4 ~ levels[4],
      TikTok == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    Andere = case_when(
      Andere == 1 ~ levels[1], 
      Andere == 2 ~ levels[2], 
      Andere == 3 ~ levels[3],
      Andere == 4 ~ levels[4],
      Andere == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels))

q6_VewendungSozialMediaAngepasst <- q6_VewendungSozialMediaAngepasst %>% gather(Facebook, Instagram, YouTube, Twitter, LinkedIn, Spnapchat, TikTok, Andere, key = SocialMedia, value = Frequenz)

q6_VewendungSozialMediaAngepasst$SocialMedia <- as.factor(q6_VewendungSozialMediaAngepasst$SocialMedia)
q6_VewendungSozialMediaAngepasst$Frequenz <- as.factor(q6_VewendungSozialMediaAngepasst$Frequenz)
q6_VewendungSozialMediaAngepasst$SocialMedia <- factor(q6_VewendungSozialMediaAngepasst$SocialMedia, levels = c("Facebook", "Instagram", "YouTube","Twitter","LinkedIn", "Spnapchat", "TikTok", "Andere"))
q6_VewendungSozialMediaAngepasst$Frequenz <- factor(q6_VewendungSozialMediaAngepasst$Frequenz, levels = c("nie", "selten", "monatlich","wöchentlich","nahezu täglich"))

q7_SucheProduktSozialMedia <- subset(dat, select=c("Nummer", "X07_1", "X07_2"))
q7_SucheProduktSozialMedia <- q7_SucheProduktSozialMedia %>% rename(privat = X07_1, geschaeftlich = X07_2)

levels = c("nie", "eher nicht", "vielleicht","ziemlich wahrscheinlich","ganz sicher")
q7_SucheProduktSozialMediaAngepasst <- q7_SucheProduktSozialMedia %>% subset(select = c(privat, geschaeftlich)) %>% 
  mutate(
    privat = case_when(
      privat == 1 ~ levels[1], 
      privat == 2 ~ levels[2], 
      privat == 3 ~ levels[3],
      privat == 4 ~ levels[4],
      privat == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    geschaeftlich = case_when(
      geschaeftlich == 1 ~ levels[1], 
      geschaeftlich == 2 ~ levels[2], 
      geschaeftlich == 3 ~ levels[3],
      geschaeftlich == 4 ~ levels[4],
      geschaeftlich == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels))

q7_SucheProduktSozialMediaAngepasst <- q7_SucheProduktSozialMediaAngepasst %>% gather(privat, geschaeftlich, key = Art, value = Frequenz)

q7_SucheProduktSozialMediaAngepasst$Art <- as.factor(q7_SucheProduktSozialMediaAngepasst$Art)
q7_SucheProduktSozialMediaAngepasst$Frequenz <- as.factor(q7_SucheProduktSozialMediaAngepasst$Frequenz)
q7_SucheProduktSozialMediaAngepasst$Art <- factor(q7_SucheProduktSozialMediaAngepasst$Art, levels = c("privat", "geschaeftlich"))
q7_SucheProduktSozialMediaAngepasst$Frequenz <- factor(q7_SucheProduktSozialMediaAngepasst$Frequenz, levels = levels)


q8_KommunikationUnternehmen <- subset(dat, select=c("Nummer", "X08_1", "X08_2"))
q8_KommunikationUnternehmen <- q8_KommunikationUnternehmen %>% rename(privat = X08_1, geschaeftlich = X08_2)

levels = c("nie", "selten", "gelegentlich","oft","immer")
q8_KommunikationUnternehmenAngepasst <- q8_KommunikationUnternehmen %>% subset(select = c(privat, geschaeftlich)) %>% 
  mutate(
    privat = case_when(
      privat == 1 ~ levels[1], 
      privat == 2 ~ levels[2], 
      privat == 3 ~ levels[3],
      privat == 4 ~ levels[4],
      privat == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    geschaeftlich = case_when(
      geschaeftlich == 1 ~ levels[1], 
      geschaeftlich == 2 ~ levels[2], 
      geschaeftlich == 3 ~ levels[3],
      geschaeftlich == 4 ~ levels[4],
      geschaeftlich == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels))

q8_KommunikationUnternehmenAngepasst <- q8_KommunikationUnternehmenAngepasst %>% gather(privat, geschaeftlich, key = Art, value = Frequenz)

q8_KommunikationUnternehmenAngepasst$Art <- as.factor(q8_KommunikationUnternehmenAngepasst$Art)
q8_KommunikationUnternehmenAngepasst$Frequenz <- as.factor(q8_KommunikationUnternehmenAngepasst$Frequenz)
q8_KommunikationUnternehmenAngepasst$Art <- factor(q8_KommunikationUnternehmenAngepasst$Art, levels = c("privat", "geschaeftlich"))
q8_KommunikationUnternehmenAngepasst$Frequenz <- factor(q8_KommunikationUnternehmenAngepasst$Frequenz, levels = levels)


q9_KommunikationUnternehmenhaufig <- subset(dat, select=c("Nummer", "X09_1", "X09_2"))
q9_KommunikationUnternehmenhaufig <- q9_KommunikationUnternehmenhaufig %>% rename(privat = X09_1, geschaeftlich = X09_2)


levels = c("nie", "selten", "gelegentlich","oft","immer")
q9_KommunikationUnternehmenhaufigAngepasst <- q9_KommunikationUnternehmenhaufig %>% subset(select = c(privat, geschaeftlich)) %>% 
  mutate(
    privat = case_when(
      privat == 1 ~ levels[1], 
      privat == 2 ~ levels[2], 
      privat == 3 ~ levels[3],
      privat == 4 ~ levels[4],
      privat == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    geschaeftlich = case_when(
      geschaeftlich == 1 ~ levels[1], 
      geschaeftlich == 2 ~ levels[2], 
      geschaeftlich == 3 ~ levels[3],
      geschaeftlich == 4 ~ levels[4],
      geschaeftlich == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels))

q9_KommunikationUnternehmenhaufigAngepasst <- q9_KommunikationUnternehmenhaufigAngepasst %>% gather(privat, geschaeftlich, key = Art, value = Frequenz)

q9_KommunikationUnternehmenhaufigAngepasst$Art <- as.factor(q9_KommunikationUnternehmenhaufigAngepasst$Art)
q9_KommunikationUnternehmenhaufigAngepasst$Frequenz <- as.factor(q9_KommunikationUnternehmenhaufigAngepasst$Frequenz)
q9_KommunikationUnternehmenhaufigAngepasst$Art <- factor(q9_KommunikationUnternehmenhaufigAngepasst$Art, levels = c("privat", "geschaeftlich"))
q9_KommunikationUnternehmenhaufigAngepasst$Frequenz <- factor(q9_KommunikationUnternehmenhaufigAngepasst$Frequenz, levels = levels)

q10_Unternehmeneinkauf <- subset(dat, select=c("Nummer", "X10_1", "X10_2"))
q10_Unternehmeneinkauf <- q10_Unternehmeneinkauf %>% rename(privat = X10_1, geschaeftlich = X10_2)


levels = c("nie", "selten", "gelegentlich","oft","immer")
q10_UnternehmeneinkaufAngepasst <- q10_Unternehmeneinkauf %>% subset(select = c(privat, geschaeftlich)) %>% 
  mutate(
    privat = case_when(
      privat == 1 ~ levels[1], 
      privat == 2 ~ levels[2], 
      privat == 3 ~ levels[3],
      privat == 4 ~ levels[4],
      privat == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    geschaeftlich = case_when(
      geschaeftlich == 1 ~ levels[1], 
      geschaeftlich == 2 ~ levels[2], 
      geschaeftlich == 3 ~ levels[3],
      geschaeftlich == 4 ~ levels[4],
      geschaeftlich == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels))

q10_UnternehmeneinkaufAngepasst <- q10_UnternehmeneinkaufAngepasst %>% gather(privat, geschaeftlich, key = Art, value = Frequenz)

q10_UnternehmeneinkaufAngepasst$Art <- as.factor(q10_UnternehmeneinkaufAngepasst$Art)
q10_UnternehmeneinkaufAngepasst$Frequenz <- as.factor(q10_UnternehmeneinkaufAngepasst$Frequenz)
q10_UnternehmeneinkaufAngepasst$Art <- factor(q10_UnternehmeneinkaufAngepasst$Art, levels = c("privat", "geschaeftlich"))
q10_UnternehmeneinkaufAngepasst$Frequenz <- factor(q10_UnternehmeneinkaufAngepasst$Frequenz, levels = levels)


q11_UnternehmeneinkaufKonkurrenz <- subset(dat, select=c("Nummer", "X11_1", "X11_2"))
q11_UnternehmeneinkaufKonkurrenz <- q11_UnternehmeneinkaufKonkurrenz %>% rename(privat = X11_1, geschaeftlich = X11_2)

levels = c("keinesfalls", "eher nicht", "vielleicht","ziemlich wahrscheinlich","ganz sicher")
q11_UnternehmeneinkaufKonkurrenzAngepasst <- q11_UnternehmeneinkaufKonkurrenz %>% subset(select = c(privat, geschaeftlich)) %>% 
  mutate(
    privat = case_when(
      privat == 1 ~ levels[1], 
      privat == 2 ~ levels[2], 
      privat == 3 ~ levels[3],
      privat == 4 ~ levels[4],
      privat == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    geschaeftlich = case_when(
      geschaeftlich == 1 ~ levels[1], 
      geschaeftlich == 2 ~ levels[2], 
      geschaeftlich == 3 ~ levels[3],
      geschaeftlich == 4 ~ levels[4],
      geschaeftlich == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels))

q11_UnternehmeneinkaufKonkurrenzAngepasst <- q11_UnternehmeneinkaufKonkurrenzAngepasst %>% gather(privat, geschaeftlich, key = Art, value = Frequenz)

q11_UnternehmeneinkaufKonkurrenzAngepasst$Art <- as.factor(q11_UnternehmeneinkaufKonkurrenzAngepasst$Art)
q11_UnternehmeneinkaufKonkurrenzAngepasst$Frequenz <- as.factor(q11_UnternehmeneinkaufKonkurrenzAngepasst$Frequenz)
q11_UnternehmeneinkaufKonkurrenzAngepasst$Art <- factor(q11_UnternehmeneinkaufKonkurrenzAngepasst$Art, levels = c("privat", "geschaeftlich"))
q11_UnternehmeneinkaufKonkurrenzAngepasst$Frequenz <- factor(q11_UnternehmeneinkaufKonkurrenzAngepasst$Frequenz, levels = levels)


q12_likseinkauf <- subset(dat, select=c("Nummer", "X12_1", "X12_2"))
q12_likseinkauf <- q12_likseinkauf %>% rename(privat = X12_1, geschaeftlich = X12_2)

levels = c("nie", "selten", "gelegentlich","oft","immer")
q12_likseinkaufAngepasst <- q12_likseinkauf %>% subset(select = c(privat, geschaeftlich)) %>% 
  mutate(
    privat = case_when(
      privat == 1 ~ levels[1], 
      privat == 2 ~ levels[2], 
      privat == 3 ~ levels[3],
      privat == 4 ~ levels[4],
      privat == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    geschaeftlich = case_when(
      geschaeftlich == 1 ~ levels[1], 
      geschaeftlich == 2 ~ levels[2], 
      geschaeftlich == 3 ~ levels[3],
      geschaeftlich == 4 ~ levels[4],
      geschaeftlich == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels))

q12_likseinkaufAngepasst <- q12_likseinkaufAngepasst %>% gather(privat, geschaeftlich, key = Art, value = Frequenz)

q12_likseinkaufAngepasst$Art <- as.factor(q12_likseinkaufAngepasst$Art)
q12_likseinkaufAngepasst$Frequenz <- as.factor(q12_likseinkaufAngepasst$Frequenz)
q12_likseinkaufAngepasst$Art <- factor(q12_likseinkaufAngepasst$Art, levels = c("privat", "geschaeftlich"))
q12_likseinkaufAngepasst$Frequenz <- factor(q12_likseinkaufAngepasst$Frequenz, levels = levels)


q13_SucheProduktprivate <- subset(dat, select=c("Nummer", "X13.1_1", "X13.1_2", "X13.1_3", "X13.1_4", "X13.1_5", "X13.1_6"))
q13_SucheProduktprivate <- q13_SucheProduktprivate %>% rename(Vergleichsdienste = X13.1_1, Blogs = X13.1_2, Influencer = X13.1_3, Freunde = X13.1_4, Mitarbeiter = X13.1_5, SocialMedia = X13.1_6 )

q13_SucheProduktfirm <- subset(dat, select=c("Nummer", "X13.2_1", "X13.2_2", "X13.2_3", "X13.2_4", "X13.2_5", "X13.2_6"))
q13_SucheProduktfirm <- q13_SucheProduktfirm %>% rename(Vergleichsdienste = X13.2_1, Blogs = X13.2_2, Influencer = X13.2_3, Freunde = X13.2_4, Mitarbeiter = X13.2_5, SocialMedia = X13.2_6 )

q14_Vertrauen <- subset(dat, select=c("Nummer", "X14_1", "X14_2", "X14_3", "X14_4", "X14_5", "X14_6", "X14_7", "X14_8"))
q14_Vertrauen <-  q14_Vertrauen %>% rename(Vergleichsdienste = X14_1, Blogs = X14_2, Influencer = X14_3, Freunde = X14_4, Mitarbeiter = X14_5, Webseite = X14_6, SocialMedia = X14_7, Email = X14_8)

q15_EmailNewsletter <- subset(dat, select=c("Nummer", "X15_1_1", "X15_1_2", "X15_2_1", "X15_2_2","X15_3_1", "X15_3_2","X15_4_1", "X15_4_2","X15_5_1", "X15_5_2","X15_6_1", "X15_6_2"))
q15_EmailNewsletter <- q15_EmailNewsletter  %>% rename(IntressePrivat = X15_1_1, IntesseFirma = X15_1_2,
                                                       AktionenPrivat = X15_2_1, AktionenFirma = X15_2_2,
                                                       AngebotPrivat = X15_3_1, AngebotFirma = X15_3_2,
                                                       InhaltPrivat = X15_4_1, InhaltFirma = X15_4_2,
                                                       TrendsPrivat = X15_5_1, TrendsFirma = X15_5_2,
                                                       UnternehmenPrivat = X15_6_1, UnternehmenFirma = X15_6_2)

q16_NichtAbonniert <- subset(dat, select=c("Nummer", "X16_1", "X16_2"))
q16_NichtAbonniert <- q16_NichtAbonniert %>% rename(privat = X16_1, geschaeftlich = X16_2)

q17_BevorzugteKommunikation <- subset(dat, select=c("Nummer", "X17_1", "X17_2", "X17_3", "X17_4","X17_5", "X17_6","X17_7", "X17_8","X17_9", "X17_10","X17_10_TEXT"))
q17_BevorzugteKommunikation <- q17_BevorzugteKommunikation  %>% rename(Email = X17_1, Brief = X17_2,
                                                                       MobileApp = X17_3, SocialMedia = X17_4,
                                                                       Messanger = X17_5, SMS = X17_6,
                                                                       Telefon = X17_7, Chatbots = X17_8,
                                                                       ServiceProtale = X17_9, Andere = X17_10,
                                                                       AndereText = X17_10_TEXT)

q18_QuellenSuche <- subset(dat, select=c("Nummer", "X18_1", "X18_2", "X18_3", "X18_4","X18_5", "X18_6","X18_7", "X18_8","X18_9", "X18_10", "X18_11","X18b"))
q18_QuellenSuche <- q18_QuellenSuche  %>% rename(Suchmaschine = X18_1, Vergleichsdienst = X18_2,
                                                 Blogs = X18_3, Influencer = X18_4,
                                                 Facebook = X18_5, Instagram = X18_6,
                                                 YouTube = X18_7, Twitter = X18_8,
                                                 Linkedin = X18_9, TikTok = X18_10,
                                                 Andere = X18_11, AndereText = X18b )

q19_QuellenKauf <- subset(dat, select=c("Nummer", "X19_1", "X19_2", "X19_3", "X19_4","X19_5", "X19_6","X19_7", "X19_8","X19_9", "X19_10"))
q19_QuellenKauf <- q19_QuellenKauf  %>% rename(Suchmaschine = X19_1, Vergleichsdienst = X19_2,
                                               Blogs = X19_3, Influencer = X19_4,
                                               Facebook = X19_5, Instagram = X19_6,
                                               YouTube = X19_7, Twitter = X19_8,
                                               Linkedin = X19_9, TikTok = X19_10)

q20_MediumAktuell <- subset(dat, select=c("Nummer", "X20", "X20_5_TEXT"))
q20_MediumAktuell <- q20_MediumAktuell %>% rename(Medium = X20, Andere = X20_5_TEXT)

q21_MediumZukunft <- subset(dat, select=c("Nummer", "X21", "X21_5_TEXT"))
q21_MediumZukunft <- q21_MediumZukunft %>% rename(Medium = X21, Andere = X21_5_TEXT)

q22_MediumVorteil <- subset(dat, select=c("Nummer", "X22_1", "X22_2", "X22_3", "X22_4", "X22_5", "X22_5_TEXT"))
q22_MediumVorteil <- q22_MediumVorteil %>% rename(Text = X22_1, Bilder = X22_2,
                                                  Audio = X22_3, Video =X22_4,
                                                  Andere = X22_5, AndereText = X22_5_TEXT)

q23_Geschlecht <- subset(dat, select=c("Nummer", "X23"))
q23_Geschlecht <- q23_Geschlecht %>% rename(Geschlecht = X23)
levels = c("männlich", "weiblich", "Keine Auswahl")
q23_Geschlecht <- q23_Geschlecht %>% mutate(GeschlechtText = case_when(Geschlecht == 1 ~ levels[1], Geschlecht == 2 ~ levels[2], TRUE ~  levels[3]) %>% factor(levels))


q24_Alter <- subset(dat, select=c("Nummer", "X24"))
q24_Alter <- q24_Alter %>% rename(Alter = X24)

levels = c("unter 18 Jahre", "18-25", "26-32","33-40","41-55","56-65","über 65 Jahre", "Leer")
q24_Alter <- q24_Alter %>% mutate(AlterText = case_when(
  Alter == 1 ~ levels[1], 
  Alter == 2 ~ levels[2], 
  Alter == 3 ~ levels[3],
  Alter == 4 ~ levels[4],
  Alter == 5 ~ levels[5],
  Alter == 6 ~ levels[6],
  Alter == 7 ~ levels[7],
  TRUE ~  levels[8]) %>% factor(levels))

q24_AlterJoined <- left_join(q24_Alter,q23_Geschlecht, by = "Nummer")

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getfrequenz <- function(x, NumberOfItems, RowNamesLevels, ColumnNames, itemNumbers){
  numberofColumns <- ncol(x)
  numberofRows <-  nrow(x)
  items <- c(1:NumberOfItems)
  itemscolums <- c(1: numberofColumns)
  
  resultDataframe <- data.frame(Nr = itemNumbers, Feld = RowNamesLevels)
  
  for(itemNumber in items){
    columnname = ColumnNames[itemNumber]
    resultDataframe <- resultDataframe %>% mutate(UQ(rlang::sym(columnname)) := "0.00% (0)")
  }
  
  resultDataframe <- resultDataframe %>% mutate(Summe = "0")
  
  for(numberofCurrentColumn in itemscolums){
    
    numbers <- transform(as.data.frame(table(x[numberofCurrentColumn])), percentage = round(Freq/nrow(x)*100, digits = 2))
    numbers$Var1 <- as.numeric(as.character(numbers$Var1))
    
    summeItems <- 0
    for(val in items)
    {
      if(!is.na(numbers[val,1]))
      {
        columnnumber <- as.numeric(numbers[val,1]) +2
        rownumber <- numberofCurrentColumn
        
        resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers[val,3]), "%\t (",as.character(numbers[val,2]), ")", sep ="")
        
        summeItems <- summeItems + as.numeric(numbers[val,2])
      }
      resultDataframe[numberofCurrentColumn,NumberOfItems +3] <- summeItems
    }
  }
  
  return(resultDataframe)
}


getLageparameter <- function(x, RowNamesLevels, itemNumbers){
  numberofColumns <-ncol(x)
  itemscolums <- c(1: numberofColumns)
  
  values <- as.vector(rep(0, numberofColumns))
  
  
  resultDataframe <- data.frame(Nr = itemNumbers, Feld = RowNamesLevels, Minimum =values, Maximum=values, Modus=values, Mittelwert=values, Standardabweichung=values, Median=values)
  
  for(numberofCurrentColumn in itemscolums){
    
    resultDataframe[numberofCurrentColumn,3] <- min(x[,numberofCurrentColumn] , na.rm = TRUE)
    resultDataframe[numberofCurrentColumn,4] <- max(x[,numberofCurrentColumn], na.rm = TRUE)
    resultDataframe[numberofCurrentColumn,5] <- getmode(x[,numberofCurrentColumn])
    resultDataframe[numberofCurrentColumn,6] <- round(mean(x[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[numberofCurrentColumn,7] <- round(sd(x[,numberofCurrentColumn], na.rm = TRUE),digits=2)
    resultDataframe[numberofCurrentColumn,8] <- round(median(x[,numberofCurrentColumn], na.rm = TRUE),digits=2)
  }
  
  return(resultDataframe)
}

getLageparameter1Dimensional <- function(x, TitelField){
  numberofColumns <-ncol(x)
  itemscolums <- c(1: numberofColumns)
  
  values <- c(0)
  
  
  resultDataframe <- data.frame(Nr = c(1), Feld = TitelField, Minimum =values, Maximum=values, Modus=values, Mittelwert=values, Standardabweichung=values, Median=values)
  resultDataframe[1,3] <- min(x[,1] , na.rm = TRUE)
  resultDataframe[1,4] <- max(x[,1], na.rm = TRUE)
  resultDataframe[1,5] <- getmode(x[,1])
  resultDataframe[1,6] <- round(mean(x[,1], na.rm = TRUE),digits=2)
  resultDataframe[1,7] <- round(sd(x[,1], na.rm = TRUE),digits=2)
  resultDataframe[1,8] <- round(median(x[,1], na.rm = TRUE),digits=2)
  
  return(resultDataframe)
}

getfrequenz1Dimension <- function(x, NumberOfItems, RowNamesLevels, itemNumbers){
  items <- c(1:NumberOfItems)
  
  resultDataframe <- data.frame(Nr = itemNumbers, Feld = RowNamesLevels)
  resultDataframe <- resultDataframe %>% mutate(Antwortanzahl= "0.00% (0)")
  
  numbers <- transform(as.data.frame(table(x[1])), percentage = round(Freq/nrow(x)*100, digits = 2))
  numbers$Var1 <- as.numeric(as.character(numbers$Var1))
  
  for(val in items)
  {
    if(!is.na(numbers[val,1]))
    {
      columnnumber <- 3
      rownumber <- as.numeric(numbers[val,1])
      
      resultDataframe[rownumber,columnnumber] <- paste(as.character(numbers[val,3]), "%\t (",as.character(numbers[val,2]), ")", sep ="")
    }
  }
  
  
  return(resultDataframe)
}



q15_EmailNewsletterprivat <- subset(dat, select=c("Nummer", "X15_1_1", "X15_2_1","X15_3_1", "X15_4_1", "X15_5_1", "X15_6_1"))
q15_EmailNewsletterprivat <- q15_EmailNewsletterprivat  %>% rename(Intresse = X15_1_1, Aktionen = X15_2_1,
                                                                   Angebot = X15_3_1, Inhalt = X15_4_1, Trends = X15_5_1, Unternehmen = X15_6_1)

q15_EmailNewsletterfirm <- subset(dat, select=c("Nummer", "X15_1_2", "X15_2_2","X15_3_2", "X15_4_2", "X15_5_2", "X15_6_2"))
q15_EmailNewsletterfirm <- q15_EmailNewsletterfirm  %>% rename(Intresse = X15_1_2, Aktionen = X15_2_2,
                                                               Angebot = X15_3_2, Inhalt = X15_4_2, Trends = X15_5_2, Unternehmen = X15_6_2)


q15_EmailNewsletterprivatAngepasst <- q15_EmailNewsletterprivat %>% gather(Intresse, Aktionen, Angebot, Inhalt, Trends, Unternehmen, key = Grund, value = Frequenz)
q15_EmailNewsletterprivatAngepasst <- q15_EmailNewsletterprivatAngepasst %>% mutate(Art = 1)
q15_EmailNewsletterfirmAngepasst <- q15_EmailNewsletterfirm %>% gather(Intresse, Aktionen, Angebot, Inhalt, Trends, Unternehmen, key = Grund, value = Frequenz)
q15_EmailNewsletterfirmAngepasst <- q15_EmailNewsletterfirmAngepasst %>% mutate(Art = 2)


q15_EmailNewsletter <- rbind(q15_EmailNewsletterprivatAngepasst,q15_EmailNewsletterfirmAngepasst)


q15_EmailNewsletter %>% ggplot(aes(x=Art, y=Frequenz, fill= Grund)) + geom_histogram(position = "dodge2") + xlab("") + ylab("Anzahl") + coord_flip()

q14_Vertrauen <- subset(dat, select=c("Nummer", "X14_1", "X14_2", "X14_3", "X14_4", "X14_5", "X14_6", "X14_7", "X14_8"))
q14_Vertrauen <-  q14_Vertrauen %>% rename(Vergleichsdienste = X14_1, Blogs = X14_2, Influencer = X14_3, Freunde = X14_4, Mitarbeiter = X14_5, Webseite = X14_6, SocialMedia = X14_7, Email = X14_8)

columnNames1 <- c("Vergleichsdienste", "Blogs", "Influencer", "Freunde", "Mitarbeiter", "Webseite", "SocialMedia", "Email")

levels = c("nicht vertrauenswürdig", "wenig vertrauenswürdig", "mittelmässig vertrauenswürdig","ziemlich vertrauenswürdig","sehr vertrauenswürdig")
q14_VertrauenAngepasst <- q14_Vertrauen %>% subset(select =              c(Nummer,Vergleichsdienste, Blogs, Influencer, Freunde, Mitarbeiter, Webseite, SocialMedia, Email)) %>% 
  mutate(
    Vergleichsdienste = case_when(
      Vergleichsdienste == 1 ~ levels[1], 
      Vergleichsdienste == 2 ~ levels[2], 
      Vergleichsdienste == 3 ~ levels[3],
      Vergleichsdienste == 4 ~ levels[4],
      Vergleichsdienste == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    Blogs = case_when(
      Blogs == 1 ~ levels[1], 
      Blogs == 2 ~ levels[2], 
      Blogs == 3 ~ levels[3],
      Blogs == 4 ~ levels[4],
      Blogs == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    Influencer = case_when(
      Influencer == 1 ~ levels[1], 
      Influencer == 2 ~ levels[2], 
      Influencer == 3 ~ levels[3],
      Influencer == 4 ~ levels[4],
      Influencer == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    Freunde = case_when(
      Freunde == 1 ~ levels[1], 
      Freunde == 2 ~ levels[2], 
      Freunde == 3 ~ levels[3],
      Freunde == 4 ~ levels[4],
      Freunde == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    Mitarbeiter = case_when(
      Mitarbeiter == 1 ~ levels[1], 
      Mitarbeiter == 2 ~ levels[2], 
      Mitarbeiter == 3 ~ levels[3],
      Mitarbeiter == 4 ~ levels[4],
      Mitarbeiter == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    Webseite = case_when(
      Webseite == 1 ~ levels[1], 
      Webseite == 2 ~ levels[2], 
      Webseite == 3 ~ levels[3],
      Webseite == 4 ~ levels[4],
      Webseite == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    Email = case_when(
      Email == 1 ~ levels[1], 
      Email == 2 ~ levels[2], 
      Email == 3 ~ levels[3],
      Email == 4 ~ levels[4],
      Email == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels),
    SocialMedia = case_when(
      SocialMedia == 1 ~ levels[1], 
      SocialMedia == 2 ~ levels[2], 
      SocialMedia == 3 ~ levels[3],
      SocialMedia == 4 ~ levels[4],
      SocialMedia == 5 ~ levels[5],
      TRUE ~  levels[1]) %>% factor(levels))

q14_VertrauenAngepasst <- q14_VertrauenAngepasst %>% gather(Vergleichsdienste, Blogs, Influencer, Freunde, Mitarbeiter, Webseite, SocialMedia, Email, key = Quelle, value = Frequenz)


q14_VertrauenAngepasst$Quelle <- as.factor(q14_VertrauenAngepasst$Quelle)
q14_VertrauenAngepasst$Frequenz <- as.factor(q14_VertrauenAngepasst$Frequenz)
q14_VertrauenAngepasst$Quelle <- factor(q14_VertrauenAngepasst$Quelle, levels = columnNames1)
q14_VertrauenAngepasst$Frequenz <- factor(q14_VertrauenAngepasst$Frequenz, levels = levels)


q14_VertrauenJoined <- left_join(q14_VertrauenAngepasst,q23_Geschlecht, by = "Nummer")
q14_VertrauenJoined <- left_join(q14_VertrauenJoined, q24_Alter, by = "Nummer")

q14_VertrauenAngepasst %>% ggplot(aes(x=Frequenz, fill= Quelle)) + geom_bar(position = "dodge2") + xlab("Quelle") + ylab("Anzahl") + coord_flip()