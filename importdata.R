library(tidyverse)
dat <- read.csv2('D:\\Desktop\\BIT\\Bachelor\\06_Auswertung\\Angepasste Tabelle.csv')
q1_Suchmaschine <-  subset(dat, select=c("X01_1", "X01_2", "X01_3", "X01_4", "X01_5", "X01_6", "X01b"))
q1_Suchmaschine <-  q1_Suchmaschine %>% rename(google = X01_1, bing = X01_2, yahoo = X01_3, search.ch = X01_4 , escosia.org = X01_5, Andere =X01_6, TextAndere = X01b)

q2_3_4_5_bezahlteLinks <- subset(dat, select=c("X2","X3","X4","X5"))
q2_3_4_5_bezahlteLinks <- q2_3_4_5_bezahlteLinks %>% rename(ZweiteSeite = X2, ProzentBezahlteLinks = X3, ProzentKaufBezahlteLinks = X4, ZweckKauf = X5)

q6_VewendungSozialMedia <- subset(dat, select=c("X06_1", "X06_2", "X06_3", "X06_4", "X06_5", "X06_6", "X06_7", "X06_8", "X06b"))
q6_VewendungSozialMedia <-  q6_VewendungSozialMedia %>% rename(Facebook = X06_1, Instagram = X06_2, YouTube = X06_3, Twitter = X06_4 , LinkedIn = X06_5, Spnapchat =X06_6, TikTok =X06_7, Andere =X06_8, AndereText = X06b)

q7_SucheProduktSozialMedia <- subset(dat, select=c("X07_1", "X07_2"))
q7_SucheProduktSozialMedia <- q7_SucheProduktSozialMedia %>% rename(privat = X07_1, geschäftlich = X07_2)

q8_KommunikationUnternehmen <- subset(dat, select=c("X08_1", "X08_2"))
q8_KommunikationUnternehmen <- q8_KommunikationUnternehmen %>% rename(privat = X08_1, geschäftlich = X08_2)

q9_KommunikationUnternehmenhaufig <- subset(dat, select=c("X09_1", "X09_2"))
q9_KommunikationUnternehmenhaufig <- q9_KommunikationUnternehmenhaufig %>% rename(privat = X09_1, geschäftlich = X09_2)

q10_Unternehmeneinkauf <- subset(dat, select=c("X10_1", "X10_2"))
q10_Unternehmeneinkauf <- q10_Unternehmeneinkauf %>% rename(privat = X10_1, geschäftlich = X10_2)

q11_UnternehmeneinkaufKonkurrenz <- subset(dat, select=c("X11_1", "X11_2"))
q11_UnternehmeneinkaufKonkurrenz <- q11_UnternehmeneinkaufKonkurrenz %>% rename(privat = X11_1, geschäftlich = X11_2)

q12_likseinkauf <- subset(dat, select=c("X12_1", "X12_2"))
q12_likseinkauf <- q12_likseinkauf %>% rename(privat = X12_1, geschäftlich = X12_2)

q13_SucheProduktprivate <- subset(dat, select=c("X13.1_1", "X13.1_2", "X13.1_3", "X13.1_4", "X13.1_5", "X13.1_6"))
q13_SucheProduktprivate <- q13_SucheProduktprivate %>% rename(Vergleichsdienste = X13.1_1, Blogs = X13.1_2, Influencer = X13.1_3, Freunde = X13.1_4, Mitarbeiter = X13.1_5, SocialMedia = X13.1_6 )

q13_SucheProduktfirm <- subset(dat, select=c("X13.2_1", "X13.2_2", "X13.2_3", "X13.2_4", "X13.2_5", "X13.2_6"))
q13_SucheProduktfirm <- q13_SucheProduktfirm %>% rename(Vergleichsdienste = X13.2_1, Blogs = X13.2_2, Influencer = X13.2_3, Freunde = X13.2_4, Mitarbeiter = X13.2_5, SocialMedia = X13.2_6 )

q14_Vertrauen <- subset(dat, select=c("X14_1", "X14_2", "X14_3", "X14_4", "X14_5", "X14_6", "X14_7", "X14_8"))
q14_Vertrauen <-  q14_Vertrauen %>% rename(Vergleichsdienste = X14_1, Blogs = X14_2, Influencer = X14_3, Freunde = X14_4, Mitarbeiter = X14_5, Webseite = X14_6, SocialMedia = X14_7, Email = X14_8)

q15_EmailNewsletter <- subset(dat, select=c("X15_1_1", "X15_1_2", "X15_2_1", "X15_2_2","X15_3_1", "X15_3_2","X15_4_1", "X15_4_2","X15_5_1", "X15_5_2","X15_6_1", "X15_6_2"))
q15_EmailNewsletter <- q15_EmailNewsletter  %>% rename(IntressePrivat = X15_1_1, IntesseFirma = X15_1_2,
                                                       AktionenPrivat = X15_2_1, AktionenFirma = X15_2_2,
                                                       AngebotPrivat = X15_3_1, AngebotFirma = X15_3_2,
                                                       InhaltPrivat = X15_4_1, InhaltFirma = X15_4_2,
                                                       TrendsPrivat = X15_5_1, TrendsFirma = X15_5_2,
                                                       UnternehmenPrivat = X15_6_1, UnternehmenFirma = X15_6_2)

q16_NichtAbonniert <- subset(dat, select=c("X16_1", "X16_2"))
q16_NichtAbonniert <- q16_NichtAbonniert %>% rename(privat = X16_1, geschäftlich = X16_2)

q17_BevorzugteKommunikation <- subset(dat, select=c("X17_1", "X17_2", "X17_3", "X17_4","X17_5", "X17_6","X17_7", "X17_8","X17_9", "X17_10","X17_10_TEXT"))
q17_BevorzugteKommunikation <- q17_BevorzugteKommunikation  %>% rename(Email = X17_1, Brief = X17_2,
                                                       MobileApp = X17_3, SocialMedia = X17_4,
                                                       Messanger = X17_5, SMS = X17_6,
                                                       Telefon = X17_7, Chatbots = X17_8,
                                                       ServiceProtale = X17_9, Andere = X17_10,
                                                       AndereText = X17_10_TEXT)

q18_QuellenSuche <- subset(dat, select=c("X18_1", "X18_2", "X18_3", "X18_4","X18_5", "X18_6","X18_7", "X18_8","X18_9", "X18_10", "X18_11","X18b"))
q18_QuellenSuche <- q18_QuellenSuche  %>% rename(Suchmaschine = X18_1, Vergleichsdienst = X18_2,
                                                 Blogs = X18_3, Influencer = X18_4,
                                                 Facebook = X18_5, Instagram = X18_6,
                                                 YouTube = X18_7, Twitter = X18_8,
                                                 Linkedin = X18_9, TikTok = X18_10,
                                                 Andere = X18_11, AndereText = X18b )

q19_QuellenKauf <- subset(dat, select=c("X19_1", "X19_2", "X19_3", "X19_4","X19_5", "X19_6","X19_7", "X19_8","X19_9", "X19_10"))
q19_QuellenKauf <- q19_QuellenKauf  %>% rename(Suchmaschine = X19_1, Vergleichsdienst = X19_2,
                                                 Blogs = X19_3, Influencer = X19_4,
                                                 Facebook = X19_5, Instagram = X19_6,
                                                 YouTube = X19_7, Twitter = X19_8,
                                                 Linkedin = X19_9, TikTok = X19_10)

q20_MediumAktuell <- subset(dat, select=c("X20", "X20_5_TEXT"))
q20_MediumAktuell <- q20_MediumAktuell %>% rename(Medium = X20, Andere = X20_5_TEXT)

q21_MediumZukunft <- subset(dat, select=c("X21", "X21_5_TEXT"))
q21_MediumZukunft <- q21_MediumZukunft %>% rename(Medium = X21, Andere = X21_5_TEXT)

q22_MediumVorteil <- subset(dat, select=c("X22_1", "X22_2", "X22_3", "X22_4", "X22_5", "X22_5_TEXT"))
q22_MediumVorteil <- q22_MediumVorteil %>% rename(Text = X22_1, Bilder = X22_2,
                                                  Audio = X22_3, Video =X22_4,
                                                 Andere = X22_5, AndereText = X22_5_TEXT)

q23_Geschlecht <- subset(dat, select=c("X23"))
q23_Geschlecht <- q23_Geschlecht %>% rename(Geschlecht = X23)

q24_Alter <- subset(dat, select=c("X24"))
q24_Alter <- q24_Alter %>% rename(Alter = X24)