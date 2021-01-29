
#### SERVER ----


## Fonctions ----

# réglès pour 'formattable' (Q2A)
sign_formatter <- formatter("span",
  style = x ~ formattable::style(
    color = ifelse(x > 0, "green", ifelse(x < 0, "red", "black"))
  )
)
evolution_formatter <- formatter("span",
  style = x ~ formattable::style(
    font.weight = "bold",
    color = ifelse(x == "positif", "green", ifelse(x == "negatif", "red", "black"))
  ),
  x ~ icontext(
    ifelse(x == "positif", "plus", ifelse(x == "negatif", "minus", "arrow-right")),
    text = list(NULL)
  )
)
my_color_bar <- function(color) {
  mainpart <- color_bar(color)
  function(x) {
    start <- x[-length(x)]
    last <- x[length(x)]
    c(mainpart(start), last)
  }
}


####

server <- function(input, output) {

  #### ETL ----

  # 1A ----
  data_question1a <- read.table(
    file = "data/question1a.csv",
    header = TRUE,
    sep = ","
  )

  # 1B ----
  data_question1b <- read.table(
    file = "data/question1b.csv",
    header = TRUE,
    sep = ","
  )
  data_question1b$ca_global <- as.numeric(data_question1b$ca_global)
  data_question1b <- subset(data_question1b, (data_question1b$ca_global > quantile(data_question1b$ca_global, c(0.005))) &
    (data_question1b$ca_global < quantile(data_question1b$ca_global, c(0.995))))

  # 1C ----
  data_question1c <- read.table(
    file = "data/question1c.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = F
  )

  data_pyramide <- as.data.frame(table(
    data_question1c$tranche_age,
    data_question1c$sexe
  ))
  categorie <- data_pyramide[1:9, 1]
  femmes <- data_pyramide[1:9, 3]
  hommes <- data_pyramide[10:18, 3]
  data_pyramide <- data.frame(hommes, femmes, categorie)
  data_pyramide$categorie <- revalue(data_pyramide$categorie, c("Under 18" = "0-18"))
  data_pyramide$categorie <- factor(data_pyramide$categorie, levels = c("0-18", "18-28", "28-38", "38-48", "48-58", "58-68", "68-78", "78-88", "88-98"))

  # 2A ----
  data_question2a <- read.table(
    file = "data/question2a.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = F,
    na.strings = "NA"
  )

  data_question2a <- data.frame(transform(
    data_question2a,
    "indices" = factor(ifelse(
      (var_client > 0 & var_ttc > 0), "positif",
      ifelse((var_client < 0 & var_ttc < 0), "negatif", "moyen")
    ), levels = c("positif", "moyen", "negatif"))
  ))

  data_question2a <- data_question2a[order(data_question2a$indices), ]

  total_nb_client <- sum(as.integer(data_question2a$nb_client), na.rm = TRUE)
  total_totalttcn2 <- sum(as.numeric(data_question2a$totalttcn2), na.rm = TRUE)
  total_totalttcn1 <- sum(as.numeric(data_question2a$totalttcn1), na.rm = TRUE)
  total_n2_client <- sum(as.integer(data_question2a$n2_client), na.rm = TRUE)
  total_n1_client <- sum(as.integer(data_question2a$n1_client), na.rm = TRUE)
  total_n1_client_p <- mean(as.numeric(data_question2a$n1_client_), na.rm = TRUE)
  total_diff_ttc <- total_totalttcn2 - total_totalttcn1
  total_var_ttc <- round(total_totalttcn2 / total_totalttcn1 * 100 - 100, 2)
  total_var_client <- round(total_n2_client / total_n1_client * 100 - 100, 2)
  total_indice <- NULL
  if (total_var_client > 0 & total_diff_ttc > 0) {
    total_indice <- "positif"
  } else if (total_var_client < 0 & total_diff_ttc < 0) {
    total_indice <- "negatif"
  } else {
    total_indice <- "moyen"
  }
  total <- data.frame(
    "TOTAL",
    total_nb_client,
    total_totalttcn2,
    total_totalttcn1,
    total_n2_client,
    total_n1_client,
    total_n1_client_p,
    total_diff_ttc,
    total_var_ttc,
    total_var_client,
    total_indice
  )
  names(total) <- names(data_question2a)
  data_question2a <- rbind(data_question2a, total)
  colnames(data_question2a) <- c(
    "Code Magasin",
    "Nombre Adherent",
    "Total TTC N-2",
    "Total TTC N-1",
    "Client Actif N-2",
    "Client Actif N-1",
    "Part des Client Actif N-1",
    "Différence Total TTC",
    "Evolution Total TTC",
    "Evolution client Actif",
    "Indice d'Evolution"
  )
  print(head(data_question2a))

  # 2B ----
  data_question2b <- read.table(
    file = "data/question2b.csv",
    header = TRUE,
    sep = ","
  )

  # 3A ----
  data_question3a <- read.table(
    file = "data/question3a.csv",
    header = TRUE,
    sep = ","
  )
  data_question3a <- spread(data_question3a, annee, ca_global)
  colnames(data_question3a) <- c("codeunivers", "ca_2016", "ca_2017")

  # 3B ----
  data_question3b <- read.table(
    file = "data/question3b.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = F
  )







  # intro ----

  output$downloadData <- downloadHandler(
    filename = "Projet Transverse SQL.pdf",
    content = function(file) {
      file.copy("www/Projet Transverse SQL.pdf", file)
    }
  )



  # Q1A ----

  output$code_1a <- renderPrint({
    cat(
      "-- calculer colonne :",
      "ALTER TABLE client ADD vip_adherent varchar(69);",
      "UPDATE client SET vip_adherent = (CASE ",
      "  WHEN vip = 1 then 'VIP'",
      "  WHEN vip != 1 and  extract (year from datedebutadhesion) = 2017 then 'NEW_N1'",
      "  WHEN vip != 1 and  extract (year from datedebutadhesion) = 2016 then 'NEW_N2'",
      "  WHEN vip != 1 and  extract (year from datedebutadhesion) < 2016 and datefinadhesion  > '2018-01-01' then 'ADHÉRENT' ",
      "  WHEN vip != 1 and  extract (year from datedebutadhesion) < 2016 and datefinadhesion < '2018-01-01' then 'CHURNER'",
      "  ELSE null",
      "END);",
      " ",
      "-- output table :",
      "SELECT vip_adherent, COUNT(*) AS count",
      "FROM client",
      "GROUP BY vip_adherent ;",
      sep = "\n"
    )
  })

  output$plot_1a <- renderPlotly(
    fig <- plot_ly(
      labels = as.character(data_question1a[, "vip_adherent"]),
      values = as.numeric(data_question1a[, "count"]),
      type = "pie"
    ) %>%
      layout(
        title = list(text = "Répartition Adhérant / VIP", x = 0.985, y = 1),
        titlefont = list(size = 13),
        xaxis = list(title = "CA client annuel", titlefont = list(size = 12)),
        yaxis = list(title = "")
      )
  )

  # Q1B ----

  output$code_1b <- renderPrint({
    cat(
      "SELECT SUM(tic_totalttc) AS ca_global,client.idclient, EXTRACT (YEAR FROM tic_date) AS annee",
      "FROM entete_ticket ",
      "JOIN client ON client.idclient = entete_ticket.idclient",
      "WHERE EXTRACT (YEAR FROM tic_date) BETWEEN 2016 AND 2017",
      "GROUP BY client.idclient, annee",
      "ORDER BY client.idclient, annee ;",
      sep = "\n"
    )
  })

  output$plot_1b <- renderPlotly(
    plot_ly(type = "box") %>%
      add_boxplot(
        x = ~ data_question1b[which(data_question1b$annee == 2016), ]$ca_global,
        boxpoints = FALSE, name = "2016"
      ) %>%
      add_boxplot(
        x = ~ data_question1b[which(data_question1b$annee == 2017), ]$ca_global,
        boxpoints = FALSE, name = "2017"
      ) %>%
      layout(
        title = list(text = "CA TOTAL client par année", x = 0.5, y = 1),
        titlefont = list(size = 13),
        xaxis = list(
          title = "", # "CA client annuel",
          titlefont = list(size = 12)
        ),
        yaxis = list(title = "")
      )
  )


  # Q1C ----

  output$code_1c <- renderPrint({
    cat(
      "ALTER TABLE client ADD tranche_age varchar(10);",
      "UPDATE client set tranche_age = (CASE",
      "WHEN age < 18 THEN  'Under 18' ",
      "WHEN age BETWEEN 18 AND 28 THEN '18-28'",
      "WHEN age BETWEEN 28 AND 38 THEN '28-38'",
      "WHEN age BETWEEN 38 AND 48 THEN '38-48'",
      "WHEN age BETWEEN 48 AND 58 THEN '48-58'",
      "WHEN age BETWEEN 58 AND 68 THEN '58-68'",
      "WHEN age BETWEEN 68 AND 78 THEN '68-78'",
      "WHEN age BETWEEN 78 AND 88 THEN '78-88'",
      "WHEN age BETWEEN 88 AND 98 THEN '88-98'",
      "ELSE null",
      "END);",
      "",
      "ALTER TABLE client ADD sexe varchar(10);",
      "UPDATE client SET sexe= (CASE",
      "WHEN civilite ='monsieur' THEN 'homme'",
      "WHEN civilite ='madame' THEN 'femme'",
      "ELSE null",
      "END);",
      "",
      "SELECT sexe, age  FROM client",
      "WHERE tranche_age IS NOT NULL",
      sep = "\n"
    )
  })

  output$plot_1c <- renderPlot(
    pyramid(
      data = data_pyramide,
      main = "Pyramide des Ages des Clients",
      Laxis = seq(from = 0, to = 75000, by = 15000),
      Llab = "Hommes", Rlab = "Femmes",
      Lcol = "navy", Ldens = 10, Rcol = "darkred", Rdens = 10,
      AxisFM = "d", AxisBM = ","
    )
  )

  # Q2A ----

  output$code_2a <- renderPrint({
    cat(
      "--- code_magasin----",
      "DROP TABLE IF EXISTS MAGASIN;",
      "CREATE TABLE MAGASIN ();",
      "ALTER TABLE magasin ADD code_mag varchar(3);",
      "INSERT INTO magasin (code_mag)",
      "SELECT codesociete FROM ref_magasin ;",
      "",
      "--- nbe_client_magasin----",
      "ALTER TABLE magasin ADD nb_client int;",
      "UPDATE magasin ",
      "SET nb_client = ",
      "(SELECT COUNT(idclient)",
      "FROM client",
      "WHERE magasin.code_mag = client.magasin",
      "GROUP BY magasin)",
      "",
      "--- nbe client actif N-2--- ",
      "ALTER TABLE magasin ADD N2_client int; ",
      "UPDATE magasin ",
      "SET N2_client = ",
      "(SELECT COUNT(DISTINCT(idclient))",
      "FROM entete_ticket",
      "WHERE magasin.code_mag = entete_ticket.mag_code",
      "AND EXTRACT (YEAR FROM tic_date) = 2016);",
      "",
      "----- nbe client actif N-1----",
      "ALTER TABLE magasin ADD N1_client int;",
      "UPDATE magasin ",
      "SET N1_client = ",
      "(SELECT COUNT(DISTINCT(idclient))",
      "FROM entete_ticket",
      "WHERE magasin.code_mag = entete_ticket.mag_code",
      "AND EXTRACT (YEAR FROM tic_date) = 2017);",
      "",
      "-- % client n-2----",
      "ALTER TABLE magasin ADD n2_client_p FLOAT;",
      "UPDATE magasin SET n2_client_p = ( (n2_client/(NULLIF(n1_client,0)+ NULLIF(n2_client,0) :: FLOAT))*100 ::FLOAT)",
      "--%client n-1----",
      "ALTER TABLE magasin ADD n1_client_p FLOAT;",
      "UPDATE magasin SET n1_client_p = ( (n1_client/(NULLIF(n1_client,0)+ NULLIF(n2_client,0) :: FLOAT))*100 ::FLOAT)",
      "",
      "----- total TTC N-2-----",
      "ALTER TABLE magasin ADD totalttcn2 FLOAT;",
      "UPDATE magasin SET totalttcn2 = ",
      "(SELECT SUM (tic_totalttc)",
      "FROM entete_ticket ",
      "WHERE magasin.code_mag = entete_ticket.mag_code ",
      "AND EXTRACT (YEAR FROM tic_date) = 2016)",
      "",
      "-------Total TTC N-1-----",
      "ALTER TABLE magasin ADD totalttcn1 FLOAT;",
      "UPDATE magasin SET totalttcn1 = ",
      "(SELECT SUM (tic_totalttc)",
      "FROM entete_ticket ",
      "WHERE magasin.code_mag = entete_ticket.mag_code ",
      "AND EXTRACT (YEAR FROM tic_date) = 2017)	;	",
      "",
      "---- différence entre N-2 & N -1----",
      "ALTER TABLE magasin ADD diff_ttc FLOAT ",
      "UPDATE magasin SET diff_ttc = totalttcn1 - totalttcn2 ::FLOAT",
      "",
      "--1 indice d'évolution ttc----",
      "ALTER TABLE magasin ADD var_ttc FLOAT ",
      "UPDATE magasin SET var_ttc = (((totalttcn1 - totalttcn2) / totalttcn2 :: FLOAT)*100 ::FLOAT)",
      "--2 indice d'évolution client----",
      "ALTER TABLE magasin ADD var_client FLOAT ",
      "UPDATE magasin SET var_client = (((n1_client - n2_client) / (NULLIF(n2_client,0)) :: FLOAT)*100 ::FLOAT)",
      "",
      "---- extraction tableau des données----",
      "COPY (SELECT * FROM magasin",
      " ) To 'C:/ Users/Public/Projet_Transverse/data/magasin.csv' With CSV DELIMITER ',' HEADER;",
      sep = "\n"
    )
  })

  output$plot_2a <- renderFormattable({
    formattable(
      data_question2a,
      list(
        "Indice d'Evolution" = evolution_formatter,
        "Evolution client Actif" = sign_formatter,
        "Evolution Total TTC" = sign_formatter,
        "Différence Total TTC" = sign_formatter,
        "Nombre Adherent" = my_color_bar("orange")
      )
    )
  })


  # Q2B ----

  output$code_2b <- renderPrint({
    cat(
      "-- fonction pour distance :",
      "CREATE OR REPLACE FUNCTION calculate_KM_distance(lat1 float, lon1 float, lat2 float, lon2 float)",
      " RETURNS float AS $distance$ ,",
      "  DECLARE",
      "   distance float = 0;",
      "   radlat1 float;",
      "   radlat2 float;",
      "   theta float;",
      "   radtheta float;",
      "  BEGIN",
      "   IF lat1 = lat2 OR lon1 = lon2",
      "    THEN RETURN distance;",
      "   ELSE",
      "    radlat1 = pi() * lat1 / 180;",
      "    radlat2 = pi() * lat2 / 180;",
      "    theta = lon1 - lon2;",
      "    radtheta = pi() * theta / 180;",
      "    distance = sin(radlat1) * sin(radlat2) + cos(radlat1) * cos(radlat2) * cos(radtheta);",
      "    IF distance > 1 THEN distance = 1; END IF;",
      "    distance = acos(distance);",
      "    distance = distance * 180 / pi();",
      "    distance = distance * 60 * 1.1515;",
      "    distance = distance * 1.609344 ;",
      "    RETURN distance;",
      "  END IF;",
      " END;",
      "$distance$ LANGUAGE plpgsql;",
      " ",
      "-- calculer distance :",
      "ALTER TABLE client ADD distance_mag float  ;",
      "update client ",
      "set distance_mag = calculate_KM_distance(client_latitude, client_longitude, mag_latitude, mag_longitude);",
      " ",
      "-- créer groupes :",
      "ALTER TABLE client ADD distance_group varchar(69);",
      "UPDATE client SET distance_group = (",
      " CASE ",
      "  WHEN distance_mag between 0 and 5 then 'Distance:0-5km'",
      "  WHEN distance_mag between 5 and 10 then 'Distance:5-10km'",
      "  WHEN distance_mag between 10 and 20 then 'Distance:10-20km'",
      "  WHEN distance_mag between 20 and 50 then 'Distance:20-50km'",
      "  WHEN distance_mag > 50 then 'Distance:+50km'",
      "  ELSE null",
      "END);",
      " ",
      "-- output table :",
      "SELECT distance_group, COUNT(*) AS count",
      "FROM client",
      "GROUP BY distance_group ",
      "ORDER BY (",
      " CASE distance_group",
      "  WHEN 'Distance:0-5km' THEN 1",
      "  WHEN 'Distance:5-10km' THEN 2",
      "  WHEN 'Distance:10-20km' THEN 3",
      "  WHEN 'Distance:20-50km' THEN 4",
      "  WHEN 'Distance:+50km' THEN 5",
      "  WHEN null THEN 6",
      "  END",
      ") ASC ;",
      sep = "\n"
    )
  })

  output$plot_2b <- renderPlotly(
    fig <- plot_ly(
      x = as.numeric(data_question2b[, "count"]),
      y = factor(as.character(data_question2b[, "distance_group"]), levels = c("Distance:0-5km", "Distance:5-10km", "Distance:10-20km", "Distance:20-50km", "Distance:+50km")),
      type = "bar",
      orientation = "h"
    ) %>%
      layout(
        title = list(text = "Distance Client / Magasin", x = 0.9, y = 1),
        titlefont = list(size = 13),
        xaxis = list(title = "Nbre de Clients", titlefont = list(size = 12)),
        yaxis = list(title = "")
      )
  )

  # Q3A ----

  output$code_3a <- renderPrint({
    cat(
      "SELECT SUM(lignes_ticket.total) AS ca_global, ref_article.codeunivers, EXTRACT(YEAR FROM entete_ticket.tic_date) AS annee",
      "FROM entete_ticket ",
      "JOIN lignes_ticket ON lignes_ticket.idticket  = entete_ticket.idticket ",
      "JOIN ref_article ON ref_article.codearticle = lignes_ticket.idarticle",
      "WHERE extract(year FROM entete_ticket.tic_date) BETWEEN 2016 AND 2017",
      "GROUP BY ref_article.codeunivers, annee",
      "ORDER BY ref_article.codeunivers, annee ;",
      sep = "\n"
    )
  })

  output$plot_3a <- renderPlotly(
    fig <- plot_ly(data_question3a,
      x = ~codeunivers, y = as.numeric(data_question3a[, "ca_2016"]),
      type = "bar", name = "2016", marker = list(color = "rgb(243,126,100)")
    )
    %>% add_trace(y = as.numeric(data_question3a[, "ca_2017"]), name = "2017", marker = list(color = "rgb(143,248,97)"))
      %>% layout(
        title = list(text = "CA par Univers", x = 0.985, y = 1),
        titlefont = list(size = 13),
        xaxis = list(title = "Univers", titlefont = list(size = 12)),
        yaxis = list(title = "")
      )
  )


  # Q3B ----

  output$code_3b <- renderPrint({
    cat(
      "SELECT SUM(lignes_ticket.margesortie) as rentability , ref_article.codeunivers, ref_article.codefamille",
      "FROM lignes_ticket ",
      "JOIN ref_article ON ref_article.codearticle = lignes_ticket.idarticle",
      "GROUP BY ref_article.codeunivers, ref_article.codefamille",
      "ORDER BY SUM(lignes_ticket.margesortie) DESC ;",
      sep = "\n"
    )
  })

  data3b <- reactive({
    TOP5 <- data_question3b %>%
      filter(codeunivers == input$select)
    TOP5 <- TOP5 %>%
      arrange(desc(rentability))
    TOP5 <- head(TOP5, 5)

    TOP5$TOP <- 1:nrow(TOP5)
    TOP5$codeunivers <- NULL
    TOP5 <- TOP5[, c(3, 2, 1)]
    TOP5
  })

  output$plot_3b_table <- renderFormattable({
    formattable(
      data3b(),
      list()
    )
  })

  output$plot_3b_hist <- renderPlotly({
    fig <- plot_ly(data3b(),
      x = ~codefamille, y = ~rentability,
      type = "bar", name = "2016", marker = list(color = "rgb(243,126,100)")
    ) %>%
      layout(
        title = list(text = paste0(input$select, " - Top familles"), x = 0.9, y = 1),
        titlefont = list(size = 13),
        xaxis = list(title = "Code famille", titlefont = list(size = 12)),
        yaxis = list(title = "")
      )
  })
}
