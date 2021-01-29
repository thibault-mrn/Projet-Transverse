#### REQUIREMENTS ----

library(shiny)
library(shinydashboard)

library(plotly)
library(ggplot2)
library(pyramid)
library(formattable)
library(rhandsontable)

library(plyr)
library(tidyr)
library(scales)
library(reshape2)


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

#### UI ----

ui <- dashboardPage(


  ## Header & Sidebar -----

  dashboardHeader(
    title = "PROJET TRANSVERSE",

    dropdownMenu(
      type = "notifications",
      icon = icon("users"),
      headerText = "This dashboard was made by :",
      notificationItem(
        text = "Thibault Maurin",
        icon("user"),
        status = "primary"
      ),
      notificationItem(
        text = "Alexandre Jaquet",
        icon("user"),
        status = "success"
      ),
      notificationItem(
        text = "Nesrine Hasbellaoui",
        icon = icon("user"),
        status = "warning"
      ),
      notificationItem(
        text = "Gregory Martin",
        icon = icon("user"),
        status = "danger"
      )
    )
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "page_intro", icon = icon("th")),
      menuItem("Etude global",
        tabName = "page_1", icon = icon("chevron-circle-right"),
        menuSubItem("Répartition Adhérant / VIP",
          tabName = "q1a",
          icon = icon("caret-right")
        ),
        menuSubItem("CA par client N-2 vs N-1",
          tabName = "q1b",
          icon = icon("caret-right")
        ),
        menuSubItem("Répartition par Age x Sexe",
          tabName = "q1c",
          icon = icon("caret-right")
        )
      ),
      menuItem("Etude par magasin",
        tabName = "page_2", icon = icon("chevron-circle-right"),
        menuSubItem("Résultat par Magasin",
          tabName = "q2a",
          icon = icon("caret-right")
        ),
        menuSubItem("Distance CLIENT / MAGASIN",
          tabName = "q2b",
          icon = icon("caret-right")
        )
      ),
      menuItem("Etude par univers",
        tabName = "page_3", icon = icon("chevron-circle-right"),
        menuSubItem("Etudes par Univers",
          tabName = "q3a",
          icon = icon("caret-right")
        ),
        menuSubItem("Top par Univers",
          tabName = "q3b",
          icon = icon("caret-right")
        )
      )
    )
  ),


  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")),

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),

    tabItems(

      # Introduction ----

      tabItem(
        tabName = "page_intro",

        box(
          width = 12,
          solidHeader = FALSE,
          collapsible = FALSE,

          box(
            width = 9, status = "success",
            solidHeader = FALSE,
            h3("PROJET TRANSVERSE", style = "font-weight: bold;"),
            h4("MBA BIG DATA - SQL")
          ),

          box(
            width = 3, status = "success", class = "mbalogo",
            solidHeader = FALSE,
            img(
              src = "mba-esg-logo.png",
              width = "100%",
              class = "mba_logo",
              align = "center"
            )
          ),

          box(
            width = 12, status = "danger",
            downloadLink("downloadData", "Cliquer ICI pour télécharger l'énoncé au format pdf")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Objectif &nbsp;", "</b>")),
            solidHeader = FALSE,
            p(" - Manipuler et analyser de la Data sous SQL"),
            p(" - Programmer en SQL"),
            p(" - A rendre avant le 31 janvier 23h (un jour de retard = 2 points en moins)")
          ),
          HTML("<br/>"),
          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Rendu &nbsp;", "</b>")),
            solidHeader = FALSE,
            p("Un fichier .sql commenté avec les numéros d’exercices + un rapport comprenant les graphiques effectués sur
                l’outil de data viz de votre choix. "),
            p("Bonus : un lien vers un répertoire GIT avec le doc .sql et le rapport.")
          ),
          HTML("<br/>"),
          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Contexte &nbsp;", "</b>")),
            solidHeader = FALSE,
            p("Une société X a envoyé ces données client ainsi que les achats sur l'année N-2 (2016) et N-1 (2017).")
          ),
          HTML("<br/>"),

          box(
            width = 12, status = "primary", class = "archi",
            title = HTML(paste0("<b>", "&nbsp; Architecture data &nbsp;", "</b>")),
            solidHeader = FALSE,
            img(
              src = "archi_transverse.png",
              width = "100%",
              class = "architecture",
              align = "center"
            )
          )
        )
      ),


      # Question 1A ----

      tabItem(
        tabName = "q1a",

        box(
          width = 12, status = "info",
          title = HTML(paste0("<b>", "1a. &nbsp;&nbsp; Répartition Adhérant / VIP ", "</b>")),
          solidHeader = FALSE,

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Énoncé &nbsp;", "</b>")),

            p("Constituer un camembert suivant la répartition suivante :"),
            p("- VIP : client étant VIP (VIP = 1)"),
            p("- NEW_N2 : client ayant adhéré au cours de l'année N-2 (date début adhésion)"),
            p("- NEW_N1 : client ayant adhéré au cours de l'année N-1 (date début adhésion)"),
            p("- ADHÉRENT : client toujours en cours d'adhésion (date de fin d'adhésion > 2018/01/01)"),
            p("- CHURNER : client ayant churner (date de fin d'adhésion < 2018/01/01)"),
            p(""),
            p("Note : le critère le plus au-dessus est prioritaire, exemple : un client étant VIP, et ayant adhéré sur
                  l'année N-1 sera compté comme étant VIP")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; SQL query &nbsp;", "</b>")),
            verbatimTextOutput("code_1a")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; SQL response &nbsp;", "</b>")),

            rhandsontable(
              data_question1a,
              readOnly = TRUE,
              stretchH = "all"
            ) %>%
              hot_col("count", format = "0.000 K")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Visualisation &nbsp;", "</b>")),
            plotlyOutput("plot_1a", height = 250),
            p(" ",
              class = "comment1"
            ),
            p("Sur ce graphique il était demandé de représenter la totalité des clients, 
              tout en les répartissant par leurs statuts en prenant compte les différents niveaux. En effet le statut de base est churner, ensuite Adhérent (séparé en N-2 et N-1),  puis VIP.",
              class = "comment"
            ),
            p("Nous obtenons donc la répartition suivante. Nous voyons bien que plus l’effectif de client avance dans cette répartition de statut plus ce dernier est réduit.",
              class = "comment"
            )
          )
        )
      ),

      # Question 1B ----

      tabItem(
        tabName = "q1b",

        box(
          width = 12, status = "info",
          title = HTML(paste0("<b>", "1b. &nbsp;&nbsp; Comportement du CA GLOBAL par client N-2 vs N-1 ", "</b>")),
          solidHeader = FALSE,

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Énoncé &nbsp;", "</b>")),

            p("Constituer une boîte à moustache pour chaque année (N-2 et N-1) comparant le CA TOTAL (TTC) des
                  clients (sommer les achats par client par années)")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; SQL query &nbsp;", "</b>")),
            verbatimTextOutput("code_1b")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; SQL response &nbsp;", "</b>")),

            rhandsontable(
              data_question1b[0:10, ],
              readOnly = TRUE,
              stretchH = "all"
            )
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Visualisation &nbsp;", "</b>")),
            plotlyOutput("plot_1b", height = 250),
            p(" ",
              class = "comment1"
            ),
            p("Ce graphique représente le comportement du CA global par client en 2016 et 2017. Nous pouvons observer une différence très légère sur la CA client médian d’environ 12€.",
              class = "comment"
            ),
            p("Afin de réaliser ce graphique il était important de retirer les valeurs aberrantes qui auraient pu fausser cette analyse.",
              class = "comment"
            )
          )
        )
      ),

      # Question 1C ----

      tabItem(
        tabName = "q1c",

        box(
          width = 12, status = "info",
          title = HTML(paste0("<b>", "1c. &nbsp;&nbsp; Répartition par âge x sexe ", "</b>")),
          solidHeader = FALSE,

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Énoncé &nbsp;", "</b>")),

            p("Constituer un graphique montrant la répartition par âge x sexe sur l'ensemble des clients.")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; SQL query &nbsp;", "</b>")),
            verbatimTextOutput("code_1c")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; SQL response &nbsp;", "</b>")),

            rhandsontable(
              data_question1c[0:12, ],
              readOnly = TRUE,
              stretchH = "all"
            )
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Visualisation &nbsp;", "</b>")),
            plotOutput("plot_1c", height = 369),
            p("Sur ce graphique représentant les populations d’homme et de femme réparties en tranche d’âge, nous pouvons voir que la majorité des clients masculins et féminins ont entre 48 et 58 ans. Les femmes sont surreprésentées en comparaison aux hommes.",
              class = "comment"
            )
          )
        )
      ),


      # Question 2A ----

      tabItem(
        tabName = "q2a",

        box(
          width = 12, status = "info",
          title = HTML(paste0("<b>", "1c. &nbsp;&nbsp; Résultat par Magasin ", "</b>")),
          solidHeader = FALSE,

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Énoncé &nbsp;", "</b>")),

            p("Résultat par magasin (+1 ligne Total)."),
            p("Constituer un tableau reprenant les données suivantes :"),
            p("- MAGASIN"),
            p("- NOMBRE DE CLIENT RATTACHE AU MAGASIN (avec une color_bar en fonction de la quantité)"),
            p("- Nombre de client actif sur N-2"),
            p("- Nombre de client actif sur N-1"),
            p("- % CLIENT N-2 vs N-1 (couleur police : vert si positif, rouge si négatif)"),
            p("- TOTAL_TTC N-2"),
            p("- TOTAL_TTC N-1"),
            p("- Différence entre N-2 et N-1 (couleur police : vert si positif, rouge si négatif)"),
            p("- indice évolution (icône de satisfaction : positif si %client actif évolue et total TTC aussi, négatif si
diminution des 2 indicateurs, moyen seulement l'un des deux diminue)"),
            p("Note : on effectuera un trie sur l'indice d'évolution (les positifs en haut, les négatifs en bas).")
          ),

          box(
            width = 12, status = "primary",
            collapsible = TRUE, collapsed = TRUE,
            title = HTML(paste0("<b>", "&nbsp; SQL query &nbsp;", "</b>")),
            verbatimTextOutput("code_2a")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; SQL response &nbsp;", "</b>")),

            rhandsontable(
              data_question2a[0:10, ],
              readOnly = TRUE,
              stretchH = "all"
            )
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Visualisation &nbsp;", "</b>")),
            formattableOutput("plot_2a"),
            p(" ",
              class = "comment1"
            ),
            p("Ce tableau affiche la répartition des clients par magasin tout en nous montrant l’évolution des effectifs et de la rentabilité entre 2016 et 2017. Les évolutions positives sont représentées en vert, les évolutions négatives sont représentées en rouge.",
              class = "comment"
            )
          )
        )
      ),

      # Question 2B ----

      tabItem(
        tabName = "q2b",

        box(
          width = 12, status = "info",
          title = HTML(paste0("<b>", "2b. &nbsp;&nbsp; Distance CLIENT / MAGASIN", "</b>")),
          solidHeader = FALSE,

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Énoncé &nbsp;", "</b>")),

            p("Le but étant de calculer la distance qui existe entre le magasin et le client. Les infos disponible pour le
          moment sont : "),
            p("- la ville du magasin"),
            p("- le code insee du client"),
            p(" Il faut donc télécharger les données GPS des villes et code-insee pour pouvoir calculer la distance :"),
            p("https://public.opendatasoft.com/explore/dataset/correspondance-code-insee-code-postal/"),
            p(" Une fois les données acquises, il faut lier les données GPS composé de la latitude et de la longitude au
          client et au magasin. (constituer pour chaque client et chaque magasin 2 colonnes : latitude et
                                 longitude)."),
            p("Créer une fonction qui détermine la distance entre 2 points. La fonction doit prendre 4 variable en
          compte : latitude1, longitude1, latitude2, longitude2
          pour savoir si la fonction est correct : http://www.lexilogos.com/calcul_distances.html"),
            p("Constituer une représentation (tableau ou graphique --> au choix) représentant le nombre de client par
          distance : 0 à 5km, 5km à 10km, 10km à 20km, 20km à 50km, plus de 50km")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; SQL query &nbsp;", "</b>")),
            verbatimTextOutput("code_2b")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; SQL response &nbsp;", "</b>")),

            rhandsontable(
              data_question2b,
              readOnly = TRUE,
              stretchH = "all"
            ) %>%
              hot_col("count", format = "0.000 K")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Visualisation &nbsp;", "</b>")),
            plotlyOutput("plot_2b", height = 250),
            p(" ",
              class = "comment1"
            ),
            p("Grâce à ce graphique nous pouvons voir que les clients sont majoritairement à moins de 5km des magasins. Plus la distance augmente moins il y a de client autour des magasins. 
              On remarque cependant que la tendance s’inverse lorsque les clients sont à plus de 50km.",
              class = "comment"
            )
          )
        )
      ),



      # Question 3A ----

      tabItem(
        tabName = "q3a",

        box(
          width = 12, status = "info",
          title = HTML(paste0("<b>", "1c. &nbsp;&nbsp; Étude par Univers ", "</b>")),
          solidHeader = FALSE,

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Énoncé &nbsp;", "</b>")),

            p("Constituer un histogramme N-2 / N-1 évolution du CA par univers.")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; SQL query &nbsp;", "</b>")),
            verbatimTextOutput("code_3a")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; SQL response &nbsp;", "</b>")),

            rhandsontable(
              data_question3a,
              readOnly = TRUE,
              stretchH = "all"
            )
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Visualisation &nbsp;", "</b>")),
            plotlyOutput("plot_3a", height = 250),
            p(" ",
              class = "comment1"
            ),
            p("Ce graphique représente une comparaison des CA de chaque Univers entre 2016 et 2017. La tendance entre les deux années est minime",
              class = "comment"
            ),
            p("On remarque que U1 atteint quasiment 80 Million au contraire du CA d'affaire de U0 qui est minime. ",
              class = "comment"
            ),
            p("La présence de coupon a été maintenue pour information. ",
              class = "comment"
            )
          )
        )
      ),

      # Question 3B ----

      tabItem(
        tabName = "q3b",

        box(
          width = 12, status = "info",
          title = HTML(paste0("<b>", "1c. &nbsp;&nbsp; Top par Univers ", "</b>")),
          solidHeader = FALSE,

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Énoncé &nbsp;", "</b>")),

            p("Afficher le top 5 des familles les plus rentable par univers (en fonction de la marge obtenu) (tableau ou
graphique -> au choix)")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; SQL query &nbsp;", "</b>")),
            verbatimTextOutput("code_3b")
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; SQL response &nbsp;", "</b>")),

            rhandsontable(
              data_question3b,
              readOnly = TRUE,
              stretchH = "all"
            )
          ),

          box(
            width = 12, status = "primary",
            title = HTML(paste0("<b>", "&nbsp; Visualisation &nbsp;", "</b>")),

            column(
              width = 4,

              selectInput("select",
                label = h5("Choisissez un univers"),
                choices = as.character(data_question3a[, "codeunivers"]),
                selected = "U1"
              ),

              p(" ",
                class = "comment1"
              ),

              formattableOutput("plot_3b_table")
            ),

            column(
              width = 8,
              plotlyOutput("plot_3b_hist"),
              p(" ",
                class = "comment1"
              )
            )
          )
        )
      )
    )
  )
)
