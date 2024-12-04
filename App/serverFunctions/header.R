output$user <- shinydashboardPlus::renderUser({
  shinydashboardPlus::dashboardUser(
    name = "Daniel Franzani",
    image = "https://raw.githubusercontent.com/Dfranzani/CV/refs/heads/main/Profile.jpeg",
    title = "Ciencia de Datos",
    subtitle = "",
    fluidRow(
      shinydashboardPlus::dashboardUserItem(width = 6, shinydashboardPlus::socialButton(href = "https://github.com/Dfranzani", icon = icon("github-square"))),
      shinydashboardPlus::dashboardUserItem(width = 6, shinydashboardPlus::socialButton(href = "https://dfranzani.github.io/website/principal/home.html", icon = icon("blog")))
    )
  )
})