### CUSTOM URL FOR MENUITEM IN SIDEBARMENU
### NOT WORK IN GITHUB PAGES!

# observeEvent(getQueryString(session)$tab, {
#   currentQueryString <- getQueryString(session)$tab # alternative: parseQueryString(session$clientData$url_search)$tab
#   if(is.null(input$sidebarID) || !is.null(currentQueryString) && currentQueryString != input$sidebarID){
#     freezeReactiveValue(input, "sidebarID")
#     updateTabItems(session, "sidebarID", selected = currentQueryString)
#   }
# }, priority = 1)
# 
# observeEvent(input$sidebarID, {
#   currentQueryString <- getQueryString(session)$tab # alternative: parseQueryString(session$clientData$url_search)$tab
#   pushQueryString <- paste0("?tab=", input$sidebarID)
#   if(is.null(currentQueryString) || currentQueryString != input$sidebarID){
#     freezeReactiveValue(input, "sidebarID")
#     updateQueryString(pushQueryString, mode = "push", session)
#   }
# }, priority = 0)

### END CUSTOM URL FOR MENUITEM IN SIDEBARMENU