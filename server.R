library(shiny)
library(lsa)
library(dplyr)
library(shinydashboard)

# Get the raw data
review = read.csv("data/data.csv")[,-1]



shinyServer(function(input, output){
  # --------THE INPUTS----------
  
  # Input user_id
  
  # --------MANIPULATE THE DATA------

  ratingUser = reactive({
  
  # isloate data
    isolate({
      userOne = review[,c("user_id","business_id","review_star")]
      userAvg= userOne%>%
       group_by(user_id,business_id)%>%
       summarise(avg_star = mean(review_star))
      
      # create the matrix
      rating_matrix  = with(userAvg, {
        M = matrix(nrow=length(unique(business_id)), 
                   ncol=length(unique(user_id)), 
                   dimnames=list(rest=levels(business_id), 
                                 users=levels(user_id)))
        M[cbind(business_id,user_id)] = avg_star
        M
        })

      rating_matrix[is.na(rating_matrix)] =0
    })

  
  # Create the rating prediction for OneUser
  
    pickUserRating =rating_matrix[,input$pick_user]
    user_sim1=cosine(pickUserRating,rating_matrix)

    user_sim1 =matrix(user_sim1,
                      dimnames=list(totalUser=names(user_sim1),
                                    user= input$pick_user))
    # drop itself
    user_sim1 =user_sim1[!(rownames(user_sim1) %in% c(input$pick_user)),]
    user_sim1 =matrix(user_sim1,
                      dimnames=list(totalUser=names(user_sim1),
                                    user= input$pick_user))
    rating_matrix = rating_matrix[,!(colnames(rating_matrix)%in% c(input$pick_user))]
    
    # compute predicting rating
    sim_score =  rating_matrix%*%user_sim1
    for (r in 1:nrow(sim_score)){
      sim_sum=0
      if(sim_score[r,1]!=0){
        for(u in 1:ncol(rating_matrix)){
          if (rating_matrix[r,u]!=0){
            sim_sum=sim_sum+user_sim1[u,1]}}
        sim_score[r,1]=sim_score[r,1]/sim_sum
      }}
    
    # dump those have been
    have_been = review[review$user_id==input$pick_user,]$business_id %>%
      droplevels()
    pre_score = sim_score[!rownames(sim_score)%in% have_been,]
    pre_score = matrix(pre_score,
                       dimnames=list(rest=names(pre_score),
                                     user= input$pick_user))
    # get the business_id of 6 recommendation
    recom = names(head(pre_score[order(-pre_score[,input$pick_user]),],6))
    Predict_rating = head(pre_score[order(-pre_score[,input$pick_user]),],6)
    recomInfo = review[review$business_id%in% recom ,][,c(8:15)]
    recomInfo= recomInfo[!duplicated(recomInfo),]
    recomInfo = cbind(Predict_rating,recomInfo)
    recomInfo
  })
  Usertable= reactive({
    UserReview = review[review$user_id==input$pick_user,][,c(1:6)]
    UserReview=UserReview[!duplicated(UserReview),]

    UserReview
  })
  # ---------TAB TO SHOW USER INFO------
  output$userinfo <- renderUI({
    user = Usertable()
    doc = tags$html(
      tags$body(
        div(span("User ID:",strong(user[1,1])),
        div(span("Name:", strong(user[1,2]))),
        div(span("Yelp User since: ",strong(user[1,3]))),
        div(span("Review Number in Las Vegas:  ",strong(user[1,6]))),
        div(span("Average Star:  ",strong(user[1,4]))) ,
        div(span("Elite User:  ",strong(user[1,5])))
    )))
  })
  # ---------TAB TO SHOW REST INFO------
  output$restinfo = renderUI({
      user = Usertable()
      Rest = ratingUser()
      doc = tags$html(
        tags$head(
          includeCSS(file.path('www', 'style2.css'))
        ),
        tags$body(
          # address [,8]; name [,3]; rating [1,4] ; 
          div("Hi,  ",strong(user[1,2], ".")),
          div(paste("We'd like to recommend you: ")),
          br(),
          div(id="container", class="left",
              h4(Rest[1,3]),
              h1("1"),
              hr(),
              h6(strong("Star:"),Rest[1,4]),
              h6(strong("Address:")),
              h6(Rest[1,8]),
              h6(strong("Cuisine:")),
              h6(Rest[1,9])),
          div(id="container", class="left",
              h4(Rest[2,3]),
              h1("2"),
              hr(),
              h6(strong("Star:"),Rest[2,4]),
              h6(strong("Address:")),
              h6(Rest[2,8]),
              h6(strong("Cuisine:")),
              h6(Rest[2,9])),
          div(id="container", class="left",
              h4(Rest[3,3]),
              h1("3"),
              hr(),
              h6(strong("Star:"),Rest[3,4]),
              h6(strong("Address:")),
              h6(Rest[3,8]),
              h6(strong("Cuisine:")),
              h6(Rest[3,9])),
          div(id="container", class="left",
             h4(Rest[4,3]),
             h1("4"),
             hr(),
             h6(strong("Star:"),Rest[4,4]),
             h6(strong("Address:")),
             h6(Rest[4,8]),
             h6(strong("Cuisine:")),
             h6(Rest[4,9])),
          div(id="container", class="left",
              h4(Rest[5,3]),
              h1("5"),
              hr(),
              h6(strong("Star:"),Rest[5,4]),
              h6(strong("Address:")),
              h6(Rest[5,8]),
              h6(strong("Cuisine:")),
              h6(Rest[5,9])),
          div(id="container", class="left",
              h4(Rest[6,3]),
              h1("6"),
              hr(),
              h6(strong("Star:"),Rest[6,4]),
              h6(strong("Address:")),
              h6(Rest[6,8]),
              h6(strong("Cuisine:")),
              h6(Rest[6,9]))
          )
      )
      
    })
})
