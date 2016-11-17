# Call packages
devtools::install_github("hrbrmstr/omdbapi")
library(dplyr)
library(pbapply)
library(omdbapi)
library(stringr)
library(devtools)

# create function to retrieve tv episodes with given name
getTv <- function(OUR_TITLE,OUR_YEAR=NA){
  
  # initialize vectors for ratings,seasons,episodes
  x <- c()
  x.season <- c()
  x.episode <- c()
  
  # loop over seasons (Assuming maximum of 50 seasons)
  for(this.season in 1:50){
    
    # check if this season exists, otherwise break the for loop
    if(dim(find_by_title(OUR_TITLE,type="episode",
                         season=this.season,
                         episode=1,
                         year_of_release = OUR_YEAR))[1] == 0){
      break
    } 
    
    else {
      
      # now go over the episodes
      # first wait for 2 seconds (this amount probably needs to be higher)
      # (we don't want to get blacklisted from the API)
      print("Waiting for 2 seconds...")
      Sys.sleep(2)
      
      # looping over episodes (maximum is 50)
      for(this.episode in 1:50){
        
        if(dim(find_by_title(OUR_TITLE, 
                             type="episode",
                             season=this.season,
                             episode=this.episode,
                             year_of_release = OUR_YEAR))[1] == 0){
          break
        } 
        
        else {
          
          # wait for two seconds
          if(this.episode %% 9 ==0){
            print("Waiting for 2 seconds...")
            Sys.sleep(2)
          }
          
          # retrieve ratings
          this.rating <- find_by_title(OUR_TITLE, 
                                       type="episode",
                                       season=this.season,
                                       episode=this.episode,
                                       year_of_release = OUR_YEAR)$imdbRating
          x <- c(x,this.rating)
          x.season <- c(x.season,this.season)
          x.episode <- c(x.episode,this.episode) 
          
        }
      }
    }
  }
  
  # return dataframe
  return(data.frame(x=x,season=x.season,episode=x.episode))
  
}