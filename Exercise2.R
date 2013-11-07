is.leap<- function(year){
  #check to see only numerical data is entered
  if(is.numeric(year)==F){return("error: argument of class numeric expected")}
  #check to see if no dates before the use of the gregorian calendar are used as input
  else{if(year<1582){return(sprintf("%s is out of the valid range",year))}
  #return whether the given year is a leap year or not
  else{return(year%%400==0|(year%%4==0&year%%100!=0))}}
}
#possible inputs
is.leap("john")
is.leap(1572)
is.leap(2002)
is.leap(2000)
is.leap(2004)
is.leap(1700)