#______________________________________________________________________________________
#______________________________________________________________________________________
#                           VIX Replication
#                         author: Garvin Kruthof
#
#                        -   Helper Function -
#______________________________________________________________________________________
#______________________________________________________________________________________


#calculating T
#Takes the current observed time and an indication if standard or weekly SPX
#expirations should be used, as inputs. Returns T


T.func = function (data, weekly=TRUE) {
  #calculating time remaining until midnight of current day
  M_current = 1440 - data$QuoteTime
  #calculating minutes from midnight until 8:30 am (510 m) for standard SPX expirations
  # or minutes from midnight until 3.00 pm (900m) for weekly
  M_settlement = ifelse(weekly==TRUE,900,510)
  #calculating minutes of the days between expiration day and quote day
  M_other = (data$days_to_expire -1 ) * 1440
  #define var T as M_current + M_Settlement + Other, if the days until expiration != 0
  #else, calculate the minutes until settlement
  T <- (M_current + M_settlement + M_other)/525600
  return(T)

}
NT = function (data, weekly=TRUE) {
  #calculating time remaining until midnight of current day
  M_current = 1440 - data$QuoteTime
  #calculating minutes from midnight until 8:30 am (510 m) for standard SPX expirations
  # or minutes from midnight until 3.00 pm (900m) for weekly
  M_settlement = ifelse(weekly==TRUE,900,510)
  #calculating minutes of the days between expiration day and quote day
  M_other = (data$days_to_expire -1 ) * 1440
  #define var T as M_current + M_Settlement + Other, if the days until expiration != 0
  #else, calculate the minutes until settlement
  NT <- (M_current + M_settlement + M_other)
  return(NT)

}

#calculating forward index price
F.func = function(data,T,rf){

  #mean will be used if multiple obs with equal difference
  strike = mean(data$strike)

  F <- (strike + exp( rf *  T[1])* (data$diff[1]))
  return (F)
}


#function that identifies out-of-the-money options,
#default: call option
#input: vector that of observations twith strikes that  are larger(smaler)
#in case of call option (put option)
OOTM = function(data, id, put=FALSE) {
  OOTM = c()
  counter = 0
  for (j in 1:length(id)) {
    #differentiate between put and call
    if(put == TRUE) {
      index = length(id) - j +1
      #index = length(id)-j
      #check if bid equals one and if there has already been
      #ab 0-obs in the previous iteration
      if(data[id[index],"bid_put"] == 0){counter = counter+1
      if (counter >1) {
        #if 2 consecutive 0-bids occur,
        #return all non-0 obs collected so far
        return(OOTM)}
      }
      #if this iteration does not include a 0-bid, set counter
      #back to 0
      else {counter = 0
      OOTM = c(OOTM,id[index])
      }

    }

  else {
    index = j
    if(data[id[index],"bid_call"] == 0){counter = counter+1
    if (counter >1) {
      return(OOTM)}
    }
    else {counter = 0
    OOTM = c(OOTM,id[index])

    }

  }
  }
  return(OOTM)

}



K_delta = function(data,OOTM,put=FALSE){
    K_delta = c()
    #differentiate between put and call
    if ( put == TRUE) {
      #loop through each observation that has been checked via OOTM function previously
    for (i in 1:length(OOTM)){
      index =  i
      #if it is the first observation, delta is the difference between the 1st and the second
      if ((index == 1)) {
        K_delta = c(K_delta ,data[OOTM[index],"strike"]-data[OOTM[index+1],"strike"])
      }
      #if it is the last observation, delta is the difference between the obs before the last and the last
      else if ((index ==length(OOTM)) ) {
        K_delta = c(K_delta ,data[OOTM[index-1],"strike"]-data[OOTM[index],"strike"])
      }
      else {
        #else, kdelta is the average of the distance of the observation before the current and the observation
        #after the current
      K_delta = c(K_delta ,(data[OOTM[index-1],"strike"]-data[OOTM[index+1],"strike"])/2)
      }
    }
    return(K_delta)
    }
    else{
      for (i in 1:length(OOTM)){
        index =  i
        if ((index == 1)) {
          #if it is the first observation, delta is the difference between the second and the first

          K_delta = c(K_delta ,data[OOTM[index+1],"strike"]-data[OOTM[index],"strike"])
        }
        else if ((index ==length(OOTM)) ) {
          #if it is the last observation, delta is the difference between  the last and obs before the last
          K_delta = c(K_delta ,data[OOTM[index],"strike"]-data[OOTM[index-1],"strike"])
        }
        else {
          #else, kdelta is the average of the distance of the observation before the current and the observation
          #after the current
        K_delta = c(K_delta ,(data[OOTM[index+1],"strike"]-data[OOTM[index-1],"strike"])/2)
        }
      }
      return(K_delta)

    }
}

#QK returns midquoted price
QK = function(data,put=FALSE,K0=FALSE) {

  #if current is K0, Midquoted is the average of call (average of bid + ask ) and put (average of bid and ask)
  if(K0 == TRUE){return(((((data$bid_call + data$ask_call )/2 )+(data$bid_put + data$ask_put)/2))/2)}

  #if current is call, midquoted is the  ask and bid of call
  if(put==FALSE) { return ((data$bid_call + data$ask_call) /2)}
  #if current is put, midquoted is the  ask and bid of put
  return ((data$bid_put + data$ask_put)/2)
}

#calculate varinance
#input: vector delta K and K and scalars
sigma2 = function(T,K,K0,rf,F){

  k=0
  for (i in 1:nrow(K)){

    tmp =  K$kdelta[i]/(K$strike[i]^2)* exp(rf*T)

    #if (K$strike[i] != K0) { tmp = tmp*K$MidQuoted[i]}
    tmp = tmp*K$MidQuoted[i]

    k = k + tmp
  }
  return( (2/T) * k - (1/T * (F/K0-1)^2) )

}



#Calculating VIX for time t.
#Takes T1,T2 ,var1 and var2 as inputs, returns VIX
VIX = function(T1,T2,var1,var2,tmp1,tmp2){
  #number of minutes in 30 and 365 days
  N30 = 30*1440
  N365 = 365 * 1440

  #number of minutes for T1 and T2
  Nt1 = NT(tmp1)[1]
  Nt2 = NT(tmp2)[1]
  vix=  100 * sqrt(((T1[1]*var1 * ((Nt2-N30) /(Nt2 - Nt1))) + (T2[1]*var2 *((N30-Nt1) /(Nt2 - Nt1))) ) * (N365/N30))
  #returning vix
  return (vix[1])
}

# convert minutes to hours
m2h <- function(x=670.93) {
  h <- floor(x/60)
  m <- floor(x - h*60)
  s <- (x - h*60 - m) * 60
  sprintf("%0.2d:%0.2d", h, m)
}
