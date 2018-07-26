#' Vix_Calc: Calculating volatility index quotes
#'
#' @param source Dataframe incl. 'QuoteDate' (format:YYYY-MM-DD), 'QuoteTime' (format:hh:mm:ss), 'expiration' (format:YYYY-MM-DD), 'option_type' ('C' for call or 
#' 'P' for put), 'bid', 'ask','active_underlaying_price' (S&P 500 spot prices),'underlying symbol' with '^SPX' representing the S&P500 symbol
#' @param nearT Defining the near term 
#' @param nextT Defining the next term 
#' @param rf_near Defining risk free rate for near term 
#' @param rf_next Defining risk free rate for next term 
#' @export
#' @seealso \code{\link{letVixShiny}},\code{\link{convert_date}}
#' @param minutes How often the VIX shall be calculated within the given day (default = once each minute), min=1
#'
#' @return A vector of VIX quptes
#'
#' @examples vix_calc(source=data, rf_near = 0.002,rf_next = 0.002, dmY=FALSE, nearT=28, nextT=35) 
#' 
vix_calc = function(Data,rf_near = 0.002,rf_next = 0.002, dmY=FALSE, nearT=28, nextT=35) {


  #subsetting data. Only observations of SPX
  tmpC = Data[(Data$option_type=='C') & (Data$underlying_symbol=='^SPX'),]
  tmpP = Data[(Data$option_type=='P')& (Data$underlying_symbol=='^SPX'),]


  #overwriting Data with dataframe consisting of all infos og tmpC + ask and bid price for the put with same
  #quote and expiration time
  Data = data.frame("quote_datetime"=tmpC$quote_datetime,"expiration"=tmpC$expiration,"strike" = tmpC$strike,
                  "bid_call" = tmpC$bid,"ask_call" = tmpC$ask ,"bid_put" = tmpP$bid,"ask_put"=tmpP$ask,
                  "call_minus_put_ask" = tmpC$bid - tmpP$bid, "sandp_bid" = tmpP$active_underlying_price)



  Data$days_to_expire = as.numeric(Data$expiration - Data$QuoteDate)
  #define call and put price as the mean of bid and ask
  Data$call = (Data$bid_call + Data$ask_call)/2
  Data$put = (Data$bid_put + Data$ask_put)/2

  #new variable diff = put - call
  Data$diff = Data$call-Data$put

  #define subset for near (28 days) - and next term (35 days)
  nearTerm = Data[(Data$expiration == (Data$QuoteDate[1]+nearT)),]
  nextTerm = Data[(Data$expiration == (Data$QuoteDate[1]+nextT)),]


  #initialise vix vector
  vix = c()
  # #looping through each minute of the trading day
  for (i in 1:405){

    #set tmp1 (tmp2) as the subset for the repsective minute of the trading day
    tmp1= nearTerm[nearTerm$QuoteTime==(min(nearTerm$QuoteTime)+i-1),]
    tmp2= nextTerm [nextTerm$QuoteTime==(min(nextTerm$QuoteTime)+i-1),]

    #identifiy observations where difference between put and call is smallest
    id_min_near = which(abs(tmp1$diff)==min(abs(tmp1$diff)))
    id_min_next = which(abs(tmp2$diff)==min(abs(tmp2$diff)))

    #calculate forward level F for near and next term
    F_near = F.func(tmp1[id_min_near,],T=T.func(tmp1[id_min_near,]),rf=rf_near)
    F_next = F.func(tmp2[id_min_next,],T=T.func(tmp2[id_min_next,]),rf=rf_next)

    #identify all observation with strike price <F
    K0_tmp_next= which(tmp2[,"strike"]<F_next)
    K0_tmp_near= which(tmp1[,"strike"]<F_near)

    #set K0 being the next strike price below F
    K0_near = tmp1[K0_tmp_near[length(K0_tmp_near)],"strike"]
    K0_next = tmp2[K0_tmp_next[length(K0_tmp_next)],"strike"]

    #identify Strike prices > K0
    id_call_near= which(tmp1$strike>K0_near)
    id_call_next= which(tmp2$strike>K0_next)
    #identify Strike prices < K0
    id_put_near= which(tmp1$strike<K0_near )
    id_put_next= which(tmp2$strike<K0_next)

    #calculating T
    T1=T.func(tmp1[id_min_near,], weekly=TRUE)
    T2=T.func(tmp2[id_min_next,],weekly=TRUE)

    #selecting only observations with non-0 bid prices
    #and stop including further observations if non-0's
    #occur twice in a row
    K_call_near = tmp1[OOTM(tmp1,id_call_near,put=FALSE),]
    K_put_near = tmp1[OOTM(tmp1,id_put_near,put=TRUE),]

    K_call_next = tmp2[OOTM(tmp2,id_call_next,put=FALSE),]
    K_put_next = tmp2[OOTM(tmp2,id_put_next,put=TRUE),]

    #calculate Kdelta
    Kdelta_call_near = K_delta(tmp1, OOTM(tmp1,id_call_near,put=FALSE),put=FALSE )
    Kdelta_put_near = K_delta(tmp1, OOTM(tmp1,id_put_near,put=TRUE),put=TRUE )

    Kdelta_call_next = K_delta(tmp2, OOTM(tmp2,id_call_next,put=FALSE),put=FALSE )
    Kdelta_put_next = K_delta(tmp2, OOTM(tmp2,id_put_next,put=TRUE),put=TRUE )

    #adding Kdelta, Option indicator and MidQuoted price to the dataframe
    K_call_near$kdelta = Kdelta_call_near
    K_call_near$Option = 'C'
    K_call_near$MidQuoted = QK(K_call_near,put=FALSE)

    K_put_near$kdelta = Kdelta_put_near
    K_put_near$Option = 'P'
    K_put_near$MidQuoted = QK(K_put_near,put=TRUE)

    #calculating Kdelta for K0 and create a dataframe for K0
    #to be added to the rest later
    #working with means in case multiple obs have the same min difference
    Kdelta_K0_near = (mean(tmp1[id_min_near+1,"strike"])    -mean(tmp1[id_min_near-1,"strike"] ))/ 2
    K0_row_near = tmp1[K0_tmp_near[length(K0_tmp_near)],]
    K0_row_near$kdelta = Kdelta_K0_near
    K0_row_near$Option = 'Mid'
    K0_row_near$MidQuoted = QK(K0_row_near,K0=TRUE)

    #combine all three dataframes into one
    K_near = rbind(K_put_near,K0_row_near,K_call_near)


    #repeat the above steps for the next term data
    K_call_next$kdelta = Kdelta_call_next
    K_call_next$Option = 'C'
    K_call_next$MidQuoted = QK(K_call_next,put=FALSE)
    K_put_next$kdelta = Kdelta_put_next
    K_put_next$Option = 'P'
    K_put_next$MidQuoted = QK(K_put_next,put=TRUE)

    #same for K0
    Kdelta_K0_next= (mean(tmp1[id_min_next+1,"strike"])    -mean(tmp1[id_min_next-1,"strike"] ))/ 2
    K0_row_next = cbind(tmp2[K0_tmp_next[length(K0_tmp_next)],],kdelta=Kdelta_K0_next)
    K0_row_next$Option = 'Mid'
    K0_row_next$MidQuoted = QK(K0_row_next,K0=TRUE)

    #combine all three dataframes into one
    K_next <- rbind(K_put_next,K0_row_next,K_call_next) #}

    #calculate sigma for next and near term
    sigma_1 = sigma2 (T=T.func(tmp1[id_min_near,]),F=F_near, rf = rf_near,K0=K0_near,K=K_near)
    sigma_2 = sigma2 (T=T.func(tmp2[id_min_next,]),F=F_next, rf = rf_next,K0=K0_next,K=K_next)

    #calculate vix
    #vix= c(vix,VIX(T1,T2,sigma_1,sigma_2,K_near,K_next))
    vix[i]= VIX(T1,T2,sigma_1,sigma_2,K_near,K_next)

  }
  #return dataframe with trading times and vix, of class vixr

  results = data.frame('Time' = m2h(seq(1:405)+570), 'VIX' = vix)
  class(results) = "vixr"
  return(results)

}
