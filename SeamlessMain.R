
#define the Class called patch that will contain information on where its boundaries are and a list of species.
patch<-setClass("patch",slots=c(bnd="numeric",spp="character"))
#initalize landscape
initialize<-function(){
lndsp<<-list()
next.patch<<-0
next.spp<<-1
SpList<<-data.frame(ancestor=character(0),descendent=character(0),branch.len=numeric(0))
PtList<<-data.frame(patch=character(0),size=numeric(0))
pltName<<-paste('p',next.patch,sep='')
lndsp[[pltName]]<<-patch(bnd=c(0,1),spp=c("S1"))
stp<<-1
}

#####landscape functions####
#add patch takes in a plot name and creates a new plot in and generates descendent species
add.patch<-function(pltNm=NULL){
 
  if(is.null(pltNm))brk<-runif(1,min=0.000001,max=0.999999) else brk<-round(runif(1,lndsp[[pltNm]]@bnd[1]+.000001,lndsp[[pltNm]]@bnd[2]-.000001),6)
  who<<-which(sapply(lndsp,function(x)brk>x@bnd[1]&brk<x@bnd[2])==T)
  if(length(lndsp[[who]]@spp)!=0){
    next.patch<<-next.patch+1
    #next.spp<<-next.spp+1
    pltName<<-paste('p',next.patch,sep='')
    lndsp[[pltName]]<<-patch(bnd=c(lndsp[[who]]@bnd[1],brk),spp=paste("S",next.spp+seq(1,length(lndsp[[who]]@spp)),sep=''))
    SpList<<-rbind(SpList,data.frame(ancestor=lndsp[[who]]@spp,descendent=lndsp[[pltName]]@spp,branch.len=stp))
    next.patch<<-next.patch+1
    next.spp<<-next.spp+length(lndsp[[who]]@spp)
    pltName<<-paste('p',next.patch,sep='')
    lndsp[[pltName]]<<-patch(bnd=c(brk,lndsp[[who]]@bnd[2]),spp=paste("S",next.spp+seq(1,length(lndsp[[who]]@spp)),sep=''))
    SpList<<-rbind(SpList,data.frame(ancestor=lndsp[[who]]@spp,descendent=lndsp[[pltName]]@spp,branch.len=stp))
    next.spp<<-next.spp+length(lndsp[[who]]@spp)
    lndsp[[who]]<<-NULL} else {
    next.patch<<-next.patch+1
    #next.spp<<-next.spp+1
    pltName<<-paste('p',next.patch,sep='')
    lndsp[[pltName]]<<-patch(bnd=c(lndsp[[who]]@bnd[1],brk),spp=character(0))
    next.patch<<-next.patch+1
    pltName<<-paste('p',next.patch,sep='')
    lndsp[[pltName]]<<-patch(bnd=c(brk,lndsp[[who]]@bnd[2]),spp=character(0))
    lndsp[[who]]<<-NULL
   }
  stp<<-stp+1
  return(brk) 
 }
#####test add.patch()####
#add.patch()
#SpList
#lndsp
#add.patch('p2')
##########################
remove.patch<-function(){
  bnds<-sapply(lndsp,function(x)x@bnd)
  to.remove<-0
  while(to.remove==0|to.remove==1)to.remove<-sample(unique(as.vector(bnds)))[[1]]
  who1<-which(sapply(lndsp,function(x)x@bnd[1]==to.remove))
  who2<-which(sapply(lndsp,function(x)x@bnd[2]==to.remove))
  next.patch<<-next.patch+1
  pltName<<-paste('p',next.patch,sep='')
  PtList<<-rbind(PtList,data.frame(pltName,lndsp[[who1]]@bnd[2]-lndsp[[who2]]@bnd[1]))
  lndsp[[pltName]]<<-patch(bnd=c(lndsp[[who2]]@bnd[1],lndsp[[who1]]@bnd[2]),spp=c(lndsp[[who2]]@spp,lndsp[[who1]]@spp))
  
  lndsp[[names(who1)]]<<-NULL
  lndsp[[names(who2)]]<<-NULL
  
  add.patch(pltName)
}


setGeneric("rmSpp",function(object)standardGeneric("rmSpp"))
setMethod(f="rmSpp",signature(object="patch"),function(object){
  object@spp<-character(0)
  return(object)})

toosmall.patch<-function(thld){
b<-sapply(lndsp,function(x)x@bnd)
sz<-b[2,]-b[1,]
lndsp[sz<thld]<<-lapply(lndsp[sz<thld],rmSpp)
}

######plots.tree function####
plots.tree<-function(SpList){
  spl<-SpList
  spl[,1:2]<-sapply(spl[,1:2],function(x)as.character(x))
  newick<-paste("S1",";",sep="")
  anc<-which(spl[,1]=="S1")
  foo<-unlist(strsplit(newick,"S1"))
  newick<-paste(foo[1],"(",paste(spl[anc,2],":",spl[anc,3],sep="",collapse=","),")","S1",foo[2],sep="")
  #loop through species list relpace the ancestor with the names of descendents in newick list, while
  #keeping internal node names and branch lengths
  for(i in 1:nrow(spl)){
    anc<-which(spl[,1]==spl[i,2])
    #check to see if it has descendents
    if(length(anc)!=0){
      splchar<-paste(spl[i,2],":",sep="")
      #split the newick string at the ancestor
      foo<-unlist(strsplit(newick,splchar))
      #modify the string depending on if it is a tip or not
      newick<-paste(foo[1],"(",paste(spl[anc,2],":",spl[anc,3]-spl[i,3],sep="",collapse=","),")",splchar,foo[2],sep="")
    }
  }
  return(newick)}
#######################
#############################
####visualize landscape
plot.lndsp<-function(){
  bnds<-sapply(lndsp,function(x)x@bnd)
  len<-ncol(bnds)
  plot(0:1,0:1,axes = F,xlab='',ylab='',type = 'n')
  axis(side=1,at=unique(c(bnds[1,],bnds[2,])),labels = round(unique(c(bnds[1,],bnds[2,])),2))
  text(apply(bnds,2,mean),rep(.1,len-1),colnames(bnds))
}
####
###get list of species in each patch
get.spp<-function()sapply(lndsp,function(x)x@spp)
###


#######tree builder functions####
sim.tree<-function(npatch,ntot){
  for(i in 1:(npatch-1))add.patch()
  for(i in 1:ntot){
    remove.patch()
  }
}
###with ancestor reporter
sim.tree.r<-function(npatch,ntot){
  for(i in 1:(npatch-1))add.patch()
  init.size<<-sapply(lndsp,function(x)x@bnd[2]-x@bnd[1])
  ances<<-sapply(lndsp,function(x)x@spp)
  for(i in 1:ntot){
    remove.patch()
  }
}
##########tree builder with extinction ######
sim.tree2<-function(npatch,ntot,too.small){
  for(i in 1:(npatch-1))add.patch()
  for(i in 1:ntot){
    toosmall.patch(too.small)
    remove.patch()
  }
}
#########tree builder with extinction and size report report######
sim.tree3<-function(npatch,ntot,too.small){
  for(i in 1:(npatch-1))add.patch()
  init.size<<-sapply(lndsp,function(x)x@bnd[2]-x@bnd[1])
  ances<<-sapply(lndsp,function(x)x@spp)
  for(i in 1:ntot){
    toosmall.patch(too.small)
    remove.patch()
  }
}

