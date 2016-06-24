###This file contains functions for quanitfing the topology
###of binary trees. each function takes as input a tree object which is
###a simple list of node objects defined below. The tree object can be 
###obtained by using the encode.tree() fuction of the SpList

#code a binary tree
node<-setClass("node",slots=c(left="character",right="character",parent="character",bnch.len="numeric"))

encode.tree<-function(SpList){
  tree=list()
  spl<-SpList
spl[,1:2]<-sapply(spl[,1:2],function(x)as.character(x))
tree[["S1"]]<-node(left="S2",right="S3",parent=character(0),bnch.len=0)
for(i in 1:nrow(spl)){
  anc<-which(spl[,1]==spl[i,2])
  if(length(anc)!=0)tree[[spl[i,2]]]<-node(left=spl[anc[1],2],right=spl[anc[2],2],parent=spl[i,1],bnch.len=spl[i,3]-tree[[spl[i,1]]]@bnch.len) else
    tree[[spl[i,2]]]<-node(left=character(0),right=character(0),parent=spl[i,1],bnch.len=spl[i,3]-tree[[spl[i,1]]]@bnch.len)
}
return(tree)}

##my.tree<-encode.tree(SpList)

node.depth<-function(tree,r,u){
    d = 0
    while (u!=r) {
      u = tree[[u]]@parent
      d=d+1
    }
    return(d);
  }

tree.size<-function(tree,u) {
  if (length(tree[[u]]@left) == 0) return(1) else
    return(1 + tree.size(tree,tree[[u]]@left) + tree.size(tree,tree[[u]]@right))
  }

node.height<-function(tree,u) {
  if (length(tree[[u]]@right) == 0) return(0)
  return(1 + max(node.height(tree,tree[[u]]@left),node.height(tree,tree[[u]]@right)))
}

#node.height(my.tree,"S3")

node.height.balance<-function(tree,u){
  rh<-node.height(tree,tree[[u]]@right)
  lh<-node.height(tree,tree[[u]]@left)
  return(abs(lh-rh))
}

#node.height.balance(my.tree,"S1")

node.diameter<-function(tree,u) {
  if (length(tree[[u]]@right)==0) return(0) 
    lheight<-node.height(tree,tree[[u]]@left)
    rheight<-node.height(tree,tree[[u]]@right)
    ldiam<-node.diameter(tree,tree[[u]]@left)
    rdiam<-node.diameter(tree,tree[[u]]@right)
    return(max(lheight+rheight+2,max(ldiam,rdiam)))
}

#node.diameter(my.tree,"S1")

node.dia.balance<-function(tree,u){
  rb<-node.diameter(tree,tree[[u]]@right)
  lb<-node.diameter(tree,tree[[u]]@left)
  return(abs(lb-rb))
}

#node.dia.balance(my.tree,"S1")


#this one need work
node.age<-function(tree,r,u){
    d = tree[[u]]@bnch.len
    while (u!=r) {
      u = tree[[u]]@parent
      d=d+tree[[u]]@bnch.len
    }
    return(d);
  } 
#int.age(my.tree,"S1","S75")

is.ancestor<-function(tree,sp1,sp2){
  u=sp1
  d = NULL
  while (u!="S1") {
    d=c(d,tree[[u]]@parent)
    u = tree[[u]]@parent
  }
  return(sp2%in%d);
}

my.ancestors<-function(tree,sp1){
  u=sp1
  d = NULL
  while (u!="S1") {
    d=c(d,tree[[u]]@parent)
    u = tree[[u]]@parent
  }
  return(d);
}

rnd.tree<-function(n){
  SpList<-data.frame(ancestor=character(0),descendent=character(0),branch.len=numeric(0))
  tree<-list()
  next.sp<-2
  tree[["S1"]]<-node(left=character(0),right=character(0),parent=character(0),bnch.len=0)
  for(i in 1:n-1){
    nxtSp<-paste("S",next.sp,sep='')
    next.sp<-next.sp+1
    spec<-"S1"
    while(length(tree[[spec]]@left!=0))spec<-sample(names(tree))[1]
    tree[[nxtSp]]<-node(left=character(0),right=character(0),parent=spec,bnch.len=i-tree[[spec]]@bnch.len)
    tree[[spec]]@left<-nxtSp
    SpList<-rbind(SpList,data.frame(spec,nxtSp,i+1))
    nxtSp<-paste("S",next.sp,sep='')
    next.sp<-next.sp+1
    tree[[nxtSp]]<-node(left=character(0),right=character(0),parent=spec,bnch.len=i-tree[[spec]]@bnch.len)
    tree[[spec]]@right<-nxtSp
    SpList<-rbind(SpList,data.frame(spec,nxtSp,i+1))
  }
  return(list(tree=tree,SpList=SpList))
}

#return a vector of names of terminal nodes
get.leaves<-function(SpList)SpList[,2][!SpList[,2]%in%SpList[,1]]


#z$SpList
#z<-rnd.tree(99)
#tree.size(z$tree,"S1")
#X11()
#plot(read.tree(text=plots.tree(z$SpList)),show.node.label=T,cex=.75)
#plot(read.tree(text=plots.tree(SpList)),show.node.label=T,cex=.75)
#((tree.size(z$tree,"S1")+1)/2)
#node.diameter(z$tree,"S1")
#node.height(z$tree,"S1")
#node.height.balance(z$tree,"S1")
#node.dia.balance(z$tree,u="S1")
#node.dia.balance(my.tree,u="S1")
