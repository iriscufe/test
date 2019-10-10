# 插入排序
x<-c(2, 5, 6, 7, 1, 4, 3)
y<-x[1]
xlength<-length(x)
for(i in 2:xlength){
  ylength<-length(y)
  yll<-ylength
  waitx<-x[i]
  for(j in 1:ylength){
      if(y[j]>waitx){yll<-j-1;break}
  }
  if(yll==ylength){y1<-y;y2<-NULL}
  if(yll==0){y1<-NULL;y2<-y}
  if(yll<ylength & yll>0){y1<-y[1:yll];y2<-y[(yll+1):ylength]}
  y<-c(y1,waitx,y2)
}
y

#冒泡排序
x<-c(2, 5, 6, 7, 1, 4, 3)
n<-length(x)
for(i in 1:n){
    for(j in 1:(n-1)){
      if(x[j+1]<x[j]){
        x[j:(j+1)]<-c(x[j+1],x[j])
      }
    }
}
x

#快速排序
#博客copy版
quicksort<-function(x){
  if(length(x)<=1){return(x)}
  point<-x[1]
  t<-x[-1]
  sv1<-t[t<point]
  sv2<-t[t>=point]
  sv1<-quicksort(sv1)
  sv2<-quicksort(sv2)
  return(c(sv1,point,sv2))
}

a<-c(2,5,6,7,1,4,3)
quicksort(a)

#二叉树版
x <- c(2, 5, 6, 7, 1, 4, 3); n <- length(x); y <- x
nodes <- 1; curr <- 1
BegEnd <- matrix(c(1, n),1,2)
while(curr <= nodes){
  tb <- BegEnd[curr,1]
  te <- BegEnd[curr,2]
  tn <- te-tb+1
  if(tn>1){
    tl <- 0
    tr <- 0
    ty <- y[tb:te]
    tty <- rep(0,tn)
    ry <- sample(ty,1)
    for(i in 1:tn){
      if (ty[i] <= ry) {
        tl <- tl + 1
        tty[tl] <- ty[i]
      } 
    }
    for(j in tn:1){
      if (ty[j]>ry){
        tr<-tr+1
        tty[tn+1-tr]<-ty[j]
      }
    }
    if(tl>0){
      ttb <- tb
      tte <- tb + tl -1
      BegEnd = rbind(BegEnd, c(ttb,tte))
      nodes <- nodes + 1
    }
    if(tr>0){
      ttb <- tb + tl
      tte <- te
      BegEnd = rbind(BegEnd, c(ttb,tte))
      nodes <- nodes +1
    }
    y[tb:te] <- tty
  }
  curr <- curr + 1
}
y

quicksort <- function(x){
  n <- length(x)
  if (n == 1) {
    return(x)
  } else {
    cx <- sample(x, 1)
    lx <- NULL
    rx <- NULL
    for(i in 1:n){
      if(x[i]<= cx){
        lx <- c(lx, x[i])
      }else{
        rx <- c(rx, x[i])
      }
    }
    #lx <- c(lx, cx)
    if(length(rx)>=1){
      return(c(quicksort(lx),quicksort(rx)))
    }else{
      return(quicksort(lx))
    }
  }
  
}

a<-c(2,5,6,7,1,4,3)
quicksort(a)


