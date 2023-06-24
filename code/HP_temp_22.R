setwd("C:/Users/s_sjw0513/Desktop/데이터마이닝/project")

library(ggplot2)

#### RF ####
#### ntree ####
rm(list=ls())

data_list = c('data_3508', 'data_3523', 'data_3569', 'data_3571')

for (data in data_list){
  rf_train_result = read.csv(paste0('./result/tuning/rf_train_result_', data, '.csv'))
  rf_val_result = read.csv(paste0('./result/tuning/rf_val_result_', data, '.csv'))
  
  #ntree param_idx dataframe 구축
  ntree.col = unique(rf_train_result$ntree)
  ntree.param = list()
  mtry.param <- list()
  ab.lst = list()
  idx = 0
  for (ntr in ntree.col){
    ndf = subset(rf_train_result, rf_train_result$ntree==ntr)
    for (mtr in unique(ndf$mtry)){
      ntree.param$ntree = c(ntree.param$ntree, ntr)
      mtry.param$mtry = c(mtry.param$mtry, mtr)
      idx = idx+1
    }
    ab.lst$ab = c(ab.lst$ab, idx+0.5)
  }
  ntree.df = data.frame(c(ntree.param, mtry.param))
  ntree.df = ntree.df[order(ntree.df$ntree), ]
  rownames(ntree.df) <- NULL
  ntree.df$param_idx = seq(1, length(ntree.df$ntree),1)
  
  #ntree graph
  leng = length(ntree.df$ntree)
  ntree_train_mse = rep(0, leng)
  ntree_val_mse = rep(0, leng)
  xlab = rep('a', leng)
  xrange = rep(0, leng)
  
  for (i in 1:length(ntree.df$ntree)){
    nt = ntree.df$ntree[i]; mt = ntree.df$mtry[i]
    ntree_train_mse[i] = mean(rf_train_result[(rf_train_result$ntree==nt)&(rf_train_result$mtry==mt),'train_mse'])
    ntree_val_mse[i] = mean(rf_val_result[(rf_val_result$ntree==nt)&(rf_val_result$mtry==mt),'val_mse'])
    xlab[i] = paste('ntree:',nt,' mtry:', mt,sep='')
    xrange[i] = i
  }
  
  gg<-ggplot(mapping=aes(xrange,ntree_val_mse))
  gg<-gg+geom_point(aes(xrange,ntree_val_mse),color='blue')
  gg<-gg+geom_point(aes(xrange,ntree_train_mse),color='red')
  gg<-gg+scale_x_continuous(breaks=xrange,labels=xlab)
  gg<-gg+theme(axis.text.x=element_text(angle=90, hjust=1))+theme(axis.title.x=element_blank()) 
  gg<-gg+ggtitle(paste0(data, ', ntree'))+theme(plot.title = element_text(hjust = 0.5))
  gg<-gg+geom_vline(xintercept=ab.lst$ab,linetype='dashed',size=0.2)
  
  # fn = paste('rf_ntree_',strsplit(file, split= ".csv"),'.png',sep='')
  ggsave(paste0('./plots/rf_ntree_', data, '.jpg'), plot = gg)
  
  
  
  #### mtry ####
  
  #ntree param_idx dataframe 구축
  mtry.col = unique(rf_train_result$mtry)
  ntree.param = list()
  mtry.param <- list()
  ab.lst = list()
  idx = 0
  for (mtr in mtry.col){
    ndf = subset(rf_train_result,rf_train_result$mtry == mtr)
    for (ntr in unique(ndf$ntree)){
      ntree.param$ntree = c(ntree.param$ntree, ntr)
      mtry.param$mtry = c(mtry.param$mtry, mtr)
      idx = idx+1
    }
    ab.lst$ab = c(ab.lst$ab,idx+0.5)
  }
  mtry.df = data.frame(c(ntree.param,mtry.param))
  # mtry.df = mtry.df[order(mtry.df$ntree), ]
  mtry.df = mtry.df[order(mtry.df$mtry), ]
  rownames(mtry.df) <- NULL
  mtry.df$param_idx = seq(1,length(mtry.df$ntree),1)
  
  # mtry graph
  leng = length(mtry.df$ntree)
  mtry_train_mse = rep(0,leng)
  mtry_val_mse = rep(0,leng)
  xlab = rep('a',leng)
  xrange = rep(0,leng)
  
  for (i in 1:length(mtry.df$ntree)){
    nt = mtry.df$ntree[i]; mt = mtry.df$mtry[i]
    mtry_train_mse[i] = mean(rf_train_result[(rf_train_result$ntree==nt)&(rf_train_result$mtry==mt),'train_mse'])
    mtry_val_mse[i] = mean(rf_val_result[(rf_train_result$ntree==nt)&(rf_train_result$mtry==mt),'val_mse'])
    xlab[i] = paste(' mtry:', mt,'ntree:',nt,sep='')
    xrange[i] = i
  }
  
  gg<-ggplot(mapping=aes(xrange,mtry_val_mse))
  gg<-gg+geom_point(aes(xrange,mtry_val_mse),color='blue')
  gg<-gg+geom_point(aes(xrange,mtry_train_mse),color='red')
  gg<-gg+scale_x_continuous(breaks=xrange,labels=xlab)
  gg<-gg+theme(axis.text.x=element_text(angle=90, hjust=1))+theme(axis.title.x=element_blank()) 
  gg<-gg+ggtitle(paste0(data, ', mtry'))+theme(plot.title = element_text(hjust = 0.5))
  gg<-gg+geom_vline(xintercept=ab.lst$ab,linetype='dashed',size=0.2)
  
  #fn = paste('rf_ntree_',strsplit(file, split= ".csv"),'.png',sep='')
  ggsave(paste0('./plots/rf_mtry_', data, '.jpg'), plot = gg)
  
  #### XG ####
  #### nrounds ####
  xg_train_result = read.csv(paste0('./result/tuning/xg_train_result_', data, '.csv'))
  xg_val_result = read.csv(paste0('./result/tuning/xg_val_result_', data, '.csv'))
  
  nrounds.col = unique(xg_train_result$nrounds)
  nrounds.param = list()
  max_depth.param <- list()
  eta.param = list()
  ab.lst = list()
  idx = 0
  for (nr in nrounds.col){
    ndf = subset(xg_train_result,xg_train_result$nrounds == nr)
    for (md in unique(ndf$max_depth)){
      temp = subset(ndf,ndf$max_depth == md)
      for (e in unique(temp$eta)){
        nrounds.param$nrounds = c(nrounds.param$nrounds, nr)
        max_depth.param$max_depth = c(max_depth.param$max_depth, md)
        eta.param$eta = c(eta.param$eta, e)
        idx = idx+1
      }
    }
    ab.lst$ab = c(ab.lst$ab,idx+0.5)
  }
  nrounds.df = data.frame(c(nrounds.param,max_depth.param,eta.param))
  nrounds.df = nrounds.df[order(nrounds.df$nrounds), ]
  rownames(nrounds.df) <- NULL
  nrounds.df$param_idx = seq(1,length(nrounds.df$nrounds),1)
  
  leng = length(nrounds.df$rnounds)
  nrounds_train_mse = rep(0,leng)
  nrounds_val_mse = rep(0,leng)
  xlab = rep('a',leng)
  xrange = rep(0,leng)
  # lne = seq(from=length(unique(ntree.df$mtry))+0.5,to=leng,by=length(unique(ntree.df$mtry)))
  
  for (i in 1:length(nrounds.df$nrounds)){
    nr = nrounds.df$nrounds[i]; md = nrounds.df$max_depth[i]
    e = nrounds.df$eta[i]
    nrounds_train_mse[i] = mean(xg_train_result[(xg_train_result$nrounds==nr)&(xg_train_result$max_depth==md)&(xg_train_result$eta==e),'train_mse'])
    nrounds_val_mse[i] = mean(xg_val_result[(xg_val_result$nrounds==nr)&(xg_val_result$max_depth==md)&(xg_val_result$eta==e),'val_mse'])
    
    xlab[i] = paste('nr:', nr,' md:',md,
                    ' eta:',e,sep='')
    xrange[i] = i
  }
  
  gg<-ggplot(mapping=aes(xrange,nrounds_val_mse))
  gg<-gg+geom_point(aes(xrange,nrounds_val_mse),color='blue')
  gg<-gg+geom_point(aes(xrange,nrounds_train_mse),color='red')
  gg<-gg+scale_x_continuous(breaks=xrange,labels=xlab)
  gg<-gg+theme(axis.text.x=element_text(angle=90, hjust=1))+theme(axis.title.x=element_blank()) 
  gg<-gg+ggtitle(paste0(data, ', nrounds'))+theme(plot.title = element_text(hjust = 0.5))
  gg<-gg+geom_vline(xintercept=ab.lst$ab,linetype='dashed',size=0.2)
  
  ggsave(paste0('./plots/xg_nrounds_', data, '.jpg'), plot = gg)
  
  
  #### max_depth ####
  md.col = unique(xg_train_result$max_depth)
  nrounds.param = list()
  max_depth.param <- list()
  eta.param = list()
  ab.lst = list()
  idx = 0
  for (md in md.col){
    ndf = subset(xg_train_result,xg_train_result$max_depth == md)
    for (nr in unique(ndf$nrounds)){
      temp = subset(ndf,ndf$nrounds == nr)
      for (e in unique(temp$eta)){
        nrounds.param$nrounds = c(nrounds.param$nrounds, nr)
        max_depth.param$max_depth = c(max_depth.param$max_depth, md)
        eta.param$eta = c(eta.param$eta, e)
        idx = idx+1
      }
    }
    ab.lst$ab = c(ab.lst$ab,idx+0.5)
  }
  md.df = data.frame(c(nrounds.param,max_depth.param,eta.param))
  md.df = md.df[order(md.df$max_depth), ]
  rownames(md.df) <- NULL
  md.df$param_idx = seq(1,length(md.df$max_depth),1)
  
  leng = length(md.df$max_depth)
  md_train_mse = rep(0,leng)
  md_val_mse = rep(0,leng)
  xlab = rep('a',leng)
  xrange = rep(0,leng)
  # lne = seq(from=length(unique(ntree.df$mtry))+0.5,to=leng,by=length(unique(ntree.df$mtry)))
  
  for (i in 1:length(md.df$max_depth)){
    nr = md.df$nrounds[i]; md = md.df$max_depth[i]; e = md.df$eta[i]
    md_train_mse[i] = mean(xg_train_result[(xg_train_result$nrounds==nr)&(xg_train_result$max_depth==md)&(xg_train_result$eta==e),'train_mse'])
    md_val_mse[i] = mean(xg_val_result[(xg_val_result$nrounds==nr)&(xg_val_result$max_depth==md)&(xg_val_result$eta==e),'val_mse']
    )  
    xlab[i] = paste('md:', md,' nrounds:',nr,' eta:',e,sep='')
    xrange[i] = i
  }
  
  gg<-ggplot(mapping=aes(xrange,md_val_mse))
  gg<-gg+geom_point(aes(xrange,md_val_mse),color='blue')
  gg<-gg+geom_point(aes(xrange,md_train_mse),color='red')
  gg<-gg+scale_x_continuous(breaks=xrange,labels=xlab)
  gg<-gg+theme(axis.text.x=element_text(angle=90, hjust=1))+theme(axis.title.x=element_blank()) 
  gg<-gg+ggtitle(paste0(data, ', max_depth'))+theme(plot.title = element_text(hjust = 0.5))
  gg<-gg+geom_vline(xintercept=ab.lst$ab,linetype='dashed',size=0.2)
  
  # fn = paste('rf_ntree_3579',strsplit(file, split= ".csv"),'.png',sep='')
  ggsave(paste0('./plots/xg_max_depth_', data, '.jpg'), plot = gg)
  
  
  #### eta ####
  eta.col = unique(xg_train_result$eta)
  nrounds.param = list()
  max_depth.param <- list()
  eta.param = list()
  ab.lst = list()
  idx = 0
  for (e in eta.col){
    ndf = subset(xg_train_result,xg_train_result$eta == e)
    for (nr in unique(ndf$nrounds)){
      temp = subset(ndf,ndf$nrounds == nr)
      for (md in unique(temp$max_depth)){
        nrounds.param$nrounds = c(nrounds.param$nrounds, nr)
        max_depth.param$max_depth = c(max_depth.param$max_depth, md)
        eta.param$eta = c(eta.param$eta, e)
        idx = idx+1
      }
    }
    ab.lst$ab = c(ab.lst$ab,idx+0.5)
  }
  eta.df = data.frame(c(nrounds.param,max_depth.param,eta.param))
  eta.df = md.df[order(md.df$eta), ]
  rownames(eta.df) <- NULL
  eta.df$param_idx = seq(1,length(eta.df$eta),1)
  
  leng = length(eta.df$eta)
  eta_train_mse = rep(0,leng)
  eta_val_mse = rep(0,leng)
  xlab = rep('a',leng)
  xrange = rep(0,leng)
  # lne = seq(from=length(unique(ntree.df$mtry))+0.5,to=leng,by=length(unique(ntree.df$mtry)))
  
  for (i in 1:length(eta.df$eta)){
    nr = eta.df$nrounds[i]; md = eta.df$max_depth[i]; e = eta.df$eta[i]
    eta_train_mse[i] = mean(xg_train_result[(xg_train_result$nrounds==nr)&(xg_train_result$max_depth==md)&(xg_train_result$eta==e),'train_mse'])
    eta_val_mse[i] = mean(xg_val_result[(xg_val_result$nrounds==nr)&(xg_val_result$max_depth==md)&(xg_val_result$eta==e),'val_mse']
    )  
    xlab[i] = paste('eta:', e,' nrounds:',nr,' md:',md,sep='')
    xrange[i] = i
  }
  
  gg<-ggplot(mapping=aes(xrange,eta_val_mse))
  gg<-gg+geom_point(aes(xrange,eta_val_mse),color='blue')
  gg<-gg+geom_point(aes(xrange,eta_train_mse),color='red')
  gg<-gg+scale_x_continuous(breaks=xrange,labels=xlab)
  gg<-gg+theme(axis.text.x=element_text(angle=90, hjust=1))+theme(axis.title.x=element_blank()) 
  gg<-gg+ggtitle(paste0(data, ', eta'))+theme(plot.title = element_text(hjust = 0.5))
  gg<-gg+geom_vline(xintercept=ab.lst$ab,linetype='dashed',size=0.2)
  
  ggsave(paste0('./plots/xg_eta_', data, '.jpg'), plot = gg)
}

