context("Federation");

library(hbplregress);

test_that("Federated results are consistent with global results", {

    x1 <- 11:30;
    x2 <- runif(20,5,95);
    x3 <- rbinom(20,1,.5);

    b0 <- 17;
    b1 <- 0.5;
    b2 <- 0.037;
    b3 <- -5.2;
    sigma <- 1.4;

    eps <- rnorm(x1,0,sigma);
    y <- b0 + b1*x1  + b2*x2  + b3*x3 + eps;

    data = data.frame(y,x1,x2,x3);
    ycolumn <- "y";
    Acolumns <- c("x1","x2","x3");

    s <- split(data, rep(1:2,10));

    data1 <- s[[1]]
    data2 <- s[[2]]

    xest1 <- LRegress(data1, ycolumn, Acolumns, c());
    xest2 <- LRegress(data2, ycolumn, Acolumns, c());

    betas <- list(xest1[[1]], xest2[[1]])
    Sigmas <- list(xest1[[2]], xest2[[2]])
    
    xestf <- LRegress_group(betas, Sigmas, 99);
    
    betaf <-  xestf[[1]];
    Sigmaf <- xestf[[2]];

    gestf <- LRegress(data, ycolumn, Acolumns, c());

    gbetaf <- gestf[[1]];
    gSigmaf <- gestf[[2]];

    expect_equivalent(betaf, gbetaf);
    expect_equivalent(Sigmaf, gSigmaf);
});

test_that("Federated results are valid over 4 disjoint datasets", {

  library("MASS");
  
  N=20;
  M=20;
  
  A1<-matrix(rnorm(N*M,mean=10,sd=4), N, M);
  A2<-matrix(rnorm(N*M,mean=12,sd=5), N, M);
  A3<-matrix(rnorm(N*M,mean=12,sd=3), N, M);
  A4<-matrix(rnorm(N*M,mean=12,sd=3.5), N, M);
  
  x<-matrix(rnorm(M,mean=2,sd=1),M,1);
  
  y1<-A1%*%x;
  y2<-A2%*%x;
  y3<-A3%*%x;
  y4<-A4%*%x;
  
  # Running regression at each local node.
  xest1<-LRegress_Node(y1,A1);
  xest2<-LRegress_Node(y2,A2);
  xest3<-LRegress_Node(y3,A3);
  xest4<-LRegress_Node(y4,A4);
  
  beta1<-xest1[[1]];Sigma1<-xest1[[2]];
  beta2<-xest2[[1]];Sigma2<-xest2[[2]];
  beta3<-xest3[[1]];Sigma3<-xest3[[2]];
  beta4<-xest4[[1]];Sigma4<-xest4[[2]];
  
  
  betas<-list(beta1,beta2,beta3,beta4);
  Sigmas<-list(Sigma1,Sigma2,Sigma3,Sigma4);
  # Integrating results of all Local Nodes in the Federation node.
  xestf<-LRegress_Federation(betas,Sigmas);
  betaf<-xestf[[1]];
  Sigmaf<-xestf[[2]];

});
