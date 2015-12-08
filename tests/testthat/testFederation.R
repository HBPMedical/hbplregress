context("Federation");

library(hbplregress);

test_that("Federated results are consistent with global results", {

	N <- 20;
    M <- 20;

    A1 <- matrix(rnorm(N*M,mean=10,sd=4), N, M);
    A2 <- matrix(rnorm(N*M,mean=12,sd=5), N, M);
    x  <- matrix(rnorm(M,mean=2,sd=1), M, 1);
    
    y1 <- A1%*%x;
    y2 <- A2%*%x;
    
    xest1 <- LRegress_Node(y1,A1);
    xest2 <- LRegress_Node(y2,A2);
    beta1 <- xest1[[1]];
    Sigma1<- xest1[[2]];
    beta2 <- xest2[[1]];
    Sigma2<- xest2[[2]];
    
    xestf <- LRegress_Federation(beta1,Sigma1,beta2,Sigma2);
    
    betaf <-  xestf[[1]];
    Sigmaf <- xestf[[2]];

    gestf <- LRegress_Node(rbind(y1,y2), rbind(A1,A2));

    gbetaf <- gestf[[1]];
    gSigmaf <- gestf[[2]];

    expect_equal(betaf, gbetaf);
    expect_equal(Sigmaf, gSigmaf);
});
