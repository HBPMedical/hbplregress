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

    xest1 <- LRegress_Node(data1, ycolumn, Acolumns);
    xest2 <- LRegress_Node(data2, ycolumn, Acolumns);

    betas <- list(xest1[[1]], xest2[[1]])
    Sigmas <- list(xest1[[2]], xest2[[2]])
    
    xestf <- LRegress_Federation(betas, Sigmas, 99);
    
    betaf <-  xestf[[1]];
    Sigmaf <- xestf[[2]];

    gestf <- LRegress_Node(data, ycolumn, Acolumns);

    gbetaf <- gestf[[1]];
    gSigmaf <- gestf[[2]];

    expect_equivalent(betaf, gbetaf);
    expect_equivalent(Sigmaf, gSigmaf);
});
