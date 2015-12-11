context("Node");

library(hbplregress);

test_that("Linear regression is correct at the node level", {

    set.seed(100);

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

    xest <- LRegress_Node(data, ycolumn, Acolumns);

    beta <- xest[[1]];
    Sigma <- xest[[2]];

    expect_equivalent(beta, c(16.18750837,0.55604708,0.02711566,-4.80906215));
    SigmaRef <- matrix(c( 1.026124730, -0.0330411429, -0.0036317683, -0.193004340,
                         -0.033041143,  0.0017638153, -0.0001235071,  0.004068457,
                         -0.003631768, -0.0001235071,  0.0001482795, -0.001159273,
                         -0.193004340,  0.0040684572, -0.0011592728,  0.251816442), byrow = TRUE, ncol = 4, nrow = 4)
    expect_equivalent(Sigma, SigmaRef);
});
