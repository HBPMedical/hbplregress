context("Node");

library(hbplregress);

test_that("Linear regression is correct at the node level", {

    set.seed(100);

	N <- 20;
    M <- 20;

    A <- matrix(rnorm(N*M,mean=10,sd=4), N, M);
    colnames(A) <- sapply(seq(1,20), function (x) {paste("a",x,sep="")});
    x <- matrix(rnorm(M,mean=2,sd=1), M, 1);
    
    y <- A%*%x;
    colnames(y) <- c("y");
    data <- as.data.frame(cbind(x, A));
    
    xest <- LRegress_Node(data, colnames(y), colnames(A));
    beta <- xest[[1]];
    Sigma <- xest[[2]];
    
    expect_equal(beta[1],      0.71517135,   tolerance = 1e-6);
    expect_equal(beta[10],     2.60860245,   tolerance = 1e-6);
    expect_equal(beta[20],     1.06780877,   tolerance = 1e-6);
    expect_equal(Sigma[1,1],   0.0363542484, tolerance = 1e-6);
    expect_equal(Sigma[1,5],  -0.0033319972, tolerance = 1e-6);
    expect_equal(Sigma[10,1], -0.0139791712, tolerance = 1e-6);
    expect_equal(Sigma[10,3], -0.056881108,  tolerance = 1e-6);
    expect_equal(Sigma[10,5],  0.0314686605, tolerance = 1e-6);
    expect_equal(Sigma[20,1], -0.0019351168, tolerance = 1e-6);
    expect_equal(Sigma[20,5],  0.0368693618, tolerance = 1e-6);
});
