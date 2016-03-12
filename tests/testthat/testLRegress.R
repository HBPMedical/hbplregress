context("Node");

library(hbplregress);
library(jsonlite);

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

    xest <- LRegress(data, ycolumn, Acolumns);

    beta <- xest$coefficients;
    Sigma <- xest$residuals;

    expect_equivalent(beta, c(16.18750837,0.55604708,0.02711566,-4.80906215));
    SigmaRef <- matrix(c( 1.026124730, -0.0330411429, -0.0036317683, -0.193004340,
                         -0.033041143,  0.0017638153, -0.0001235071,  0.004068457,
                         -0.003631768, -0.0001235071,  0.0001482795, -0.001159273,
                         -0.193004340,  0.0040684572, -0.0011592728,  0.251816442), byrow = TRUE, ncol = 4, nrow = 4)
    expect_equivalent(Sigma, SigmaRef);
});

#############################################################
# function to generate random proportions whose rowSums = 1 #
#############################################################
props <- function(ncol, nrow, var.names=NULL){
    if (ncol < 2) stop("ncol must be greater than 1")
    p <- function(n){
        y <- 0
        z <- sapply(seq_len(n-1), function(i) {
                x <- sample(seq(0, 1-y, by=.01), 1)
                y <<- y + x
                return(x)
            }
        )
        w <- c(z , 1-sum(z))
        return(w)
    }
    DF <- data.frame(t(replicate(nrow, p(n=ncol))))
    if (!is.null(var.names)) colnames(DF) <- var.names
    return(DF)
}

################################################################
# RANDOMLY INSERT A CERTAIN PROPORTION OF NAs INTO A DATAFRAME #
################################################################
NAins <-  NAinsert <- function(df, prop = .1){
    n <- nrow(df)
    m <- ncol(df)
    num.to.na <- ceiling(prop*n*m)
    id <- sample(0:(m*n-1), num.to.na, replace = FALSE)
    rows <- id %/% m + 1
    cols <- id %% m + 1
    sapply(seq(num.to.na), function(x){
            df[rows[x], cols[x]] <<- NA
        }
    )
    return(df)
}

############################################################
# GENERATE A RANDOM DATA SET.  CAN BE SET TO LONG OR WIDE. #
# DATA SET HAS FACTORS AND NUMERIC VARIABLES AND CAN       #
# OPTIONALLY GIVE BUDGET EXPENDITURES AS A PROPORTION.     #
# CAN ALSO TELL A PROPORTION OF CELLS TO BE MISSING VALUES #
############################################################
# NOTE RELIES ON THE props FUNCTION AND THE NAins FUNCTION #
############################################################
DFgen <- DFmaker <- function(n=10, type=wide, digits=2,
    proportion=FALSE, na.rate=0) {

    rownamer <- function(dataframe){
        x <- as.data.frame(dataframe)
        rownames(x) <- NULL
        return(x)
    }

    dfround <- function(dataframe, digits = 0){
      df <- dataframe
      df[,sapply(df, is.numeric)] <-round(df[,sapply(df, is.numeric)], digits)
      return(df)
    }

    TYPE <- as.character(substitute(type))
    time1 <- sample(1:100, n, replace = TRUE) + abs(rnorm(n))
    DF <- data.frame(id = paste0("ID.", 1:n),
        group= sample(c("control", "treat"), n, replace = TRUE),
        hs.grad = sample(c("yes", "no"), n, replace = TRUE),
        race = sample(c("black", "white", "asian"), n,
            replace = TRUE, prob=c(.25, .5, .25)),
        gender = sample(c("male", "female"), n, replace = TRUE),
        age = sample(18:40, n, replace = TRUE),
        m.status = sample(c("never", "married", "divorced", "widowed"),
            n, replace = TRUE, prob=c(.25, .4, .3, .05)),
        political = sample(c("democrat", "republican",
            "independent", "other"), n, replace= TRUE,
            prob=c(.35, .35, .20, .1)),
        n.kids = rpois(n, 1.5),
        income = sample(c(seq(0, 30000, by=1000),
            seq(0, 150000, by=1000)), n, replace=TRUE),
        score = rnorm(n),
        time1,
        time2 = c(time1 + 2 * abs(rnorm(n))),
        time3 = c(time1 + (4 * abs(rnorm(n)))))
    if (proportion) {
        DF <- cbind (DF[, 1:10],
            props(ncol=3, nrow=n, var.names=c("food", "housing", "other")),
            DF[, 11:14])
    }
    if (na.rate!=0) {
        DF <- cbind(DF[, 1, drop=FALSE], NAins(DF[, -1],
            prop=na.rate))
    }
    DF <- switch(TYPE,
        wide = DF,
        long = {DF <- reshape(DF, direction = "long", idvar = "id",
                varying = c("time1","time2", "time3"),
                v.names = c("value"),
                timevar = "time", times = c("time1", "time2", "time3"))
            rownamer(DF)},
        stop("Invalid Data \"type\""))
    return(dfround(DF, digits=digits))
}

test_that("Linear regression works on groupings", {

    set.seed(100);

    data <- DFgen(n=100);

    ycolumn <- "income";
    Acolumns <- c("n.kids","score", "time1", "time2", "time3");
    Agroups <- c("gender", "political");

    xest <- LRegress(data, ycolumn, Acolumns, Agroups);

    beta <- xest$coefficients;
    Anova <- xest$anova;

    expected_beta <- fromJSON("[95026.5887,-5388.9498,6311.3897,-7747.943,6892.6825,478.8107,-20314.8567,-15315.307,-20950.5903,-27977.7211,7817.3006,-34889.8209,-14405.5822,\"NA\"]");
    expected_anova <- fromJSON("[{\"Df\":1,\"Sum Sq\":7568383415.6476,\"Mean Sq\":7568383415.6476,\"F value\":3.5848,\"Pr(>F)\":0.0616,\"_row\":\"n.kids\"},{\"Df\":1,\"Sum Sq\":3456793797.5424,\"Mean Sq\":3456793797.5424,\"F value\":1.6373,\"Pr(>F)\":0.2041,\"_row\":\"score\"},{\"Df\":1,\"Sum Sq\":10755043093.765,\"Mean Sq\":10755043093.765,\"F value\":5.0942,\"Pr(>F)\":0.0265,\"_row\":\"time1\"},{\"Df\":1,\"Sum Sq\":4353749711.2506,\"Mean Sq\":4353749711.2506,\"F value\":2.0622,\"Pr(>F)\":0.1546,\"_row\":\"time2\"},{\"Df\":1,\"Sum Sq\":14016477.4257,\"Mean Sq\":14016477.4257,\"F value\":0.0066,\"Pr(>F)\":0.9352,\"_row\":\"time3\"},{\"Df\":7,\"Sum Sq\":12679583197.3894,\"Mean Sq\":1811369028.1985,\"F value\":0.858,\"Pr(>F)\":0.543,\"_row\":\"gender:political\"},{\"Df\":87,\"Sum Sq\":183677270306.979,\"Mean Sq\":2111232992.0342,\"_row\":\"Residuals\"}]");

    expect_equal(beta, expected_beta, tolerance = .01, check.attributes = FALSE);
    expect_equal(Anova, expected_anova, tolerance = .01, check.attributes = FALSE);
});
