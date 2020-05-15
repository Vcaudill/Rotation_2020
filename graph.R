
testfile <- read.table("text_test.txt")
readfile <- read.table("file")
thefile <- read.table("cut/exp_K_10_hK_100_sigma_K_1.0_sigma_C_0.4_trial_10.txt", header = TRUE)


names(testfile)[1] <- "gen"
names(testfile)[2] <- "p_pheno"
names(testfile)[3] <- "h_pheno"
names(testfile)[4] <- "p_div"
names(testfile)[5] <- "h_div"

testfile$dif_pheno <- testfile$p_pheno-testfile$h_pheno

plot(testfile$gen, testfile$p_div, type = "l", col = "red" , xlim = c(0,500),ylim = c(-6,6))
points(testfile$gen, testfile$h_div, type = "l", col = "blue")
points(testfile$gen, testfile$dif_pheno, type = "l", col = "green")


plot(testfile$gen, testfile$p_pheno, type = "l", col = "red" , xlim = c(0,1000))
points(testfile$gen, testfile$h_pheno, type = "l", col = "blue")
points(testfile$gen, testfile$dif_pheno, type = "l", col = "green")
