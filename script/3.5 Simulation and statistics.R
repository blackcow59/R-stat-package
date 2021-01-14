### 3.5 시뮬레이션과 통계학



require(tidyverse)
install.packages("kableExtra")
require(kableExtra)
# 데이터 요약

# 데이터 불러오기
DBP <- read_delim("data/DBP.txt", delim = "\t")
DBP

# 변화량 변수 추가
DBP <- DBP %>% 
  mutate(DIFF = DBP5 - DBP1)

# 각 투약군 별 변화량 측정값
DBP$DIFF[DBP$TRT == "A"]
DBP$DIFF[DBP$TRT == "B"]



# 요약 통계량
DBP %>% 
  group_by(TRT) %>% 
  summarise(N = n(),
            Mean = mean(DIFF, na.rm = T),
            SD = sd(DIFF, na.rm = T),
            Min = min(DIFF, na.rm = T),
            Max = max(DIFF, na.rm = T),
            Median = median(DIFF, na.rm = T),
            Q1 = quantile(DIFF, probs = 0.25),
            Q3 = quantile(DIFF, probs = 0.75)) %>% 
  kbl %>% 
  kable_paper


# 데이터 시각화

# 변화량 데이터의 분포 확인(치료약에 대해서)
# ggplot() 사용
require(ggpubr)
DBP %>% 
  ggplot() + 
  aes(x = DIFF, fill = TRT) + 
  geom_density(color = "white",
               alpha = 0.2) +
  scale_x_continuous(limits = c(-25, 5)) +
  geom_histogram(aes(y = ..density..),
                 color = "white",
                 binwidth = 1.8, alpha = 0.4) + 
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal() -> p1
p1


DBP %>% 
  ggplot() +
  aes(x = TRT, y = DIFF, fill = TRT) +
  geom_boxplot(alpha = 0.4) +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal() -> p2
p2

# 두 개 플롯을 한 화면에 표시하기 위한 함수
ggarrange(p1, p2, ncol = 1, common.legend = TRUE)




# 위 데이터로 어떤 결론을 도출할 수 있을까?
  # 1. 혈압치료제 A는 4개월 복용 후 DBP를 감소시키는가?
  # 2. 위약 B를 4개월 복용 후에도 DBP를 감소시키는가?
  # 3. 치료약 A는 위약 B보다 효과가 있는가?
  # 4. 위약 대비 치료약 A의 효과는 어느정도인가?
  # 5. 다른 고혈압 환자가 치료제 A를 복용하면 어떤 효과가 있을까?
install.packages("tidymodels")
require(tidymodels)

# Question 1-2 : 일표본 t검정

DBP %>% 
  mutate_if(is.character, factor) %>% 
  group_by(TRT) %>% 
  nest %>% # group 변수 별로 데이터 나누기
  # split한 데이터셋 각각에 일표본 t-test 실시
  mutate(t_test_result = map(data, ~ t.test(.x$DIFF))) %>% 
  # t-test 결과로 부터 통계량 추출 후 tibble로 변환
  mutate(result = map(t_test_result, ~ tidy(.x))) %>% 
  select(TRT, result) %>% 
  unnest(cols = result) %>% 
  mutate_at(vars(statistic, conf.low, conf.high),
            format, digits = 3) %>% 
  mutate(p.value = format(p.value, digits = 2)) %>% 
  select(-method, -alternative) %>% 
  set_names(c("Treatment", "$\\hat{\\delta}$", "t-value", "p-value", "df", "LCL (95%)", "UCL (95%)")) %>% 
  kbl(escape = FALSE) %>% 
  kable_paper()


# Question 3-4 : 독립 이표본 t 검정
t.test(DIFF ~ TRT, data = DBP, var.equal = TRUE)






# DBP 예제에서 독립 이표본 t-검정을 시뮬레이션을 통해 확인

# 1. 현재 획득한 표본으로 부터 필요한 통계량 계산
n1 <- n2 <- 20
n <- n1 + n2 # 전체 표본 수
mu_real <- tapply(DBP$DIFF, DBP$TRT, mean)
sigma_real <- tapply(DBP$DIFF, DBP$TRT, sd)
delta_real <- -diff(mu_real)
sp <- sqrt(mean(sigma_real^2))
tval <- delta_real/(sp * sqrt(1/n1 + 1/n2))

set.seed(38317)
# 귀무가설이 참(mu_a = mu_b)이라는 가정 하에서
# DBP 변화량의 두 군 간 차이에 대해 10,000개의 표본(평행우주) 생성
# 단, 분산(표준편차)은 현재 획득한 표본으로부터 구한
# 합동표준편차(분산)과 동일하다고 가정

B <- 10000
delta_star <- replicate(B, rnorm(n, 0, sp))
md_star <- apply(delta_star, 2, mean)
sd_star <- apply(delta_star, 2, sd)
t_star <- md_star/(sd_star * sqrt(1/n1 + 1/n2))

par(mfrow = c(2, 2))
hist(md_star, nclass = 100,
     main = expression(Histogram ~ of ~ bar(delta)),
     xlim = c(-11, 2))
abline(v = delta_real, col = "red")
hist(sd_star, nclass = 100,
     main = expression(Histogram ~ of ~ s[p]))
abline(v = sp, col = "red")
hist(t_star, nclass = 100, 
     main = expression(Histogram ~ of ~ t[0]),
     xlim = c(-12.5, 3))
abline(v = tval, col = "red")
qqnorm(t_star); qqline(t_star)

# p-value 계산 : 귀무가설 하에 관찰한 통계량만큼 큰(작은) 값이 발생할 확률

length(which(t_star < tval))/B





# 중심극한정리

# CLT 시각화를 위한 일반 함수
clt_test <- function(rep = 300, # 반복 수
                     n = 50, 
                     mtrue = NULL, 
                     strue = NULL, 
                     FUN = rbinom, 
                     plot = TRUE, 
                     sim.out = FALSE, 
                     ...) {
  # browser()
  dots <- list(...)
  x <- matrix(nrow = n, ncol = rep)
  for (i in 1:n) {
    x[i, ] <- apply(matrix(drop(
      mapply(FUN, i*rep, MoreArgs = dots)), i, rep), 
      2, mean)
  }
  
  if (is.null(mtrue)) mtrue = NA
  if (is.null(strue)) strue = NA
  
  
  mf <- match.call()
  FUN.name <- as.character(mf$FUN)
  title_name <- switch(FUN.name, 
                       rbinom = "Binomial distribution", 
                       rpois = "Poisson distribution", 
                       runif = "Uniform distribution", 
                       rexp = "Exponential distribution", 
                       rnorm = "Normal distribution", 
                       rchisq = expression(chi^2 ~ distribution), 
                       rt = "t distribution")
  
  xlim <- quantile(x, c(0.005, 0.995))
  # norm_line_x <- seq(xlim[1], xlim[2], length = 300)
  if (plot) {
    for (i in 1:n) {
      dev.hold()
      hist(x[i, ], freq = FALSE, 
           main = title_name, 
           xlab = substitute(italic(bar(X)[N == i]), 
                             list(i = i)), 
           xlim = xlim)
      lines(density(x[i, ]), col = "red")
      if (!is.na(mtrue) && !is.na(strue)) {
        curve(dnorm(x, mtrue, strue/sqrt(i)), col = "blue", lty = 2, add = TRUE)
        legend("bottomright", legend = bquote(mu == .(sprintf("%.3f", mtrue)) ~  
                                                sigma/sqrt(n) == 
                                                .(sprintf("%.3f", strue/sqrt(i)))), 
               bty = "n", cex = 1.5)      
      }
      legend("topright", legend = bquote(bar(bar(x)) == 
                                           .(sprintf("%.3f", mean(x[i,]))) ~  
                                           hat(sigma)[bar(x)] == 
                                           .(sprintf("%.3f", sd(x[i,])))), 
             bty = "n", cex = 1.5)      
      dev.flush() #
      Sys.sleep(0.05)
    }
  }
  
  if (sim.out) return(x)
}

# binomial distribution
p <- 0.25; size = 1
clt_test(rep = 500, n = 50,
         FUN = rbinom, mtrue = size * p,
         strue = sqrt(size * p * (1 - p)), size = size, prob = p * size)



# 이항분포
# 예제
clt_test(rep = 500, n = 50,
         FUN = rbinom, mtrue = size * p,
         strue = sqrt(size * p * (1 - p)), size = size, prob = p)



# 포아송분포
# 예제
l <- 3
clt_test(rep = 300, n = 50, 
         FUN = rpois, mtrue = l, 
         strue = sqrt(l), lambda = l)



# 연속균일분포
# 예제
a = 0; b = 1
clt_test(rep = 300, n = 50,
         FUN = runif, mtrue = 0.5 * (a + b),
         strue = sqrt((b - a)^2/12), min = a, max = b)



# 카이제곱분포
# 예제
nu = 3
clt_test(rep = 300, n = 50,
         FUN = rchisq,
         mtrue = nu,
         strue = sqrt(2*nu), df = nu)






# 신뢰구간의 정확한 의미


# 예제 : sleep 데이터
# 10명의 대상자가 수면제 1과 수면제 2를 복용 시 수면시간 증가량 데이터
# 군 별 기술통계량 요약

# 독립 이표본 t검정에서 두 군간 평균 차이에 대한 95% 신뢰구간

sleep %>% 
  group_by(group) %>% 
  summarise(N = n(),
            Mean = mean(extra),
            SD = sd(extra)) -> desc_sleep

desc_sleep %>% 
  kbl() %>%
  kable_paper()



# 독립 이표본 t-검정

mean_diff <- -diff(desc_sleep$Mean); mean_diff
sp <- sqrt(mean(desc_sleep$SD^2))
tval <- mean_diff/(sp * sqrt(sum(1/desc_sleep$N))); tval

# df = n1 + n2 -2 = 10 + 10 - 2 = 18
p.value <- 2*(1-pt(abs(tval), df = 18)); p.value



# 해당 데이터가 주어졌을 때 두 군간 평균 차이에 대한 95% 신뢰구간

alpha <- 0.05
lcl <- mean_diff - qt(1 - alpha/2, 18) * sqrt(sum((desc_sleep$SD^2)/10))
ucl <- mean_diff + qt(1- alpha/2, 18) * sqrt(sum((desc_sleep$SD^2)/10))
lcl; ucl


# check
t.test(extra ~ group, data = sleep, var.equal = T)





N <- 100
n1 <- n2 <- 10
m_true <- -2 # mu_x1 - mu_x2
mu_contain_count = 0

plot(c(1,N),
     c(-8, 2),
     type = "n",
     ylab = "95 % CI",
     xlab = "Replicates")
abline(h = m_true, col = "red", lty = 2)

set.seed(1313)
for(i in 1:N){
  x1 <- rnorm(n1, 1, 1.79)
  x2 <- rnorm(n2, 3, 2.0)
  md <- mean(x1) - mean(x2)
  lcl <- md - qt(1 - alpha/2, n1 + n2 - 2) * sqrt((sd(x1)^2 + sd(x2)^2/10))
  ucl <- md + qt(1 - alpha/2, n1 + n2 - 2) * sqrt((sd(x1)^2 + sd(x2)^2)/10)
  contain_mu <- lcl <= m_true & m_true <= ucl
  if(contain_mu) mu_contain_count <- mu_contain_count + 1
  segments(i, lcl, i, ucl, col = as.numeric(!contain_mu) + 1)
}
legend("bottomright", 
       legend = sprintf("# of not containing true mu: %d",  N - mu_contain_count), 
       bty = "n")



# 위 sleep 데이터의 결과가 모집단의 특성을 충분히 반영했다고 가정하고, 독립 이표본 t검정에서 귀무가설이 참일 때 두 집단 간 평균 차이에 대한 95% 신뢰구간이 0을 포함하는 빈도

m_true <- 0 # mu_x1 - mu_x2
mu_contain_count = 0

plot(c(1,N),
     c(-6, 3),
     type = "n",
     ylab = "95 % CI",
     xlab = "Replicates")
abline(h = m_true, col = "red", lty = 2)

set.seed(1313)
for(i in 1:N) {
  x1 <- rnorm(n1, 0.75, 1.79)
  x2 <- rnorm(n2, 2.23, 2.0)
  md <- mean(x1) - mean(x2)
  lcl <- md - qt(1 - alpha/2, n1 + n2 - 2) * sqrt((sd(x1)^2 + sd(x2)^2)/10)
  ucl <- md + qt(1 - alpha/2, n1 + n2 - 2) * sqrt((sd(x1)^2 + sd(x2)^2)/10)
  contain_mu <- lcl <= m_true & m_true <= ucl
  if(contain_mu) mu_contain_count <- mu_contain_count + 1
  segments(i, lcl, i, ucl, col = as.numeric(!contain_mu) + 1)
}
legend("bottomright",
       legend = sprintf("# of not containing true mu : %d", N - mu_contain_count),
       bty = "n")






## 3.5.0.1 P값에 대한 이해

# Simulation setting 1 : 귀무가설이 참인 경우
# 10,000번 반복

nsim <- 10000
p <- numeric(nsim)
set.seed(19780904)
for(i in 1:nsim) {
  x <- rnorm(n = 30, mean = 100, sd = 20)
  y <- rnorm(n = 30, mean = 100, sd = 20)
  p[i] <- t.test(x, y)$p.value
}

hist(p, main = "Histogram of p-value under the null hypothesis",
     xlab = "Observed p-value", col = "#87DAFA")



# Simulation setting 2 : 대립가설이 참인 경우
# 첫 번째 모의실험과 동일한 크기로 진행

set.seed(19780904)
for (i in 1:nsim) {
  x <- rnorm(n = 30, mean = 100, sd = 20)
  y <- rnorm(n = 30, mean = 96, sd = 20)
  p[i] <- t.test(x, y)$p.value
}

hist(p, main = "Histogram of p-value under the alternative hypothesis with ES = 0.2",
     xlab = "Observed p-value", col = "#87DAFA")



# Simulation setting 3: 대립가설이 참, 표본 크기를 증가시킨 경우

set.seed(19780904)
for (i in 1:nsim) {
  x <- rnorm(n = 100, mean = 100, sd = 20)
  y <- rnorm(n = 100, mean = 96, sd = 20)
  p[i] <- t.test(x, y)$p.value
}

hist(p, main = "Histogram of p-value under the alternative hypothesis wiht 100 samples per group",
     xlab = "Observed p-value", col = "#87DAFA")




# Simulation setting 4 : 대립가설이 참, 효과크기가 1.0인 경우

set.seed(19780904)
for(i in 1:nsim) {
  x <- rnorm(n = 30, mean = 100, sd = 20)
  y <- rnorm(n = 30, mean = 80, sd = 20)
  p[i] <- t.test(x, y)$p.value
}
hist(p, main = "Histogram of p-value under the alternative hypothesis : n = 30, eta = 1.0",
     xlab = "Observed p-value", col = "#87DAFA")
