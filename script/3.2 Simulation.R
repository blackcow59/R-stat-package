### 3.2 통계 시뮬레이션에 활용되는 수학 함수 및 분포 함수


## 3.2.1 수학함수

# R은 광범위한 수학 함수를 내장하고 있고 다음 열거한 함수 목록은 그 일부임

# exp() : 지수 (e)를 밑으로 하는 지수 함수
# log() : 자연 로그 함수
# log10() : 10을 밑으로 하는 로그
# sqrt() : 제곱근
# abs() : 절대값
# sin(), cos(), tan() ... : 삼각함수
# min(), max() : 벡터 내 최솟값과 최댓값 반환
# which.min(), which.max() : 벡터 내 최댓값과 최솟값에 대한 인덱스 반환
# sum(), prod() : 벡터 원소들의 함과 곱 결과 반환
# cumsum(), cumprod() : 벡터 원소들의 누적합과 누적곱
# round(), floor(), ceiling() : 수치형 값의 반올림, 내림, 올림
# factorial() : 펙토리얼 함수
# choose() : 조합 함수


# 확장예제1 : 확률계산
  # Pi : n개의 독립적인 사건이 있고 i번째 사건이 발생할 확률
  # n = 3 일 때, 각 사건의 이름을 각각 A,B,C 라고 할 때 이중 사건 하나가 발생할 확률

# P(사건 하나가 발생할 확률) = 
# P(A가 일어나고 B와 C가 일어나지 않을 확률) +
#   P(A가 일어나지 않고 B가 일어나고 C가 일어나지 않을 확률) +
#   P(A, B 가 일어나지 않고 C만 일어날 확률)

  # 구현함수
# 벡터 p에서 p-i 계산 함수

probability_i <- function(p) {
  notp <- 1 - p
  tot <- 0
  for (i in 1:length(p))
    tot <- tot + p[i] * prod(notp[-i])
  return(tot)
}

# test
set.seed(3)
p <- runif(3 ,0 ,1) # 0-1 사이 값 3개를 균일분포로부터 추출
probability_i(p)



# 확장예제 2 : 누적합, 누적곱
# cumsum, cumprod 함수 사용 예시
x <- c(2, 4, 1, 3, 7, 8)
cumsum(x); cumprod(x)



# 확장예제 3 : 최솟값, 최댓값
# which.min, which.max 사용 예시

set.seed(100)
x <- sample(1:100, 100)
idx_min <- which.min(x)
x[idx_min]

idx_max <- which.max(x)
x[idx_max]


# min(), max(), pmin(), pmax() 비교

set.seed(5)
x <- runif(5, 2, 4)
y <- runif(5, 2, 4)
z <- cbind(x, y)

min(z); max(z) # z의 전체 값 중 최솟값과 최댓값 반환환
pmin(z); pmax(z) # 아무 값을 반환하지 않음음


# 두 열을 비교해 각 행에서 최솟값, 최댓값을 반환
pmin(z[, 1], z[, 2])
pmax(z[, 1], z[, 2])



# 확장예제4 : 미분/적분
  # 문자의 미분 및 수치 적분 가능

# 도함수 구하기

# D() 함수 사용
dx <- D(expression(exp(x^2)), "x") # exp(x^2)을 x에 대해서 1차 미분한 도함수
set.seed(3)
x <- runif(3, 1, 2)
eval(dx) # 위 입력한 x에 대한 도함수 값 출력

# deriv() 함수 사용
grad <- D(expression(x*sin(x)), "x")
# 도함수를 R의 function으로 바로 반환 가능
dx2 <- deriv(expression(x*sin(x)), "x", function.arg = TRUE)
dx2(x)

# 수치 적분
# integrate() 함수 사용
# 주어진 함수의 적분식을 구한 후, 입력 구간에 대한 적분값 계산

integrate(f = function(x) x^2, lower = 0, upper = 1)



# 확장예제 응용 : Newton-Raphon method
# 임의의 함수 f(x)가 주어졌을 때 f(x) = 0(f(x)의해)를 만족하는 x를 반복적인 수치계산을 통해 찾는 방법

  # Newton-Raphson(N-R)방법 적용 시 f(x)의 만족 조건
    # x의 특정 범위 내에서 f(x) = 0를 만족하는 유일한 실수값 존재
    # f(x)는 미분 가능한 함수



newton_raphson <- function(FUN, # 함수
                           x0 = 1, # 초기값
                           max_iters = 5000, # 최대 반복 횟수
                           tol = 1.0e-9,
                           range = c(-Inf, Inf),
                           ...)
{
  iters <- 1;
  grads <- deriv(as.expression(body(FUN)), "x", function.arg = TRUE)
  # grads 반환값 중 "gradient" 값 = f'(x0)
  gap <- x0 - FUN(x0)/attr(grads(x0), "gradient")
  
  while(iters <- max_iters & abs(gap) > tol) {
    # x_new 계산
    x_new <- x0 - FUN(x0)/attr(grads(x0), "gradient")
    gap <- FUN(x_new)
    # x_new 가 범위를 벗어난 경우 처리
    if(x_new <= range[1]) x_new <- range[1]
    if(x_new >= range[2]) x_new <- range[2]
    iters <- iters + 1
    x0 <- x_new # 초기값 업데이트
  }
  if (x_new == range[1] | x_new == range[2])
    warning("마지막 점이 x범위의 경계선 상에 있습니다.")
  if (iters > max_iters)
    warning("최대 반복 때 까지 해를 찾지 못했습니다.")
  cat("x 가", x_new, "일 때 함수값:", FUN(x_new), "\n")
  return(list(solution = x_new, iteration = iters))
  
}

# test 
f <- function(x) 5 * x^3 - 7 * x^2 - 40 * x + 100
newton_raphson(FUN = f,
               x0 = 1,
               range = c(-10, 10)) -> sols





## 3.2.2 통계 분포 함수

# R은 현존하는 대부분의 통계 확률 분포 함수를 제공하고 "접두사 + 분포이름" 형태의 함수명을 갖고 있으며, 보통 다음과 같은 접두사를 통해 분포 함수 생성

# d : 밀도(density)의 약어로 확률밀도함수(probability density function, pdf) 또는 이산형 분포의 확률질량함수(probability mass function, pmf)
# q : 분위수(quantile)의 약어로 상위 %에 해당하는 x값을 반환
# p : 누적분포함수(cumulative density function, cdf)
# r : 특정 분포로부터 난수(확률변수) 생성

# 예 : dnorm(), qnorm(), prnorm(), rnorm() 은 정규분포 관련 함수임



# 예제 : 확률 분포 함수
# 카이제곱분포
x <- seq(0 ,30, by = 0.1)
y <- dchisq(x, df = 3) # 자유도가 3인 카이제곱분포 밀도 함수
plot(x, y, type = "l",
     bty = "n",
     xlab = "", ylab = "",
     main = expression(paste("PDF of ", ~chi^2, " distribution")),
     lwd = 2,
     cex.main = 2)

# P(5 < V < 10)
pchisq(10, df = 3) - pchisq(5, df = 3)


# 그림에 표현
idx <- x >=5 & x <= 10
polygon(c(5, x[idx], 10),
        c(0, y[idx], 0),
        col = "blue",
        border = "blue")
abline(h = 0, col = "darkgray")
text(x = 10, y = 0.05, cex = 2,
     bquote(P({5 <= V} <= 10) ==
              .(sprintf("%.3f", pchisq(10, df = 3) - pchisq(5, df = 3)))),
     adj = 0)

# 분위수
qchisq(pchisq(10, df = 3), df = 3)

# 난수 생성
v <- rchisq(1000, df = 3)
mean(v)


# 난수 생성의 방법
  # 난수(random number) : 어떤 방법으로도 예측될 수 없는 일련의 수열(숫자)
  # 통계적 의미로 난수는 특정 범위(보통 0에서 1사이)의 균일분포 에서 추출된 표본들의 관찰값으로, 임의의 확률분포(예 : 정규분포, 지수분포 등)를 따르는 확률변수와는 구별됨
    # 보통 확률 변수는 균일분포를 따르는 확률변수로부터 적절한 변환을 통해 얻을 수 있음
  # 난수를 발생하려면 어떤 알고리즘이 필요하고 알고리즘은 일정한 규칙에 의해 구현되기 때문에 컴퓨터로 발생한 남수는 엄밀한 의미에서 난수가 아님.
  # 이를 구별하기 위해 보통 컴퓨터로 생성한 난수를 유사난수라 칭함
  # 난수 생성을 위한 알고리즘으로 합동법, 역변환법 등이 널리 사용됨
  # 통계 시뮬레이션에서는 특히 변수변환방법을 통해 확률변수 생성