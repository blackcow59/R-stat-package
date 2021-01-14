## 2.5 함수(Function)

# 함수 : 특정한 목적을 위한 연산을 수행하기 위해 명명된 일련의 문장(추상화)
#   예 : sum(x) -> 벡터 x의 값을 모두 합산하는 기호로 "sum"이라고 명명된 내장함수
#   R콘솔에서 함수 명칭(예 : sum)을 입력 후 실행하면 함수 내부 확인가능


sum


# 함수를 사용해야만 하는 이유
#   매우 큰 프로그램 작업을 해야 할 경우 함수를 통해 작업 단위 별로 분할 가능
#   한번 작성한 함수는 재사용 가능
#   프로그램의 체계적 관리가 가능하기 떄문에 유지 및 보수가 용이
#   프로그램 코드의 간결화








## 2.5.1 함수의 정의

# function 이라는 R의 예약어를 통해 사용자 함수 정의
# 함수 정의시 함수의 명칭을 반드시 부여해야 함

# 함수이름 <- function()

# 함수는 일반적으로 인수(argument)로 입력값을 전달 받으면 그 결과값을 반환(return)
# 함수의 인수와 반환에 따라 다음과 같이 4가지 유형의 함수 정의 가능
    # 인수를 갖는 함수
    # 인수를 갖지 않는 함수
    # 값을 반환하는 함수
    # 값을 반환하지 않는 함수





# (1) 인수를 갖는 함수

# (모)분산을 계산하는 함수
var_pop <- function(x) {
  n <- length(x)
  if( n < 2) {
    stop("적어도 두 개 이상의 관찰값이 존재해야 합니다")
  }
  mx <- mean(x)
  v <- sum((x-mx)^2)/n
  return(v) # 결과를 반환하는 함수 : v를 함수의 출력값으로 설정
}


# test
set.seed(1) # 동일한 난수 생성을 위해 seed 번호 부여
x <- rnorm(1)
var_pop(x)

set.seed(1000)
x <- rnorm(1000, 2, 4)
var_pop(x)





# (2) 인수를 갖지 않는 함수

print_lyrics_let_it_be <- function() {
  print("When I find myself in times of trouble, ")
  print("Mother Mary comes to me.")
  print("Speaking words of wisdom 'let it be'.")
}

print_lyrics_let_it_be()


# 주사위를 돌리는 함수
rolling_dice <- function() {
  sample(1:6, 1, replace = TRUE)
}

rolling_dice(); rolling_dice(); rolling_dice()





# (3) 값을 반환하는 함수

manual_mean <- function(x) {
  n <- length(x)
  sumi <- 0
  for (i in 1:n) {
    sumi <- sumi + x[i]
  }
  return(sumi/n)
}


set.seed(20)
x <- sample(1:200, 20, replace = FALSE) # 1 ~ 200 중 랜덤하게 20개 추출 (비복원)
manual_mean(x)


# 미리 정의하지 않은 인수를 입력한 경우
set.seed(4)
na_idx <- sample(1:length(x), 4)
x[na_idx] <- NA
manual_mean(x, na.rm = TRUE)





# (4) 값을 반환하지 않는 함수(void function)

summary_mean <- function(x, ...) {
  n <- sum(!is.na(x))
  mx <- sum(x, ...)/n
  cat("Data :", sprintf("%.2f", x), "\n") # 소숫점 2째 자리까지 출력
  cat("전체 관찰값 개수(결측 제외) = ", n, "\n")
  cat("산술평균 = ", mx, "\n")
}


set.seed(20)
x <- rnorm(20)
summary_mean(x)

result <- summary_mean(x)
result


x[na_idx] <-NA
# ...를 통해 미리 정하지 않은 인수를
# 함수 내부에서 호출한 다른 함수로 전달 가능
summary_mean(x, na.rm = TRUE)











## 2.5.2 함수의 인수 전달 방법

# 함수는 입력값(input)을 가지며, 이러한 입력값은 함수의 인수(argument)에 해당 값을 할당함으로써 입력값이 함수로 전달됨
# 함수의 인수 정의는 내 마음대로 가능(개수 무관)
# R에서 함수 호출 시 인수 전달은"값"을 호출 하는 방식(call by value)



# 예시

# 두 변수의 값을 바꾸는 함수 : swap
swap <- function(x, y) {
  temp <- x
  x <- y
  y <- temp
  cat("두 값이 바뀌었습니다.", sprintf("x = %d, y = %d", x, y), "\n")
}
x <- 3; y <- 10
swap(x, y)

x; y # 두 값이 바뀌지 않음



# 인수를 전달하는 방법은 다음 두 가지임
  # 인수의 위치 순서에 의한 전달 : 정의한 인수의 순서대로 각 인수에 대응하는 값을 전달
  # 인수의 이름에 의한 전달 : 위치와 관계 없이 정의한 인수의 이름을 지정하여 값을 전달



# 표준편차 계산 함수 : stdev
stdev <- function(x, na.rm = TRUE) {
  if (is.matrix(x)) apply(x, 2, sd, na.rm = na.rm)
  else if (is.vector(x)) sqrt(var(x, na.rm = na.rm))
  else if (is.data.frame(x)) sapply(x, sd, na.rm = na.rm)
  else sqrt(var(as.vector(x), na.rm = na.rm))
}

set.seed(1000)
X <- matrix(rnorm(1000), 100, 10)
# matrix 함수에 인수가 이상함 rnorm()에 들어가야 하는 인수같음
x <- rpois(50, lambda = 10) # 포아송 분포(lamda = 10)에서 50개 난수 추출
dat <- mtcars


# (1) 순서에 의한 전달
stdev(X, T); stdev(X) # 동일한 결과

stdev(x)
stdev(dat)

stdev(TRUE, dat) # 오류 why ??



# (2) 이름에 의한 전달
set.seed(5)
na_idx <- sample(1:50, 5)
x[na_idx] <- NA

stdev(na.rm = T, x = X)

stdev(dat = dat, na.rm = TRUE) # 오류 why ???












## 2.5.3 함수의 기본 구성 요소

# function() 에서 ()안의 부분(일반적으로 첫 번째 줄)을 머리(header) 부분
  # 함수의 초기 형태(매개변수 또는 인수의 형태)를 지정
# 연산 또는 명령이 수행되는 부분은 함수의 몸통(body)부분 ({} 로 표시)
# 인수(argument) : 함수의 기능을 선택적으로 조정하는 parameter로 함수 안에서 작동하는 매개변수들을 통칭
  # 인수는 argument 또는 argument = default value 로 설정
  # 복수의 인수는 콤마(,)로 구분 -> fun_name <- function(arg1, arg2, arg3)
  # 특수 인수 ... : 어떠한 개수의 인수를 함수로 전달할 후 있음
    # 일반적으로 인수의 개수가 불특정하거나 함수 안에서 다른 함수를 호출할 때 특정 인수를 다른 함수로 전달시킬 때 유용(위 예시 참고)



# (1) 인수에 default 값을 주지 않은 함수
fun_without_arg_default <- function(x, y) {
  x*y
}

set.seed(10)
a <- sample(1:20, 10, replace = TRUE) # 복원 추출
a[7] <- NA
b <- 5

fun_without_arg_default(a, b)



# (2) 인수에 default 값을 부여한 함수
fun_without_arg_default <- function(x = 5, y = 8) {
  x * y
}

fun_without_arg_default()


trim_mean <- function(x, trim = 0, na.rm = F) {
  mean(x, trim = trim, na.rm = na.rm)
}
trim_mean(a)

trim_mean(x = a, trim = 0.2, na.rm = TRUE)

trim_mean(a, 0.3, TRUE)




# (3) ... 인수 사용 예제
# List() 함수를 이용해 `...` 에 해당하는 인수들을 리스트 객체로 만든 후
# 이를 함수에서 사용

dot_example <- function(x, ...) {
  # browser()
  trim <- 0
  na.rm <- FALSE
  
  
  dots <- list(...) # ...에 해당하는 인수 추출
  for (arg in names(dots)) {
    if (arg == "trim") trim <- as.numeric(dots[arg])
    if (arg == "na.rm") na.rm <- as.logical(dots[arg])
  }
  
  mean(x, trim = trim, na.rm = na.rm)
}

dot_example(a)

set.seed(30)
a <- sample(1:30, 15, replace = TRUE) # 복원추출
dot_example(a)

a[9] <- NA
dot_example(a)

dot_example(a, trim = 0.1, na.rm = TRUE)




# (4) `...` 인수가 함수 내 사용(호출)된 다른 함수의 인수로 전달하는 경우
# summary_mean() 함수 예제와 유사

mean_manual <- function(x, ...) {
  mean(x, ...)
}

set.seed(30)
x <- rnorm(30, mean = 10, sd = 5)
na_idx <- sample(1:30, 3, replace = TRUE)
xna <- x; xna[na_idx] <- NA

mean_manual(x)
mean_manual(xna)
mean_manual(xna, na.rm = TRUE)
mean_manual(x = xna, trim = 0.2, na.rm = TRUE)







# 인수 관련 몇 가지 유용한 함수들

# args() : 특정 함수에서 사용되는 인수 확인
args(fun_without_arg_default)
args(rnorm)

# body() : 함수의 몸체 조회
body(var_pop)
body(dot_example)

# match.arg() : 인수를 매칭하는 함수로 매치랑 대상의 인수를 지정
  # arg : 매치할 대상 인수 지정
  # choice : 매치될 인수값 목록
  # several.ok : 복수 선택 여부(TRUE/FALSE)

# 인수의 매치
match.arg(arg = c('med', 'max'),
          choices = c('mean', 'median', 'iqr', 'minimum', 'maximum', 'range'),
          several.ok = TRUE)

match.arg(arg = c('median', 'maximum'),
          choices = c('mean', 'med', 'iqr', 'minimum', 'max', 'range'),
          several.ok = TRUE) # 오류 why ??

match.arg(arg = c('med', 'max'),
          choices = c('mean', 'median', 'iqr', 'minimum', 'maximum', 'range'),
          several.ok = FALSE)


# match.arg() 함수 응용
# 중심값 관련 통계량 계산 함수
# 평균(mean), 절삭평균(trimmed mean), 중앙값(median), 최빈수(mode) 계산


pkg_list <- rownames(installed.packages()) # 설치된 패키지 목록 
if(!("DescTools" %in% pkg_list))
  install.packages("DescTools") # 최빈수를 구하기 위한 패키지 설치  

center <- function(x,
                   type = c('mean', "trimmed", "median", "mode"),
                   ...
)
{
  # browser()
  trim = 0 ; na.rm = FALSE # dot 인수 초기값
  type <- match.arg(type)
  dots <- list(...)
  for(arg in names(dots)) {
    if (arg == "trim") trim <- as.numeric(dots[arg])
    if (arg == "na.rm") na.rm <- as.logical(dots[arg])
  }
  
  switch(type,
         mean = mean(x, na.rm = na.rm),
         trimed = mean(x, trim = trim, na.rm = na.rm),
         median = median(x, na.rm = na.rm),
         mod = DescTools::Mode(round(x, 1), na.rm = na.rm)
         # DescTools 패키지 내 Mode 함수를 worksapce에 불러오지 않고 사용
         )
}

set.seed(100)
x <- rchisq(100, df = 3) # 자유도가 3인 카이제곱분포에서 난수 추출
xna <- x; xna[na_idx] <- NA

center(x, 'mean'); center(x, 'me')
center(x, 'trimmed', trim = 0.1)
center(x, 'median')
center(x, 'mode')
center(xna, "median")
center(xna, 'median', na.rm = TRUE)









# 함수 제어 관련 주요 함수

# return() : 계산 된 결과를 반환하는 함수로 함수의 흐름에서 return() 이 나타나면 결과값을 반환하고 함수 종료
  # 강제 종료가 필요한 경우 응용 가능


# (1) 객체 반환
set.seed(100)
x <- rnorm(100, mean = 24, sd = 2.2)
value_retun1 <- function(x) {
  tot <- sum(x)
  n <- length(x)
  result <- list(size = n, total = tot, average = mean(x), stdev = sd(x))
  return(result)
}

value_retun1(x)

desc <- value_retun1(x)
desc$stdev

value_retun2 <- function(x) {
  return(sum(x) / length(x))
}
value_retun2(x)




# (2) 강제 종료 시 활용
value_retun3 <- function(x) {
  if (anyNA(x)) return
  return(sum(x) / length(x))
}

xna <- x; xna[na_idx] <- NA

value_retun3(xna)
value_retun3(x)





# stop() : 예외처리 함수의 일종으로 특정 조건일 경우 (오류) 메세지를 출력하고 함수 종료
  # 인수로 문자열을 가짐

# (1) stop() 함수 사용
# 복소수 값을 실수와 허수로 분할

split_complex <- function(z) {
  if(!is.complex(z))
    stop("입력값이 복소수가 아닙니다")
  re <- Re(z)
  im <- Im(z)
  return(list(real = re, imaginary = im))
}

split_complex(pi)
split_complex(23 + 7i)











## 2.5.4 함수의 적용 범위(scoping rule)

# Scoping rule : 변수 또는 객체가 어디에서 사용 가능한지를 결정하는 규칙
  # 1. 매개변수(parameter) : 함수를 적용할 때 사용되는 변수로 인수로부터 발생함
    # 함수의 인수 리스트에서 인수값이 매개변수로 할당
  # 2. 지역변수(local variable) : 함수의 몸체 부분에서 정의된 변수들을 지칭하며 함수의 종료와 동시에 재사용 불가
  # 3. 전역변수(global variable) : 함수의 외부(workspace)에서 정의된 변수로 함수 내부에서 값을 할당하지 않더라도 사용 가능

# (1) 매개변수, 지역변수, 전역변수 구분
x <- 10 # 전역변수
y <- 5 # 전역변수


scope1 <- function(x) {
  y <- x^2
  print(x) # 매개변수
  print(y) # 지역변수
}

x; y # 전역변수가 출력

scope1(x = 10)

# 작업공간에서 x와 y는 각각 10, 5 값이 할당
# 작업공간 상에서 x,y값은 변하지 않음
# 지역변수 y의 사용 범위는 함수 몸체이기 때문에 함수 밖에 있는 y는 값이 변하지 않음


x <- 10 # 전역변수
y <- 5 # 전역변수

rm(z)

scope2 <- function(x) {
  y <- x^2
  print(x) # 매개변수
  print(y) # 지역변수
  print(z)
}

scope2(x = 5)

z <- 13 # 전역변수 z 할당

scope2(x = 5)


# 함수 외부와 내부 모두에서 z가 정의되지 않았기 때문에 에러 출력
# 작업공간 상에 z를 정의한 경우 함수 내부에서 workspace에서 정의한 z를 그대로 사용 -> 함수 외부와 내부 자유로이 사용 가능한 변수를 자유변수(free variable)이라고 지칭함.


# 지역변수의 사용 범위는 함수 몸체 안이지만 그 범위를 밖으로 확장할 수 있음 -> <<- 또는 ->> 사용

# 지역변수의 확장 예제

x <- 1; y <- 2; z <- 3; k <- 10
scope3 <- function(x) {
  y <<- x + 10
  y * 3 ->> z
  
  print(x) # 매개변수
  print(y) # 지역변수
  print(z) # 지역변수
  print(k) # 자유변수
}

x;y;z;k

scope3(x = 2)
x; y; z; k




# 하나의 함수 내부에 또 다른 함수 생성 가능
mean_manual12 <- function(x) {
  tot <- sum(x)
  size <- function(x) {
    return(length(x))
  }
  return(tot/size(x))
}

mean_manual12(1:10)












## 2.5.5 재귀 호출 함수(recursive call funtion)

# 함수 자신을 다시 호출하는 함수로 직관적으로 이해하기 쉽고 간결함
  # 재귀함수 작성 시 재귀호출을 탈출하는 조건을 명확히 하는 것이 관건

# 예제1 : 계승(factorial) 계산하기

f(3) = 3 * f(2)
           f(2) = 2 * f(1)
                      f(1) = 1

# 위 과정을 함수로 구현
factorial_manual <- function(n) {
  if (n == 0) return(1)
  return(n * factorial_manual(n-1))
}

# test
factorial_manual(3)
factorial_manual(10)

# R 내장함수로 검증
factorial(10)








# 예제 : 하노이 탑(tower of Hanoi)

# 문제 : 3개의 기둥 A, B, C 가 있고, 기둥 A에 N개의 원판이 크기 순서대로 쌓여져 있을 때(제일 밑에 원판이 가장 큼), 모든 원판을 기둥 C로 옮기기

# 조건 
  # 한 번에 하나의 원판만 옮길 수 있음
  # 큰 원판이 작은 원판 위에 있으면 안됨


# Solution
# 원판의 크기가 제일 작은 것 부터 큰 것 까지 각각 1,2,3,번을 부여 했을 때
# 1 번 원판을 봉 A에서 C로 옮김 (A→C)
# 2 번 원판을 봉 A에서 B로 옮김 (A→B)
# 1 번 원판을 봉 C에서 B로 옮김 (C→B)
# 3 번 원판을 봉 A에서 C로 옮김 (A→C)
# 1 번 원판을 봉 B에서 A로 옮김 (B→A)
# 2 번 원판을 봉 B에서 C로 옮김 (B→C)
# 1 번 원판을 봉 A에서 C로 옮김 (A→C)

# 원판이 3개인 경우 총 7번의 이동이 필요 -> n개의 원판이 있을 경우 2^n-1번의 이동이 필요


# 알고리즘 구현
move_hanoi <- function(n, from, to, via) {
  if (n == 1) {
    print(sprintf("%d 번 원판을 %s 에서 %s 로 이동", 1, from, to))
  } else {
    move_hanoi(n - 1, from, via, to)
    print(sprintf("%d 번 원판을 %s 에서 %s 로 이동", n, from, to))
    move_hanoi(n - 1, via, to, from)
  }
}


move_hanoi(3, "A", "C", "B")
move_hanoi(4, "A", "C", "B")



move_hanoi <- function(n, from, to, via) {
  if (n == 1) {
    print(sprintf("%d 번 원판을 %s 에서 %s 로 이동", 1, from, to))
  } else {
    move_hanoi(n - 1, from, via, to)
    print(sprintf("%d 번 원판을 %s 에서 %s 로 이동", n, from, to))
    move_hanoi(n - 1, via, to, from)
  }
}
