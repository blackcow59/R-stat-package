### 4.2 연속형 변수의 비교




## 4.2.1 대응 표본 t 검정(paired samples t test)

# 대응 표본(paired sample)

# 동일한 대상자에 대해 쌍으로 이루어진 데이터
  # 새 교습방법에 대한 적용 전,후 수학 점수
  # 실험용 쥐에 실험약 투여 전,후 대사량
  # 다이어트 약 복용 전,후 체중


# 혈압강하제 임상시험데이터에서 시험약 복용 전(DBP1) 대비 복용 1개월 후 (DBP2)이완기 혈압(diastolic blood pressure)의 차이 검정

DBP <- read_delim("data/DBP.txt", delim = "\t")

DBP <- DBP %>% 
  mutate(D1 = DBP2 - DBP1)
treatment <- DBP %>% 
  filter(TRT == "A")

D1 <- treatment$D1
mD1 <- mean(D1)
sD1 <- sd(D1)

### check 
mb <- mean(treatment$DBP1); ma <- mean(treatment$DBP2)
sD1_check <- with(treatment,
                  sqrt(var(DBP1) + var(DBP2) -
                         2*cor(DBP1, DBP2)*sd(DBP1)*sd(DBP2)))
mD1; ma - mb
sD1; sD1_check

tstat <- mD1/(sD1/sqrt(length(D1)))
p.val <- 2*(pt(tstat, df = length(D1) - 1))

## t.test() check
tstat; t.test(D1)$statistic

p.val; t.test(D1)$p.value





# R에서 대응표본(일표본) 및 독립 이표본 t 검정을 하기 위한 함수는 t.test()이고 아래와 같은 인수를 가짐

# x: 검정을 할 데이터
# y: 독립 이표본 검정 수행 시 두 번째 데이터
  # 독립 이표본 t 검정 시 수식 형태 y ~ x로 표현 가능하며,   이 때 입력 데이터는 데이터 프레임 형태임
# alternative: 대립가설 형태(양측 또는 단측: “two.sided”, “greater”, “less”)
# var.equal: 논리값을 가지며 독립 이표본 t 검정 시 두 집단의 분산에 대한 가정(FALSE = 이분산, TRUE=등분산)
# conf.level: 신뢰수준(default 값은 0.95)

# R은 다양한 가설검정 관련 함수를 제공하는데 검정 결과는 htest라는 클래스 개체에 저장함. htest 객체는 다음과 같은 출력결과를 가짐(검정함수에 따라 달라질 수 있음)
 
# statistic: 검정 통계량
# parametmer: 검정통계량 계산에 사용된 자유도(degree of freedom)
# p.value: P 값
# conf.int: 신뢰구간
# estimate: 추정 평균 또는 평균 차이 (t 검정인 경우)


