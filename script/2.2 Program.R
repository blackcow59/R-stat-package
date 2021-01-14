## 2.2 Program

# 프로그램(program) : 특정 작업(목적)을 수행할 수 있도록 작성한 일련의 R 문장(명령어)의 집합
  # 일련의 문장(명령어)들은 텍스트 편집기를 통해 작성하며, 스크립트(script)로 명칭되는 파일로 저장 -> R 스크립티 .R확장자를 가짐



# Hellow.R
print("안녕 R!!") # 한국어
print("HI R!!") # 영어
print("こんにちはR!!") # 일본어
print("Γεια R!!") #그리스어


# 예시 : 텍스트 파일에서 가장 자주 나오는 단어 찾기 프로그램

require(tidyverse)
require(stringr)
require(ggpubr)
require(ggthemes)

text_dat <- readLines("data/text-example-01.txt.txt")

# 공백 또는 구둣점 문자를 기준으로 텍스트 나누기

split_wd <- str_split(text_dat, pattern = "\\b|[[:punct:]]")
split_wd <- do.call(c, split_wd)
id <- grepl("[a-zA-Z]+", split_wd) # 알파벳을 포함한 단어 인덱스
split_wd <- split_wd[id]
unique_wd <- unique(split_wd) # 중복을 제외한 총 사용 단어
res_v <- vector("integer", length(unique_wd)) # 저장 벡터 생성

for (i in seq_along(unique_wd)) {
  for(j in seq_along(split_wd)) {
    if (unique_wd[i] == split_wd[j]) {
      res_v[i] <- res_v[i] + 1
    }
  }
}

bind_cols("word" = unique_wd, "freq" = res_v) %>% 
  arrange(desc(freq))



### 프로그램 작성을 위한 개념적 요소
  # 입력(input) : 외부로부터 가져온 데이터, 값 등
  # 출력(output) : 입력에 대한 반응 (결과 출력, 파일 저장, 음악 재생, ...)
  # 순차실행(sequential execution) : 스크립트 또는 코드 작성 순서에 따라 한줄씩 실행
  # 조건실행(conditional execution) : 특정 조건에 따라 문장(명령)을 실행하거나 건너뜀
  # 반복실행(iterative execution) : 특정 명령을 반복적으로 실행
  # 재사용(resuse) : 스크립트의 집합(다수 줄로 구성된 코드 또는 스크립트)에 이름을 부여하고 저장 -> 사용자 지정 함수(function)


### 프로그램 오류의 종류
  # 구문오류(syntax error) : R 언어가 이해할 수 없는 문장 또는 문법으로 실행했을 때 나타나는 오류 -> 가장 고치기 쉽고 즉각적으로 알려줌
  # 논리 또는 run-time 오류(logic or runtime error) : 구문은 완벽하지만 실행 순서 또는 논리적으로 연관방식에 문제가 있어서 명령러를 수행할 수 없는 경우
  # 의미론적 오류(sementic error) : 프로그램은 구문적으로 오류가 없고 실행되지만 올바른 결과를 출력하지 않는 경우 -> 제일 고치기 어려움

### 가장 간단한 프로그래밍은 순차적으로 명령을 실행하되 입력 시 흐름을 잠시 중단하고 대기하는 방법 -> 프롬프트 상 명령어 한 줄씩 입력



# 아주 간단한 프로그래밍 예제
# readline() 함수 이용해 R한테 인사 받기
name <- readline("What's your name?: ")
cat("Hellow, ", name, "!/n", sep = "")

# readline() 함수를 이용해 알바비 계산
x <- as.numeric(readline(prompt = "하루 아르바이트 시간을 입력하시오: "))
y <- as.numeric(readline(prompt = "시급을 입력하시오 (단위=원): "))
z <- as.numeric(readline(prompt = "한달 동안 총 몇 일 동안 일을 하셨나요?: "))
cat("월 급여는 ", x * y * z, "원 입니다.\n", sep = "")
