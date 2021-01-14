## 2.4 반복문(Looping)

# Prerequisite
#   프로그램 또는 알고리즘 구현 시 특정 문장 또는 표현을 반복해야만 하는 상황이 발생
#   특히 시뮬레이션 시 반복문은 거의 필수적임
#   반복문을 통해 코딩의 효율을 극대화 할 수 있음
#   반복문은 특정 변수의 값을 갱신(update) 하기 위해 주로 사용

x <- x + 1 # 현재 값에 1을 더해서 x를 새로운 값으로 update




#   통상적으로 특정 변수의 값을 갱신하기 위해 변수 값을 초기화(initialize)

x <- 0 # x 변수 초기화
x <- x + 1
#   몇 번 반복이라는 정의가 없는 상태에서 특정 조건이 거짓(FALSE)이 될 때 까지 계속 반복








### 2.4.1 repeat 구문

# repeat 표현식

#   repeat 다음에 오는 표현식을 무한 반복(infinite loop)

repeat print("무한 루프에 걸림 ...ESC 키 누르시오!!")



# 특정 작업에 대해 블록을 지정(중괄화)하고 블록 안에 표현 가능
# 일반적으로 특정 조건( if (조건) break )을 두어 무한 루프에서 탈출
# if 문의 조건은 언제 반복이 끝날 지를 제어하는 변수로 반복변수(iteration variable) 이라고도 함
# 언제까지(until) 반복(repeat) -> REPEAT-UNTIL 구문으로 표현


# repeat {
#   표현식 1
#   if (조건) break
#   반복변수 update
# }



# REPEAT-UNTIL 예시 1
# 1:100 까지 합 계산 함수
tot <- 0; i <- 1 # 사용 변수 초기화 (updat 변수)
repeat {
  tot <- tot + i
  if (i >= 100) break # i는 반복 변수
  i <- i + 1
}
tot

# check
sum(1:100)


# 1. tot에 i를 더한 후 i 가 조건을 만족하는지 확인
# 2. 조건에 부합하지 않으면 다음 문장 실행(i에 1을 증가 후 업데이트) 1.의 작업을 반복(loop)
# 3. i 가 조건에 부합하면 반복 종료


# REPEAT 예시 2

# 1에서 20 사이 숫자 알아맞추기 게임

n <- 20
number <- sample(1:n, size = 1)
cat("1에서 ", n, "까지 숫자 알아 맞추기", sep = "")
repeat {
  guess <- readline("어떤 숫자를 생각하시나요 ? (종료 : q 입력)")
  if (guess == "q") {
    cat("재미가 없나봐요.\n")
    break
  } else if (as.numeric(guess) == number) {
    cat("천재인데요?ㅋㅋㅋ\n")
    break
    
  }
  # 틀리면 계속 반복
}


# 1. guess에 readline() 으로부터 값 입력
# 2. guess 값이 q 이면 종료
# 3. guess 값이 number 와 일치하면 종료
# 4. 2.와 3. 조건에 부합하지 않으면 guess 값을 반복적으로 입력










### 2.4.2 while 구문

# while (조건) 표현식 ...

# while에 지정된 조건이 참이면 계속해서 반복
# repeat는 반복이 처음부터 시작되는 반면, while문은 조건을 먼저 평가한 후 반복이 시작됨.
# while (FALSE)인 경우 루프 본문 코드가 실행되지 않음
# while (TRUE)는 repeat 구문과 동일



# while문 의 일반적 형태

# while (조건) {
#   표현식 1
#   반복변수 update
# }





# WHILE 구문 예시 1
# 1:100 까지 합 계산 함수
tot <- 0 ; i <- 1 # 사용 변수 초기화(update 변수)
while (i <= 100) {
  tot <- tot + i
  i <- i + 1
}
tot

# 1. 초기값 i 가 조건 i <= 100 인지 확인
# 2. 참인 경우 tot + i 를 통해 tot을 업데이트 한 다음 i를 1만큼 증가
# 3. 만약 i에 대한 조건 평가 결과가 거짓이면 while 구문을 빠져나감



# while 문 조건이 TRUE 인 경우
tot <- 0; i <- 1 # 사용 변수 초기화(update 변수)
while (TRUE) {
  tot <- tot + i
  if (i >= 100) break
  i <- i + 1
}
tot
 
# 1. while의 조건이 참이기 때문에 무한 반복
# 2. 단 i가 100과 같거나 클 경우 구문 탈출
# 3. 그 전 까지는 tot와 i를 갱신




# WHILE 구문 예시 2
# 문자열 벡터에서 특정 문자열 인덱스를 반환

txtvec <- c("R", "package", "flow-control", "while", "if", "for", "repeat")
found <- FALSE
i <- 1

word <- readline("검색할 텍스트 : ")
while(!found & i <= length(txtvec)) {
  if (txtvec[i] == word) {
    found <- TRUE
    break
  }
  cat(i, " 번째 위치에 해당 단어가 존재하지 않습니다.\n", sep = "")
  i <- i + 1
} 

if (found){
  cat(i, " 번째 위치에 ", word, "를 찾았습니다.", sep = "")
} else {
  cat(word, " 단어는 해당 문자열 벡터에 존재하지 않습니다.\n", sep = "")
}

 
# 1. found = FALSE, i = 1 을 초기값으로 입력
# 2. readline()으로 입력한 텍스트를 word에 저장
# 3. found가 참이고 i가 텍스트 벡터의 길이 값과 같을 때 까지 다음 구문 반복
# 4. txtvec각 원소와 word값이 같은지 확인











### 2.4.3 for 구문 

# 가장 많이 사용되는 반복구문으로 일반적인 형태는 아래와 같음
 
# for (반복변수 in sequence) {
#   표현식 1
#   ...
# }

# R 에서 sequence은 특정 유형의 벡터이며, 반복변수에 sequence의 원소를 순차적으로 할당함
# 반복변수는 for 반복문 안의 표현식 1 에서 사용됨



# for 문 예시 1
student <- readxl::read_excel("data/stat-students.xlsx")
student_name <- student$이름
for (s in student_name) {
  cat(s, "학생!! 즐거운 명절 보내세요^^\n")
}


# 1. student_name의 첫 번째 원소를 s에 할당
# 2. for 구문 안에 표현 실행
# 3. student_name의 마지막 원소까지 반복


# 위 예시와 동일한 표현

## 인덱싱을 사용
for (i in 1:length(student_name)) {
  cat(student_name[i], "학생!! 즐거운 명절 보내세요^^\n")
}


## sequence를 만드는 함수 seq_along() 사용
for (i in seq_along(student_name)) {
  cat(student_name[i], "학생!! 즐거운 명절 보내세요^^\n")
}







# for 구문 안에 for 문을 1개 이상 중첩 가능
# 보통 R에서 for문 중첩은 계산속도가 매우 느려지므로 2개이상 중첩은 잘 하지않는다.

## 2중 for문 예시
set.seed(12345)
id <- sample(1:length(student_name), 5)
sel_student <- student_name[id]

for (i in seq_along(student_name)) {
  for (j in seq_along(sel_student)) {
    if (student_name[i] == sel_student[j]) {
      cat(sel_student[j], "님!! 당첨 축하 드립니다!!\n")
    }  
  }
}





# 불확정 반복문 학습 시 무한루프로부터 break를 통해 루프에서 탈출
# 루프를 완전히 탈출하지 않고 현재 반복을 중지하고 그 다음 반복을 진행하고 싶을 경우 next 예약어를 사용


# next 예약어 사용 예시
# 알파벳 e와 일치하는 경우에만 텍스트 메세지 출력
vec <- c("c", "e", "e", "i", "o", "u", "e", "z")
word <- "e"
for (i in 1:length(vec)) {
  if (vec[i] != word) next
  cat(word, "가", i, "번 째 인덱스에 있네요!!\n")
}



