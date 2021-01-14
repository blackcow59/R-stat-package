### 2.1 Prerequisite

# 예약어(researved wods) : R에서 의미(sementic)를 미리 정해 놓은 단어

# if eles, while, function, in, next, break : 조건, 함수, 반복문에 사용
# TRUE/FALSE : 논리 상수(logical constants)
# NULL : 정의되지 않은 값 혹은 값이 없음 표현
# inf : 무한(infinity)
# NaN : 숫자가 아님(not a number)
# NA : 결측값(not available)
# NA_interger_, NA_real, NA_complex_, NA_character_ : 결측값을 처리하는 상수
# ... : 함수가 다른 함수에 인자를 전달하도록 지원



## 변수(variable) : 사용자가 프로그램 처리를 위해 지정한 단어
  # 적당한 값을 저장하고 나중에 필요시 해당 값을 호출해 사용하기 위한 목적으로 사용되는 표식(label)
  # 예약어를 변수명으로 사용할 수 없음

## 고수준 언어(high-level language) : 사람이 읽고 쓰기 쉬운 형태의 명령어를 컴퓨터가 읽고 처리 할 수 있도록 고안된 프로그래밍 언어
  # 컴퓨터가 이해 할 수 있는 언어 -> 중앙처리장치(central processing unit, CPU)가 이해하는 언어 -> 기계어(machine language)
  # 기계어는 0과 1로 구성된 이진수(binary number)임(예 : 0100101001001001001110110101101010110)
  # 고수준 언어의 종류 : C, C++, JAVA, 베이직, Perl, Python, R, ..

## 번역기(translator) : 사람이 이해할 수 있는 표현(언어)를 기계(컴퓨터)가 이해 할 수 있는 언어(기계어)로 변환
  # 인터프리터(interpreter)
  # 컴파일러(compiler)

## I**인터프리터* : 코드(스크립트) 한 줄을 즉석에서 읽고, 파싱(프로그램을 검사하고 구문론적 구조를 분석)하고, 해석
  # R, Python, MATLAB 등은 인터프리터를 번역기로 사용
  # 인터엑티브 모드 -> R 프롬프트(>)뒤에 한 줄의 명령어를 작성하면 측석해서 처리 후 다음 입력에 대해 준비(prompt)함

안녕하세요!!
통계패키지활용 수업에서 R을 배우고 있습니다.
처음이라 실수가 많습니다.
앞으로 잘 부탁해요!!

print("안녕하세요!!")
print("통계패키지활용 수업을 위해 R을 배우고 있습니다.")
print("처음이라 실수가 많습니다.")
print("앞으로 잘 부탁해요!!")



# 컴파일러 : 왼전한 프로그램을 하나의 파일에 담고 파일 안에 저장되어 있는 소스코드를 기계어로 번역 후 다음 실행할 수 있도록 변환한 기계어를 파일에 담음
  # 보통은 .exe, .dll파일 형태로 저장됨
