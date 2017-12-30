/*
 * @file    rsa.c
 * @author  작성자 이재현 / 2013043337
 * @date    2017.11.20
 * @brief   mini RSA implementation code
 * @details 세부 설명
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "miniRSA.h"

uint p, q, e, d, n;

/*
 * @brief     모듈러 덧셈 연산을 하는 함수.
 * @param     uint a     : 피연산자1.
 * @param     uint b     : 피연산자2.
 * @param     byte op    : +, - 연산자.
 * @param     uint n      : 모듈러 값.
 * @return    uint result : 피연산자의 덧셈에 대한 모듈러 연산 값. (a op b) mod n
 * @todo      모듈러 값과 오버플로우 상황을 고려하여 작성한다.
 */
uint ModAdd(uint a, uint b, byte op, uint n) {
  uint result, temp;
  if(op == '+'){
    if((a + b) < n){
      result = (a + b);
    }
    else if((a + b) <= a || (a + b) <= b){
      result = a - (n - b);
      while(temp >= n){
        temp -= n;
      }
    } //오버플로우 방지
    else{
      result = a + b - n;
    }
  }

  else if(op == '-'){
    if(a < b){
      if((b - a) < n){
        result = (b - a);
      }
      else if((b - a) > n){
        temp = b - a;
        while(temp >= n){
          temp -= n;
        }
        result = temp;
      }
    }
    else if(a == b){
      result = 0;
    }
    else{
      if((a - b) < n){
        result = (a - b);
      }
      else if((a - b) > n){
        temp = a - b;
        while(temp >= n){
          temp -= n;
        }
        result = temp;
      }
    }
  }

  else{
    perror("Invalid op");
    exit(1);
  }
  return result;
}

/*
 * @brief      모듈러 곱셈 연산을 하는 함수.
 * @param      uint x       : 피연산자1.
 * @param      uint y       : 피연산자2.
 * @param      uint n       : 모듈러 값.
 * @return     uint result  : 피연산자의 곱셈에 대한 모듈러 연산 값. (a x b) mod n
 * @todo       모듈러 값과 오버플로우 상황을 고려하여 작성한다.
 */
uint ModMul(uint x, uint y, uint n) {
  uint temp, temp2;
	uint history;
	uint result = 0;
	uint count = 1;

	while(x >= 1){
    temp = x&1;
    if(temp == 1){
      if(count == 1){
	     	while(y >= n){
          y -= n;
        }
        result = y;
      }
      else{
        history = y;
        temp2 = count;
        while(temp2 != 1){
          if(history + history < history){
            history = history - (n - history);
          }
          else if(history + history <= n){
            history = history + history;
          }
          else{
            history = ModAdd(history, history, '+', n);
          }
          temp2 >>= 1;
        }
        if(result + history <= result){
          result = result - (n - history);
        }
        else if(result + history <= n){
          result += history;
        }
        else{
          result = ModAdd(result, history, '+', n);
        }
      } //오버플로우 방지.
    }
    x >>= 1;
		count <<= 1;
	}
	return result;
}

/*
 * @brief      모듈러 거듭제곱 연산을 하는 함수.
 * @param      uint base   : 피연산자1.
 * @param      uint exp    : 피연산자2.
 * @param      uint n      : 모듈러 값.
 * @return     uint result : 피연산자의 연산에 대한 모듈러 연산 값. (base ^ exp) mod n
 * @todo       모듈러 값과 오버플로우 상황을 고려하여 작성한다.
               'square and multiply' 알고리즘을 사용하여 작성한다.
 */
uint ModPow(uint base, uint exp, uint n) {
  uint result = 1;
  uint temp = base;
  for(; ; exp >>= 1){
    if(exp <= 0){
      while(result >= n){
        result -= n;
      }
      return result;
    }
    if((exp & 1) == 1){
      result = ModMul(result, temp, n);
    }
    temp = ModMul(temp, temp, n);
  }
}

/*
 * @brief      입력된 수가 소수인지 입력된 횟수만큼 반복하여 검증하는 함수.
 * @param      uint testNum   : 임의 생성된 홀수.
 * @param      uint repeat    : 판단함수의 반복횟수.
 * @return     uint result    : 판단 결과에 따른 TRUE, FALSE 값.
 * @todo       Miller-Rabin 소수 판별법과 같은 확률적인 방법을 사용하여,
               이론적으로 4N(99.99%) 이상 되는 값을 선택하도록 한다.
 */
bool IsPrime(uint testNum, uint repeat) {
  uint result = TRUE, randNum, temp;
  uint n = testNum-1;
  int i, j, s = 0;

  while((n&1) == 0){
    n >>= 1;
    s++;
  }

  for(i=repeat; i>0; i--){
    randNum = (uint)((double)WELLRNG512a())*testNum + 1;
    if(randNum < 2){
      randNum = 2;
    }

    if(GCD(randNum, testNum) != 1){
      result = FALSE;
    }

    temp = ModPow(randNum, n, testNum);

    if(temp == 1 || temp == (testNum-1)){
      continue;
    }
    else{
      for(j=0; j<s-1; j++){
        temp = ModMul(temp, temp, testNum);
        if(temp == (testNum-1)){
          break;
        }
      }
      if(temp != (testNum-1)){
        result = FALSE;
      }
    }
  }
  return result;
}
/*
 * @brief       모듈러 역 값을 계산하는 함수.
 * @param       uint a      : 피연산자1.
 * @param       uint m      : 모듈러 값.
 * @return      uint result : 피연산자의 모듈러 역수 값.
 * @todo        확장 유클리드 알고리즘을 사용하여 작성하도록 한다.
 */
uint ModInv(uint a, uint m) {
  uint x, x1, x2;
	uint q;
	uint t, t1 = 0;
	uint temp, temp2, qTemp;
	int result = 1;

	for (x1 = a, x2 = m; x1 != 1; x2 = x1, x1 = x){
		temp = x2;
		temp2 = x1;
		qTemp = 0;
		while(temp >= temp2){
			qTemp++;
			temp -= temp2;
		}
		q = qTemp;
		x = x2 - x1 * q;
		t = t1 - result * q;
		t1 = (uint)result;
		result = t;
	}

	if(result < 0){
    result += m;
  }
	return (uint)result;
}

/*
 * @brief     RSA 키를 생성하는 함수.
 * @param     uint *p   : 소수 p.
 * @param     uint *q   : 소수 q.
 * @param     uint *e   : 공개키 값.
 * @param     uint *d   : 개인키 값.
 * @param     uint *n   : 모듈러 n 값.
 * @return    void
 * @todo      과제 안내 문서의 제한사항을 참고하여 작성한다.
 */
void miniRSAKeygen(uint *p, uint *q, uint *e, uint *d, uint *n) {
  while(1){
    while(1){
      *p = (uint)((double)WELLRNG512a() * (65536 - 46340 + 1)) + 46340;
      if(IsPrime(*p, 10)){
        printf("%u may be prime.\n\n", *p);
        break;
      }
      printf("%u is not prime.\n", *p);
    }

    while(1){
      *q = (uint)((double)WELLRNG512a() * (65536 - 46340 + 1)) + 46340;
      if(IsPrime(*q, 10)){
        printf("%u may be prime.\n\n", *q);
        break;
      }
      printf("%u is not prime.\n", *q);
    }

    *n = (*p) * (*q);

    if(*n < 2147483648 || *n > 4294967295){
      continue;
    }

    uint n_p = ((*p)-1) * ((*q)-1);

    while(1){
      *e = (uint)((double)WELLRNG512a() * n_p) + 2;
      if(GCD(*e, n_p) == 1){
        printf("===============Selection: p = %u, q = %u, so n = %u===============\n\n", *p, *q, *n);
        printf("===========================e is selected as %u==========================\n\n", *e);

        *d = ModInv(*e, n_p);
        printf("===================e is %u, so d must be %u.====================\n\n", *e, *d);

        break;
      }
    }

    break;

    printf("=======================n out of range=======================\n\n");
  }
}

/*
 * @brief     RSA 암복호화를 진행하는 함수.
 * @param     uint data   : 키 값.
 * @param     uint key    : 키 값.
 * @param     uint n      : 모듈러 n 값.
 * @return    uint result : 암복호화에 결과값
 * @todo      과제 안내 문서의 제한사항을 참고하여 작성한다.
 */
uint miniRSA(uint data, uint key, uint n) {
  uint result = ModPow(data, key, n);
  return result;
}

uint GCD(uint a, uint b) {
    uint prev_a;

    while(b != 0) {
        printf("GCD(%u, %u)\n", a, b);
        prev_a = a;
        a = b;
        while(prev_a >= b) prev_a -= b;
        b = prev_a;
    }
    printf("GCD(%u, %u)\n\n", a, b);
    return a;
}

int main(int argc, char* argv[]) {
    byte plain_text[4] = {0x12, 0x34, 0x56, 0x78};
    uint plain_data, encrpyted_data, decrpyted_data;
    uint seed = time(NULL);

    memcpy(&plain_data, plain_text, 4);

    // 난수 생성기 시드값 설정
    seed = time(NULL);
    InitWELLRNG512a(&seed);

    // RSA 키 생성
    miniRSAKeygen(&p, &q, &e, &d, &n);
    printf("0. Key generation is Success!\n ");
    printf("p : %u\n q : %u\n e : %u\n d : %u\n N : %u\n\n", p, q, e, d, n);

    // RSA 암호화 테스트
    encrpyted_data = miniRSA(plain_data, e, n);
    printf("1. plain text : %u\n", plain_data);
    printf("2. encrypted plain text : %u\n\n", encrpyted_data);

    // RSA 복호화 테스트
    decrpyted_data = miniRSA(encrpyted_data, d, n);
    printf("3. cipher text : %u\n", encrpyted_data);
    printf("4. Decrypted plain text : %u\n\n", decrpyted_data);

    // 결과 출력
    printf("RSA Decryption: %s\n", (decrpyted_data == plain_data) ? "SUCCESS!" : "FAILURE!");

    return 0;
}
