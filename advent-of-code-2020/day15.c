#include <time.h>
#include <stdint.h>
#include <stdio.h>

#define N 7

int run(int* numbers, int limit) {
  int vec[30000000];
  int i;
  int turn = 1;

  for (i = 0; i < limit; i++) { vec[i] = 0l; }
  for (i = 0; i < (N-1); i++) { vec[numbers[i]] = turn++; }

  int last = numbers[N-1];
  int prev_turn;

  while (turn < limit) {
    prev_turn = vec[last];
    vec[last] = turn;
    last = (prev_turn > 0) ? turn - prev_turn : 0;
    turn += 1;
  }

  return last;
}

int get_millis() {
  struct timespec t;
  clock_gettime(CLOCK_REALTIME, &t);
  return t.tv_sec * INT64_C(1000) + t.tv_nsec / 1000000;
}

int main() {
  int t1 = get_millis();
  int nums[] = {12,20,0,6,1,17,7};
  printf("Answer = %d. Millis = %d\n", run(nums, 30000000), get_millis() - t1);
  return 0;
}
