#include <stdio.h>
#include <pthread.h>


int i = 0;

void *task_a(){
  int j = 0;
 
  for(; j < 1000000; j++){
   i++;
  }
} 

void *task_b(){
  int j = 0;

  for(; j < 1000000; j++){
    i--;
  }
}


int main() {

  pthread_t thread1;
  pthread_t thread2;

  pthread_create(&thread1, NULL, task_a, NULL);
  pthread_create(&thread2, NULL, task_b, NULL);

  pthread_join(thread1, NULL);
  pthread_join(thread2, NULL);

  printf("%d\n", i);

  return 0;

}
