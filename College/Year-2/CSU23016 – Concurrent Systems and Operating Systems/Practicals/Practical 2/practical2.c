
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "cond.c"

//Initialise mutex 
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
//Initialise condition variable
pthread_cond_t cond = PTHREAD_COND_INITIALIZER;

int bool_prod_run = 1;
int pnum;  // number updated when producer runs.
int csum;  // sum computed using pnum when consumer runs.

int (*pred)(int); // predicate indicating if pnum is to be consumed

int produceT() {
  scanf("%d",&pnum); // read a number from stdin
  return pnum;
}

void *Produce(void *a) {
  //Variable to keep track of whether the thread should continue running
  int p;
  p = 1;
  // Keep producing until p is set to 0
  while (p) {
    // Acquire lock on mutex
    pthread_mutex_lock(&mutex);
    // Print message to indicate that producer has acquired lock
    printf("P-LOCK\n");
    // Print message to indicate that producer is ready to produce (i.e. ready to execute produceT() function)
    printf("@P-READY\n");
    // Call produceT() function to generate new item to be consumed
    p = produceT();
    // Print message to indicate that a new item has been produced
    printf("@PRODUCED %d\n", p);
    // Signal the consumer thread to start consuming
    pthread_cond_signal(&cond);
    // Print message to indicate that producer has sent signal to consumer
    printf("P-SIGNAL\n");
    // Set boolean variable to indicate that producer has produced item
    bool_prod_run = 0;
    // Wait until consumer thread has consumed the item (i.e. completed its execution)
    while (bool_prod_run == 0) {
      // Wait for the consumer thread to signal the condition variable letting the producer know that it has consumed the item (i.e. completed its execution)
      pthread_cond_wait(&cond, &mutex);
    }
    // Release lock on mutex
    pthread_mutex_unlock(&mutex);
  }
  // Print message to indicate that producer thread is exiting
  printf("@P-EXIT\n");
  // Exit thread
  pthread_exit(NULL);
}


int consumeT() {
  if ( pred(pnum) ) { csum += pnum; }
  return pnum;
}

void *Consume(void *a) {
  //Variable to keep track of whether the thread should continue running
  int p; 
  p=1;
  // keep Consuming until p is set to 0
  while (p) { 
    // Acquire lock on mutex to prevent other threads from accessing the critical section
    pthread_mutex_lock(&mutex); 
    // Print message to indicate that consumer has acquired lock
    printf("C-LOCK\n"); 
    // Wait until producer thread has produced an item (i.e. completed its execution)
    while(bool_prod_run == 1) { 
      // Wait for the producer thread to signal the condition variable letting the consumer know that it has produced an item (i.e. completed its execution)
      pthread_cond_wait(&cond, &mutex); 
    }
    // Print message to indicate that consumer has received signal from producer
    printf("C-SIGNAL\n");
    // Print message to indicate that consumer is ready to consume (i.e. ready to execute consumeT() function)
    printf("@C-READY\n");
    // Call consumeT() function to consume the item produced by the producer thread
    p = consumeT(); 
    // Print message to indicate that consumer has consumed the item and updated the sum 
    printf("@CONSUMED %d\n",csum); 
    // Signal the producer thread to start producing 
    pthread_cond_signal(&cond); 
    // Set boolean variable to indicate that consumer has consumed item 
    bool_prod_run = 1; 
    // Release lock on mutex 
    pthread_mutex_unlock(&mutex);
  }
  // Print message to indicate that consumer thread is exiting
  printf("@C-EXIT\n"); 
  // Exit thread
  pthread_exit(NULL); 
}



int main (int argc, const char * argv[]) {
  // the current number predicate
  static pthread_t prod,cons;
	long rc;

  pred = &cond1;
  if (argc>1) {
    if      (!strncmp(argv[1],"2",10)) { pred = &cond2; }
    else if (!strncmp(argv[1],"3",10)) { pred = &cond3; }
  }


  pnum = 999;
  csum=0;
  
  printf("@P-CREATE\n");
 	rc = pthread_create(&prod,NULL,Produce,(void *)0);
	if (rc) {
			printf("@P-ERROR %ld\n",rc);
			exit(-1);
		}
  printf("@C-CREATE\n");
 	rc = pthread_create(&cons,NULL,Consume,(void *)0);
	if (rc) {
			printf("@C-ERROR %ld\n",rc);
			exit(-1);
		}

  printf("@P-JOIN\n");
  pthread_join( prod, NULL);
  printf("@C-JOIN\n");
  pthread_join( cons, NULL);


  printf("@CSUM=%d.\n",csum);

  return 0;
}
