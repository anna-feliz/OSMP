/**
 * Game of luck: Implementation of the Gamemaster
 *
 * Course: Operating Systems and Multicore Programming - OSM lab
 * assignment 1: game of luck.
 *
 * Author: Nikos Nikoleris <nikos.nikoleris@it.uu.se>
 *
 */

#include <stdio.h> /* I/O functions: printf() ... */
#include <stdlib.h> /* rand(), srand() */
#include <unistd.h> /* read(), write() calls */
#include <assert.h> /* assert() */
#include <time.h>   /* time() */
#include <signal.h> /* kill(), raise() and SIG???? */

#include <sys/types.h> /* pid */
#include <sys/wait.h> /* waitpid() */

#include "common.h"


#define EXIT_FAILURE 1
#define READ 0
#define WRITE 1
#define SCORE 1
#define SEED 0

int main(int argc, char *argv[])
{
	int i, seed;
	pid_t pid[NUM_PLAYERS];
	int pfd [NUM_PLAYERS][2][2];
        /* TODO: Use the following variables in the exec system call. Using the
	 * function sprintf and the arg1 variable you can pass the id parameter
	 * to the children
	 */

	char arg0[] = "./shooter";
	char arg1[10];
	char *args[] = {arg0, arg1, NULL};

	/* TODO: initialize the communication with the players */
	for (i = 0; i < NUM_PLAYERS; i++) {
	  // skapa pipes för alla spelare
	  pipe(pfd[i][SEED]);
	  pipe(pfd[i][SCORE]);
	}
	
	for (i = 0; i < NUM_PLAYERS; i++) {
	  /* TODO: spawn the processes that simulate the players */
	  // forka alla spelare och se till att alla anropar varsin gång av funktionen shooter
	  
	  for (i = 0; i < NUM_PLAYERS; i++) {
	    pid[i]=fork();
	    
	    if (pid[i] < 0) {
	      // inget barn skapades
	      perror("A child was not created, will now exit...");
	      exit(EXIT_FAILURE);
	    }
	    
	    if(pid[i] == 0) {
	      // barn skapats, gör dessa operationer för varje barn
	      // ....FRÅGA.... Varför skicka med bara i iställer för pid[i] till funktionen shooter?

	      close(pfd[i][SEED][WRITE]); // Stänga file descriptors som inte används
	      close(pfd[i][SCORE][READ]); // Stänga file descriptors som inte används

	      //shooter(i,(pfd[i][SEED][READ]),(pfd[i][SCORE][WRITE]));

	      dup2(pfd[i][SCORE][WRITE], STDOUT_FILENO);
	      dup2(pfd[i][SEED][READ], STDIN_FILENO);


	      sprintf(arg1, "%d", i); //används för att kunna få ut ID:t av processen
	      execv(arg0, args); 

	    } else {
	      // föräldern, gör dessa operationer

	      close(pfd[i][SEED][READ]);
	      close(pfd[i][SCORE][WRITE]);
	    }
	  }
	}

	  seed = time(NULL);
	  for (i = 0; i < NUM_PLAYERS; i++) {
	    seed++;
	    /* TODO: send the seed to the players */
	    //	    printf("Player %d get number %d\n", i, seed);
	    write(pfd[i][SEED][WRITE], &seed, sizeof(int));
	  }
	  
	  /* TODO: get the dice results from the players, find the winner */
	  
	  int highscore = 0;
	  int result = -1;
	  
	  int winner = -1;
	  
	  for (i = 0; i < NUM_PLAYERS; i++) {
	    while (result < 0) {
	      read(pfd[i][SCORE][READ], &result, sizeof(int));
	    }
	    //	    printf("**PLAYER %d ** gets the number: %d\n\n", i, result);
	    
	    if (result > highscore) {
	      winner = i;
	      highscore = result;
	    }
	    result = -1;
	  }
	  
	printf("\n\nmaster: player %d WINS\n", winner);

	/* TODO: signal the winner */
	kill(pid[winner], SIGUSR1);
	
	/* TODO: signal all players the end of game */
	for (i = 0; i < NUM_PLAYERS; i++) {
	  kill(pid[i], SIGUSR2);
	}

	printf("master: the game ends\n\n\n");

	/* TODO: cleanup resources and exit with success */
	for (i = 0; i < NUM_PLAYERS; i++) {
	  waitpid(pid[i], 0, 0);
	}

	return 0;
}
