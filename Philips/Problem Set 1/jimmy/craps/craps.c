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

int main(int argc, char *argv[])
{
	int i, seed;
	char arg0[] = "./shooter";
	char arg1[10];
	char *args[] = {arg0, arg1, NULL};
	pid_t pIDs[NUM_PLAYERS];
	int seedPipes[NUM_PLAYERS][2];
	int scorePipes[NUM_PLAYERS][2];
	int score[NUM_PLAYERS];
	
	for (i = 0; i < NUM_PLAYERS; i++) {
		score[i] = 0;
		if(pipe(seedPipes[i]))
		{
			perror("Failed to create seed pipes.");
			exit(EXIT_FAILURE);
		}
		if(pipe(scorePipes[i]))
		{
			perror("Failed to create score pipes.\n");
			exit(EXIT_FAILURE);
		}
	}

	for (i = 0; i < NUM_PLAYERS; i++) {
	 pIDs[i] = fork();
		if(pIDs[i] == 0)
		{
		 sprintf(arg1, "%d", i);
			close(seedPipes[i][1]);
			close(scorePipes[i][0]);
			if(dup2(seedPipes[i][0], STDIN_FILENO) == -1)
				perror("Failed to rerout stdin.\n");
			if(dup2(scorePipes[i][1], STDOUT_FILENO) == -1)
				perror("Failed to rerout stdout.\n");
//	shooter(i, seedPips[i][0], scorePipes[i][1]);
			fprintf(stderr, "Executing thread %d\n", i+1);
			execv(arg0, args);
		}
		else
		{
			close(seedPipes[i][0]);
			close(scorePipes[i][1]);
		}
	}
	printf("%d %d\n", dStdin, dStdout);
	seed = time(NULL);
	for (i = 0; i < NUM_PLAYERS; i++) {
		seed++;
		if(write(seedPipes[i][1], &seed, sizeof(int)) <= 0)
		{
			perror("Failed to write seed to pipe.");
			exit(EXIT_FAILURE);
		}
	}
	int high = 0;
	for (i = 0; i < NUM_PLAYERS; i++) {
	 while(read(scorePipes[i][0], &score[i], sizeof(int)) < 0)
		{
		}
		printf("Player %d's score is: %d\n", i, score[i]);
		if(score[i] > high)
			winner = i;
	}
	printf("master: player %d WINS\n", winner);
	kill(pIDs[winner], SIGUSR1);

	for (i = 0; i < NUM_PLAYERS; i++) {
	 kill(pIDs[i], SIGUSR2);
	}

	printf("master: the game ends\n");

	for (i = 0; i < NUM_PLAYERS; i++) {
		waitpid(pIDs[i], 0, 0);
		close(seedPipes[i][1]);
		close(scorePipes[i][0]);
	}
	return 0;
}
