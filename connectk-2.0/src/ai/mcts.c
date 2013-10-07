
/*

connectk -- a program to play the connect-k family of games
Copyright (C) 2007 Jeff Deitch

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

*/

#include "config.h"
#include <glib.h>
#include <stdio.h>
#include "../shared.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

AIMove move;
AIMoves *ai_mcts(const Board *b){
        sleep(1);	
	int MY_PIECE_A;
	int MY_PIECE_B;
	if (b->turn==PIECE_BLACK) {
		MY_PIECE_A=PIECE_WHITE;
		MY_PIECE_B=PIECE_BLACK;
	} else if (b->turn==PIECE_WHITE) {
		MY_PIECE_A=PIECE_BLACK;
		MY_PIECE_B=PIECE_WHITE;
	} else {
		int d = 1/0;
	}
	char buf[10*board_size*board_size+100]; // need ~10 chars per element. +10 to remove anomolies for small board sizes
        AIMoves *moves = aimoves_new();
	sprintf(buf, "(%d,[[", b->moves_left);
        for (int x = 0; x < board_size; x++) {
		if (x>0) strcat(buf,"],[");
		for (int y = 0; y < board_size-1; y++) {
			if (piece_at(b,y,x)==PIECE_NONE){strcat(buf,"Nothing,");}
			if (piece_at(b,y,x)==MY_PIECE_A){strcat(buf,"Just B, ");}
			if (piece_at(b,y,x)==MY_PIECE_B){strcat(buf,"Just A, ");}
		}
		if (piece_at(b,board_size-1,x)==PIECE_NONE){strcat(buf,"Nothing");}
		if (piece_at(b,board_size-1,x)==MY_PIECE_A){strcat(buf,"Just B ");}
		if (piece_at(b,board_size-1,x)==MY_PIECE_B){strcat(buf,"Just A ");}
		if (piece_at(b,board_size-1,x)==-1){strcat(buf,"err");}
	}
        strcat(buf, "]])\n");
	
        int fd1 = open("/tmp/pipe1",O_RDWR);
        FILE * fp1 = fdopen (fd1, "w");
        fprintf(fp1,buf);
        fclose(fp1);
        close(fd1);

	int x, y;
	char result[80];
        int fd2 = open("/tmp/pipe2",O_RDWR);
        FILE * fp2 = fdopen (fd2, "r");
        char* result2 = fgets(result,80,fp2);
	sscanf(result2, "%d %d", &y, &x);
        fclose(fp2);
        close(fd2);
        
        printf("RESULTS: %d %d\n",x,y);
        
        
	/*for (int x = 0; x < board_size; x++) {
		for (int y = 0; y < board_size; y++) {
			printf("%d %d %d \n", x, y, piece_at(b,x,y));
			move.weight = (x==xu && y==yu) ?  1 : 0;
			move.x = x;
			move.y = y;
                        aimoves_add(moves, &invmagic[]);
		}
	}*/
	move.x = x;
	move.y = y;
	move.weight = 100;
	aimoves_add(moves, &move);
        moves->utility = 100;
	/* return the array */
	return moves;
}
