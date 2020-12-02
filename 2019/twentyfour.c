#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int adjacent(char *grid, int i)
{
     int n = 0;

     if (i % 5 != 0)
         n += grid[i-1] & 1;

     if (i % 5 != 4)
         n += grid[i+1] & 1;

     if (i % 25 > 4)
         n += grid[i-5] & 1;

     if (i % 25 < 20)
         n += grid[i+5] & 1;

     return n;
}

void prngrid(char *grid)
{
    for (int i=0; i<5; i++) {
        for (int j=0; j<5; j++)
            printf("%d ", grid[i*5 + j]);
        printf("\n");
    }
}

void gameoflife(char *grid)
{
    int a;
    for (int i=0; i<25; i++) {
        a = adjacent(grid, i);
        if (a == 1 || (a == 2 && (grid[i] & 1) == 0))
            grid[i] |= 2;
        else
            grid[i] &= 1;
    }

    for (int i=0; i<25; i++) {
        grid[i] >>= 1;
    }
}

int main()
{
    char grid[] = {
        0, 0, 0, 0, 1,
        1, 0, 0, 1, 0,
        1, 0, 0, 1, 1,
        0, 0, 1, 0, 0,
        1, 0, 0, 0, 0
    };

    for (int i=0;; i++) {
        gameoflife(grid);
        if (i % 10000 == 0)
            prngrid(grid);
    }
}
