#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "SDL.h"

#define BUF_SIZE 45

struct point {
    double xval, yval;
    int undefined;
};

int isInWindow(int px, int py, int width, int height) {
    return px > 0 && px < width && py > 0 && py < height;
}

int main(int argc, char *argv[]) {
    // check for file name input
    if(argc < 2) {
        printf("%s\n", "Usage: must provide filename");
        return 0;
    }
    
    // prepare file to read
    FILE *infile = fopen(argv[1], "r");
    char buffer[BUF_SIZE];
    if(!infile) {
        printf("cannot open file\n");
        return 0;
    }

    // read in the dimensions
    int nrange = 0, trange = 0;
    if(fgets(buffer, BUF_SIZE, infile)) {
        char *nptr, *tptr;
        nptr = buffer;

        nrange = strtol(nptr, &tptr, 10);
        trange = strtol(tptr, NULL, 10);
    }

    // create an array of size
    struct point func[trange][nrange];
 
    // input file data into array
    for(int i = 0; i < trange; i++) {
        for(int j = 0; j < nrange 
        && (fgets(buffer, BUF_SIZE, infile)); j++) {
            char *xptr, *yptr, *defptr;
            xptr = buffer;
        
            struct point *cur = &func[i][j];
            cur->xval = strtod(xptr, &yptr);
            cur->yval = strtod(yptr, &defptr);
            cur->undefined = strtol(defptr, NULL, 2);
        }
    }
    
    // window values
    int wwidth = 640;
    int wheight = 480;

    // axes values
    int originx = wwidth/2;
    int originy = wheight/2;
    double pxlsclx = 0.03125;
    double pxlscly = 0.04167;

    // synchronization
    long preferredMillisPerDraw = 50;

    // initialize SDL
    if(SDL_Init(SDL_INIT_VIDEO)) {
        fprintf(stderr, "SDL_Init Error: %s\n", SDL_GetError());
        return 0;
    }

    // create the sdl window
    SDL_Window *window = SDL_CreateWindow("Graph",SDL_WINDOWPOS_UNDEFINED,SDL_WINDOWPOS_UNDEFINED,wwidth,wheight,SDL_WINDOW_SHOWN);
    if(!window) {
        fprintf(stderr, "SDL_CreateWindow Error: %s\n", SDL_GetError());
        SDL_Quit();
        return 0;
    }

    // create the sdl renderer
    SDL_Renderer *renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED| SDL_RENDERER_PRESENTVSYNC);
    if (!renderer) {
        SDL_DestroyWindow(window);
        fprintf(stderr, "SDL_CreateRenderer Error: %s\n", SDL_GetError());
        SDL_Quit();
        return 0;
    }

    int running = 1;
    int tidx = 0;
    while (running) {
        // start time
        clock_t startTime = clock();

        // event processing
        SDL_Event event;
        while (SDL_PollEvent(&event)) {
            if (event.type == SDL_QUIT) {
                running = 0;
            }
        }

        // reset t index
        if(tidx == trange) {
            tidx = 0;
        }

        // draw background
        SDL_SetRenderDrawColor(renderer, 255,255,255,255);
        SDL_RenderClear(renderer);

        // draw axes
        SDL_SetRenderDrawColor(renderer, 255,0,0,255);
        SDL_RenderDrawLine(renderer, originx, 0, originx, wheight);
        SDL_RenderDrawLine(renderer, 0, originy, wwidth, originy);

        // draw the curve
        SDL_SetRenderDrawColor(renderer, 0,0,0,255);
        SDL_Point p1, p2;
        struct point cur = func[tidx][0];
        p2.x = originx + (int)(cur.xval/pxlsclx);
        p2.y = originy - (int)(cur.yval/pxlscly);
        for(int nidx = 1; nidx < nrange; nidx++) {
            // move p2 to p1
            p1.x = p2.x;
            p1.y = p2.y;
            int last_undefined = cur.undefined;
 
            cur = func[tidx][nidx];
            if(!cur.undefined) {
                // calculate p2
                p2.x = originx + (int)(cur.xval/pxlsclx);
                p2.y = originy - (int)(cur.yval/pxlscly);

                if(!last_undefined && (isInWindow(p1.x, p1.y, wwidth, wheight) || isInWindow(p2.x, p2.y, wwidth, wheight))) {
                    // draw the line
                    SDL_RenderDrawLine(renderer, p1.x, p1.y, p2.x, p2.y);
                }   
            }
        }

        // present screen
        SDL_RenderPresent(renderer);

        // increment t index
        tidx++;

        // end time
        clock_t endTime = clock();

        // wait
        long elapsedMillis = (double)(endTime-startTime)/CLOCKS_PER_SEC * 1000;
        if(elapsedMillis < preferredMillisPerDraw) {
            SDL_Delay(preferredMillisPerDraw-elapsedMillis);
        }
    }
	
    // release memory
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
}
