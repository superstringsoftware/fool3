#include <stdio.h>
#include <stdlib.h>

#define HEAP_SIZE 256*1024*1024

int main(void)
{
  char *heap;

  heap = (char *) malloc(HEAP_SIZE);

  if (heap == NULL) {
    printf("Heap allocation failed\n");
    exit(-1);
  }

  printf("Hello, World: %i\n", HEAP_SIZE);
  printf("Size of char: %lu", sizeof(char));
  free(heap);

}
