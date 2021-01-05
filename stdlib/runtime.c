#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void *__malloc(size_t size) {
  void *ptr = malloc(size);
  if (!ptr) {
    exit(0);
  } else {
    return ptr;
  }
}

void printInt(int x) { printf("%d\n", x); }

void printString(char *string) { printf("%s\n", string); }

char *__concat_string(char *s1, char *s2) {
  size_t s1_len = strlen(s1);
  size_t s2_len = strlen(s2);
  char *new = (char *)malloc(s1_len + s2_len + 1);
  if (new == NULL) {
    exit(1);
  }
  memcpy(new, s1, s1_len);
  memcpy(new + s1_len, s2, s2_len + 1);
  return new;
}

char *readString() {
  int ch;
  size_t size = 256;
  size_t len = 0;
  char *str = __malloc(sizeof(char) * size); // size is start size

  while (EOF != (ch = fgetc(stdin)) && ch != '\n') {
    str[len++] = ch;
    if (len == size) {
      str = realloc(str, sizeof(char) * (size += 16));
      if (!str) {
        exit(1);
      }
    }
  }
  str[len++] = '\0';

  return realloc(str, sizeof(char) * len);
}

int readInt() {
  char *input = readString();
  int read_value = atoi(input);
  free(input);
  return read_value;
}

void __memset(int *ptr, int value, size_t num) {
  for (size_t i = 0; i < num / 4; ++i) {
    ptr[i] = value;
  }
}
