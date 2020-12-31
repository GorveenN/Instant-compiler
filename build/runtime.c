#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void printInt(int x)
{
	printf("%d\n", x);
}

void printString(char *string)
{
	printf("%s\n", string);
}

char *__str_concat(char *s1, char *s2)
{
	size_t s1_len = strlen(s1);
	size_t s2_len = strlen(s2);
	char *new = (char *)malloc(s1_len + s2_len + 1);
	if (new == NULL)
	{
		exit(1);
	}
	memcpy(new, s1, s1_len);
	memcpy(new + s1_len, s2, s2_len + 1);
	return new;
}

char *readString()
{
	int ch;
	size_t size = 256;
	size_t len = 0;
	char *str = malloc(sizeof(char) * size); //size is start size
	if (!str)
	{
		exit(1);
	}

	while (EOF != (ch = fgetc(stdin)) && ch != '\n')
	{
		str[len++] = ch;
		if (len == size)
		{
			str = realloc(str, sizeof(char) * (size += 16));
			if (!str)
			{
				exit(1);
			}
		}
	}
	str[len++] = '\0';

	return realloc(str, sizeof(char) * len);
}

int readInt()
{
	return 1;
}
