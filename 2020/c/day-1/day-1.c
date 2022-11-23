#import <stdio.h>
#import <string.h>

int main(int argc, char *argv[]) {

  int *nums = NULL;
  size_t nums_size = 0;

  /* open file */
  FILE *fp = fopen(argv[1], "r");
  if (!fp) perror("open");
  
  /* parse lines */
  char* line = NULL;
  size_t linecap = 0;
  ssize_t linelen = 0;
  while ((linelen = getline(&line, &linecap, fp)) != -1) {
    memcpy(&nums[nums_size], line, linelen);
    nums_size++;
  }
  fclose(fp);
  printf("%d",nums[1]);
}
