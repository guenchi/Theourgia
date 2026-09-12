#include <stdio.h>
#include <fcntl.h>
#include <sys/file.h>
#include <unistd.h>
#include <errno.h>
int main(void){
  printf("O_RDONLY %d\nO_WRONLY %d\nO_RDWR %d\nO_CREAT %d\nO_EXCL %d\nO_TRUNC %d\nO_APPEND %d\n",
    O_RDONLY,O_WRONLY,O_RDWR,O_CREAT,O_EXCL,O_TRUNC,O_APPEND);
  printf("LOCK_SH %d\nLOCK_EX %d\nLOCK_NB %d\nLOCK_UN %d\n",LOCK_SH,LOCK_EX,LOCK_NB,LOCK_UN);
  printf("SEEK_SET %d\nSEEK_CUR %d\nSEEK_END %d\n",SEEK_SET,SEEK_CUR,SEEK_END);
  printf("EINTR %d\nEAGAIN %d\nEWOULDBLOCK %d\nEIO %d\n",EINTR,EAGAIN,EWOULDBLOCK,EIO);
  printf("sizeof_off_t %zu\n", sizeof(off_t));
  return 0;
}
