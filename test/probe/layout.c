/*
 * Copyright 2026 guenchi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * THE LAYOUTS ffi.sc READS BY OFFSET, MEASURED BY THE COMPILER. ffi.sc reads
 * struct stat and struct sockaddr_un through byte offsets it holds as
 * constants; they were read from headers on macOS and FreeBSD and never on
 * Linux. This program prints what the platform's own compiler and headers
 * say: each struct's size, and for every field ffi.sc could read its
 * offset and size, with the sizes of the types behind them. The output is
 * one S-expression per line, so a reader can take it as data.
 *
 * NOTE: IT MEASURES, IT DOES NOT CHECK. There is no expected value in this
 * file; the table built from its runs is what the code is then written
 * against.
 */

#define _DEFAULT_SOURCE 1
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <time.h>
#if defined(__GLIBC__)
#include <gnu/libc-version.h>
#endif

#define FIELD(type, member) \
  printf("  (field %s (offset %lu) (size %lu))\n", #member, \
         (unsigned long)offsetof(type, member), \
         (unsigned long)sizeof(((type *)0)->member))

#define TYPE(t) printf("  (type %s (size %lu))\n", #t, (unsigned long)sizeof(t))

/* macOS names the stat times st_*timespec; POSIX 2008 and Linux, st_*tim. */
#if defined(__APPLE__)
#define ATIM st_atimespec
#define MTIM st_mtimespec
#define CTIM st_ctimespec
#else
#define ATIM st_atim
#define MTIM st_mtim
#define CTIM st_ctim
#endif

#define XSTR(x) STR(x)
#define STR(x) #x

int main(void) {
  struct utsname u;
  struct sockaddr_un a;
  const char *sample = "/nonexistent/theourgia-layout-probe/socket";

  printf("(layout-probe 1)\n");
  if (uname(&u) == 0) {
    printf("(machine \"%s\") (system \"%s\") (release \"%s\")\n", u.machine, u.sysname, u.release);
  }
#if defined(__GLIBC__)
  printf("(libc \"glibc %s\")\n", gnu_get_libc_version());
#elif defined(__FreeBSD__)
  printf("(libc \"freebsd %d\")\n", __FreeBSD__);
#elif defined(__APPLE__)
  printf("(libc \"darwin\")\n");
#else
  printf("(libc \"unknown\")\n");
#endif
  printf("(pointer-size %lu)\n", (unsigned long)sizeof(void *));

  printf("(stat (size %lu)\n", (unsigned long)sizeof(struct stat));
  FIELD(struct stat, st_dev);
  FIELD(struct stat, st_ino);
  FIELD(struct stat, st_mode);
  FIELD(struct stat, st_nlink);
  FIELD(struct stat, st_uid);
  FIELD(struct stat, st_gid);
  FIELD(struct stat, st_size);
  FIELD(struct stat, st_blksize);
  FIELD(struct stat, st_blocks);
  printf("  (field st_atim (member %s) (offset %lu) (size %lu))\n", XSTR(ATIM),
         (unsigned long)offsetof(struct stat, ATIM), (unsigned long)sizeof(((struct stat *)0)->ATIM));
  printf("  (field st_mtim (member %s) (offset %lu) (size %lu))\n", XSTR(MTIM),
         (unsigned long)offsetof(struct stat, MTIM), (unsigned long)sizeof(((struct stat *)0)->MTIM));
  printf("  (field st_ctim (member %s) (offset %lu) (size %lu))\n", XSTR(CTIM),
         (unsigned long)offsetof(struct stat, CTIM), (unsigned long)sizeof(((struct stat *)0)->CTIM));
  TYPE(dev_t);
  TYPE(ino_t);
  TYPE(mode_t);
  TYPE(nlink_t);
  TYPE(uid_t);
  TYPE(gid_t);
  TYPE(off_t);
  TYPE(blksize_t);
  TYPE(blkcnt_t);
  TYPE(time_t);
  TYPE(struct timespec);
  printf("  (field-of-timespec tv_nsec (offset %lu) (size %lu)))\n",
         (unsigned long)offsetof(struct timespec, tv_nsec),
         (unsigned long)sizeof(((struct timespec *)0)->tv_nsec));

  memset(&a, 0, sizeof a);
  a.sun_family = AF_UNIX;
  strncpy(a.sun_path, sample, sizeof a.sun_path - 1);
  printf("(sockaddr_un (size %lu)\n", (unsigned long)sizeof(struct sockaddr_un));
#if defined(__APPLE__) || defined(__FreeBSD__)
  FIELD(struct sockaddr_un, sun_len);
#else
  printf("  (field sun_len absent)\n");
#endif
  FIELD(struct sockaddr_un, sun_family);
  FIELD(struct sockaddr_un, sun_path);
  TYPE(sa_family_t);
  printf("  (af-unix %d)\n", (int)AF_UNIX);
  printf("  (sun-len \"%s\" %lu))\n", sample, (unsigned long)SUN_LEN(&a));
  return 0;
}
