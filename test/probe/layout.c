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
 * struct stat, struct sockaddr_un and the other kernel structs at the
 * offsets and widths of the running platform's row in
 * (theourgia platform-numbers), and each row is this program's output on
 * that platform, kept verbatim in readings/. It prints what the
 * platform's own compiler and headers say: each struct's size, and for
 * every field ffi.sc could read its offset and size, with the sizes of the
 * types behind them. The output is S-expressions -- a (struct ...) block
 * per struct, one (constants ...) block -- so a reader can take it as data.
 *
 * AND EVERY PLATFORM NUMBER ffi.sc NAMES: open and fcntl flags, the ioctl
 * and sysconf requests, flock and seek, the file type bits, the socket
 * and signal numbers, the rlimit resources, the errno values, and the
 * sizes of the posix_spawn types, struct dirent, struct timeval and
 * struct rlimit, the sizes of pid_t and int, and the process-size structs
 * ffi.sc reads per system (proc_taskinfo on macOS, kinfo_proc on the BSDs).
 * A constant, struct or field a platform does not have prints as absent.
 *
 * NOTE: IT MEASURES, IT DOES NOT CHECK. There is no expected value in this
 * file; the table built from its runs is what the code is then written
 * against.
 */

#define _DEFAULT_SOURCE 1
#include <stddef.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/utsname.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <spawn.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#if defined(__FreeBSD__)
#include <sys/param.h>
#include <sys/user.h>
#endif
#if defined(__APPLE__) || defined(__FreeBSD__)
#include <sys/sysctl.h>
#endif
#if defined(__APPLE__)
#include <libproc.h>
#include <sys/proc_info.h>
#endif
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
  int exited = 0x0300;
  int signalled = 0x0009;

  printf("(layout-probe 4)\n");
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

  printf("(struct stat (size %lu)\n", (unsigned long)sizeof(struct stat));
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
  printf("(struct sockaddr_un (size %lu)\n", (unsigned long)sizeof(struct sockaddr_un));
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

  printf("(struct dirent (size %lu)\n", (unsigned long)sizeof(struct dirent));
  FIELD(struct dirent, d_ino);
#if defined(_DIRENT_HAVE_D_TYPE) || defined(__APPLE__) || defined(__FreeBSD__)
  FIELD(struct dirent, d_type);
#else
  printf("  (field d_type absent)\n");
#endif
#if defined(__APPLE__) || defined(__FreeBSD__)
  FIELD(struct dirent, d_namlen);
#else
  printf("  (field d_namlen absent)\n");
#endif
  FIELD(struct dirent, d_reclen);
  FIELD(struct dirent, d_name);
  printf("  )\n");

  printf("(struct timeval (size %lu)\n", (unsigned long)sizeof(struct timeval));
  FIELD(struct timeval, tv_sec);
  FIELD(struct timeval, tv_usec);
  printf("  )\n");

  printf("(struct rlimit (size %lu)\n", (unsigned long)sizeof(struct rlimit));
  FIELD(struct rlimit, rlim_cur);
  FIELD(struct rlimit, rlim_max);
  printf("  (RLIM_INFINITY %llu))\n", (unsigned long long)RLIM_INFINITY);

  printf("(types (pid_t (size %lu)) (int (size %lu)))\n", (unsigned long)sizeof(pid_t), (unsigned long)sizeof(int));

  /*
   * The process-size readings ffi.sc makes per system: proc_taskinfo on
   * macOS, kinfo_proc on the BSDs.
   */
#if defined(__APPLE__)
  printf("(struct proc_taskinfo (size %lu)\n", (unsigned long)sizeof(struct proc_taskinfo));
  FIELD(struct proc_taskinfo, pti_virtual_size);
  FIELD(struct proc_taskinfo, pti_resident_size);
  printf("  )\n");
#else
  printf("(struct proc_taskinfo absent)\n");
#endif
#if defined(__APPLE__) || defined(__FreeBSD__)
  printf("(struct kinfo_proc (size %lu)\n", (unsigned long)sizeof(struct kinfo_proc));
#if defined(__FreeBSD__)
  FIELD(struct kinfo_proc, ki_rssize);
#else
  printf("  (field ki_rssize absent)\n");
#endif
  printf("  )\n");
#else
  printf("(struct kinfo_proc absent)\n");
#endif

  printf("(struct posix_spawn (attr-size %lu) (file-actions-size %lu))\n",
         (unsigned long)sizeof(posix_spawnattr_t), (unsigned long)sizeof(posix_spawn_file_actions_t));

  /* Variables, not literals: macOS's wait macros take their argument's address. */
  printf("(wait-status (WEXITSTATUS-0x0300 %d) (WIFEXITED-0x0300 %d) (WTERMSIG-0x0009 %d) (WIFSIGNALED-0x0009 %d))\n",
         WEXITSTATUS(exited), WIFEXITED(exited) ? 1 : 0, WTERMSIG(signalled), WIFSIGNALED(signalled) ? 1 : 0);

  printf("(constants\n");
#ifdef X_OK
  printf("  (X_OK %ld)\n", (long)(X_OK));
#else
  printf("  (X_OK absent)\n");
#endif
#ifdef F_OK
  printf("  (F_OK %ld)\n", (long)(F_OK));
#else
  printf("  (F_OK absent)\n");
#endif
#ifdef PATH_MAX
  printf("  (PATH_MAX %ld)\n", (long)(PATH_MAX));
#else
  printf("  (PATH_MAX absent)\n");
#endif
#ifdef PROC_PIDTASKINFO
  printf("  (PROC_PIDTASKINFO %ld)\n", (long)(PROC_PIDTASKINFO));
#else
  printf("  (PROC_PIDTASKINFO absent)\n");
#endif
#ifdef O_RDONLY
  printf("  (O_RDONLY %ld)\n", (long)(O_RDONLY));
#else
  printf("  (O_RDONLY absent)\n");
#endif
#ifdef O_WRONLY
  printf("  (O_WRONLY %ld)\n", (long)(O_WRONLY));
#else
  printf("  (O_WRONLY absent)\n");
#endif
#ifdef O_RDWR
  printf("  (O_RDWR %ld)\n", (long)(O_RDWR));
#else
  printf("  (O_RDWR absent)\n");
#endif
#ifdef O_CREAT
  printf("  (O_CREAT %ld)\n", (long)(O_CREAT));
#else
  printf("  (O_CREAT absent)\n");
#endif
#ifdef O_TRUNC
  printf("  (O_TRUNC %ld)\n", (long)(O_TRUNC));
#else
  printf("  (O_TRUNC absent)\n");
#endif
#ifdef O_EXCL
  printf("  (O_EXCL %ld)\n", (long)(O_EXCL));
#else
  printf("  (O_EXCL absent)\n");
#endif
#ifdef O_APPEND
  printf("  (O_APPEND %ld)\n", (long)(O_APPEND));
#else
  printf("  (O_APPEND absent)\n");
#endif
#ifdef O_CLOEXEC
  printf("  (O_CLOEXEC %ld)\n", (long)(O_CLOEXEC));
#else
  printf("  (O_CLOEXEC absent)\n");
#endif
#ifdef O_NOFOLLOW
  printf("  (O_NOFOLLOW %ld)\n", (long)(O_NOFOLLOW));
#else
  printf("  (O_NOFOLLOW absent)\n");
#endif
#ifdef O_DIRECTORY
  printf("  (O_DIRECTORY %ld)\n", (long)(O_DIRECTORY));
#else
  printf("  (O_DIRECTORY absent)\n");
#endif
#ifdef O_NONBLOCK
  printf("  (O_NONBLOCK %ld)\n", (long)(O_NONBLOCK));
#else
  printf("  (O_NONBLOCK absent)\n");
#endif
#ifdef FIOCLEX
  printf("  (FIOCLEX %ld)\n", (long)(FIOCLEX));
#else
  printf("  (FIOCLEX absent)\n");
#endif
#ifdef FIONCLEX
  printf("  (FIONCLEX %ld)\n", (long)(FIONCLEX));
#else
  printf("  (FIONCLEX absent)\n");
#endif
#ifdef F_GETFD
  printf("  (F_GETFD %ld)\n", (long)(F_GETFD));
#else
  printf("  (F_GETFD absent)\n");
#endif
#ifdef F_SETFD
  printf("  (F_SETFD %ld)\n", (long)(F_SETFD));
#else
  printf("  (F_SETFD absent)\n");
#endif
#ifdef FD_CLOEXEC
  printf("  (FD_CLOEXEC %ld)\n", (long)(FD_CLOEXEC));
#else
  printf("  (FD_CLOEXEC absent)\n");
#endif
#ifdef F_GETFL
  printf("  (F_GETFL %ld)\n", (long)(F_GETFL));
#else
  printf("  (F_GETFL absent)\n");
#endif
#ifdef F_SETFL
  printf("  (F_SETFL %ld)\n", (long)(F_SETFL));
#else
  printf("  (F_SETFL absent)\n");
#endif
#ifdef F_DUPFD
  printf("  (F_DUPFD %ld)\n", (long)(F_DUPFD));
#else
  printf("  (F_DUPFD absent)\n");
#endif
#ifdef F_FULLFSYNC
  printf("  (F_FULLFSYNC %ld)\n", (long)(F_FULLFSYNC));
#else
  printf("  (F_FULLFSYNC absent)\n");
#endif
#ifdef _SC_NPROCESSORS_ONLN
  printf("  (_SC_NPROCESSORS_ONLN %ld)\n", (long)(_SC_NPROCESSORS_ONLN));
#else
  printf("  (_SC_NPROCESSORS_ONLN absent)\n");
#endif
#ifdef _SC_PAGESIZE
  printf("  (_SC_PAGESIZE %ld)\n", (long)(_SC_PAGESIZE));
#else
  printf("  (_SC_PAGESIZE absent)\n");
#endif
#ifdef _PC_CASE_SENSITIVE
  printf("  (_PC_CASE_SENSITIVE %ld)\n", (long)(_PC_CASE_SENSITIVE));
#else
  printf("  (_PC_CASE_SENSITIVE absent)\n");
#endif
#ifdef LOCK_SH
  printf("  (LOCK_SH %ld)\n", (long)(LOCK_SH));
#else
  printf("  (LOCK_SH absent)\n");
#endif
#ifdef LOCK_EX
  printf("  (LOCK_EX %ld)\n", (long)(LOCK_EX));
#else
  printf("  (LOCK_EX absent)\n");
#endif
#ifdef LOCK_NB
  printf("  (LOCK_NB %ld)\n", (long)(LOCK_NB));
#else
  printf("  (LOCK_NB absent)\n");
#endif
#ifdef LOCK_UN
  printf("  (LOCK_UN %ld)\n", (long)(LOCK_UN));
#else
  printf("  (LOCK_UN absent)\n");
#endif
#ifdef SEEK_SET
  printf("  (SEEK_SET %ld)\n", (long)(SEEK_SET));
#else
  printf("  (SEEK_SET absent)\n");
#endif
#ifdef SEEK_CUR
  printf("  (SEEK_CUR %ld)\n", (long)(SEEK_CUR));
#else
  printf("  (SEEK_CUR absent)\n");
#endif
#ifdef SEEK_END
  printf("  (SEEK_END %ld)\n", (long)(SEEK_END));
#else
  printf("  (SEEK_END absent)\n");
#endif
#ifdef S_IFMT
  printf("  (S_IFMT %ld)\n", (long)(S_IFMT));
#else
  printf("  (S_IFMT absent)\n");
#endif
#ifdef S_IFDIR
  printf("  (S_IFDIR %ld)\n", (long)(S_IFDIR));
#else
  printf("  (S_IFDIR absent)\n");
#endif
#ifdef S_IFREG
  printf("  (S_IFREG %ld)\n", (long)(S_IFREG));
#else
  printf("  (S_IFREG absent)\n");
#endif
#ifdef S_IFLNK
  printf("  (S_IFLNK %ld)\n", (long)(S_IFLNK));
#else
  printf("  (S_IFLNK absent)\n");
#endif
#ifdef S_IFSOCK
  printf("  (S_IFSOCK %ld)\n", (long)(S_IFSOCK));
#else
  printf("  (S_IFSOCK absent)\n");
#endif
#ifdef AF_UNIX
  printf("  (AF_UNIX %ld)\n", (long)(AF_UNIX));
#else
  printf("  (AF_UNIX absent)\n");
#endif
#ifdef SOCK_STREAM
  printf("  (SOCK_STREAM %ld)\n", (long)(SOCK_STREAM));
#else
  printf("  (SOCK_STREAM absent)\n");
#endif
#ifdef SOL_SOCKET
  printf("  (SOL_SOCKET %ld)\n", (long)(SOL_SOCKET));
#else
  printf("  (SOL_SOCKET absent)\n");
#endif
#ifdef SO_RCVTIMEO
  printf("  (SO_RCVTIMEO %ld)\n", (long)(SO_RCVTIMEO));
#else
  printf("  (SO_RCVTIMEO absent)\n");
#endif
#ifdef SO_SNDTIMEO
  printf("  (SO_SNDTIMEO %ld)\n", (long)(SO_SNDTIMEO));
#else
  printf("  (SO_SNDTIMEO absent)\n");
#endif
#ifdef SO_REUSEADDR
  printf("  (SO_REUSEADDR %ld)\n", (long)(SO_REUSEADDR));
#else
  printf("  (SO_REUSEADDR absent)\n");
#endif
#ifdef SIGTERM
  printf("  (SIGTERM %ld)\n", (long)(SIGTERM));
#else
  printf("  (SIGTERM absent)\n");
#endif
#ifdef SIGKILL
  printf("  (SIGKILL %ld)\n", (long)(SIGKILL));
#else
  printf("  (SIGKILL absent)\n");
#endif
#ifdef SIGINT
  printf("  (SIGINT %ld)\n", (long)(SIGINT));
#else
  printf("  (SIGINT absent)\n");
#endif
#ifdef SIGHUP
  printf("  (SIGHUP %ld)\n", (long)(SIGHUP));
#else
  printf("  (SIGHUP absent)\n");
#endif
#ifdef SIGCHLD
  printf("  (SIGCHLD %ld)\n", (long)(SIGCHLD));
#else
  printf("  (SIGCHLD absent)\n");
#endif
#ifdef SIGPIPE
  printf("  (SIGPIPE %ld)\n", (long)(SIGPIPE));
#else
  printf("  (SIGPIPE absent)\n");
#endif
#ifdef WNOHANG
  printf("  (WNOHANG %ld)\n", (long)(WNOHANG));
#else
  printf("  (WNOHANG absent)\n");
#endif
#ifdef RLIMIT_CPU
  printf("  (RLIMIT_CPU %ld)\n", (long)(RLIMIT_CPU));
#else
  printf("  (RLIMIT_CPU absent)\n");
#endif
#ifdef RLIMIT_AS
  printf("  (RLIMIT_AS %ld)\n", (long)(RLIMIT_AS));
#else
  printf("  (RLIMIT_AS absent)\n");
#endif
#ifdef POSIX_SPAWN_SETSID
  printf("  (POSIX_SPAWN_SETSID %ld)\n", (long)(POSIX_SPAWN_SETSID));
#else
  printf("  (POSIX_SPAWN_SETSID absent)\n");
#endif
#ifdef POSIX_SPAWN_CLOEXEC_DEFAULT
  printf("  (POSIX_SPAWN_CLOEXEC_DEFAULT %ld)\n", (long)(POSIX_SPAWN_CLOEXEC_DEFAULT));
#else
  printf("  (POSIX_SPAWN_CLOEXEC_DEFAULT absent)\n");
#endif
#ifdef POSIX_SPAWN_SETPGROUP
  printf("  (POSIX_SPAWN_SETPGROUP %ld)\n", (long)(POSIX_SPAWN_SETPGROUP));
#else
  printf("  (POSIX_SPAWN_SETPGROUP absent)\n");
#endif
#ifdef CTL_KERN
  printf("  (CTL_KERN %ld)\n", (long)(CTL_KERN));
#else
  printf("  (CTL_KERN absent)\n");
#endif
#ifdef KERN_PROC
  printf("  (KERN_PROC %ld)\n", (long)(KERN_PROC));
#else
  printf("  (KERN_PROC absent)\n");
#endif
#ifdef KERN_PROC_PID
  printf("  (KERN_PROC_PID %ld)\n", (long)(KERN_PROC_PID));
#else
  printf("  (KERN_PROC_PID absent)\n");
#endif
#ifdef EPERM
  printf("  (EPERM %ld)\n", (long)(EPERM));
#else
  printf("  (EPERM absent)\n");
#endif
#ifdef ENOENT
  printf("  (ENOENT %ld)\n", (long)(ENOENT));
#else
  printf("  (ENOENT absent)\n");
#endif
#ifdef ESRCH
  printf("  (ESRCH %ld)\n", (long)(ESRCH));
#else
  printf("  (ESRCH absent)\n");
#endif
#ifdef EINTR
  printf("  (EINTR %ld)\n", (long)(EINTR));
#else
  printf("  (EINTR absent)\n");
#endif
#ifdef EIO
  printf("  (EIO %ld)\n", (long)(EIO));
#else
  printf("  (EIO absent)\n");
#endif
#ifdef EBADF
  printf("  (EBADF %ld)\n", (long)(EBADF));
#else
  printf("  (EBADF absent)\n");
#endif
#ifdef ECHILD
  printf("  (ECHILD %ld)\n", (long)(ECHILD));
#else
  printf("  (ECHILD absent)\n");
#endif
#ifdef EAGAIN
  printf("  (EAGAIN %ld)\n", (long)(EAGAIN));
#else
  printf("  (EAGAIN absent)\n");
#endif
#ifdef EWOULDBLOCK
  printf("  (EWOULDBLOCK %ld)\n", (long)(EWOULDBLOCK));
#else
  printf("  (EWOULDBLOCK absent)\n");
#endif
#ifdef EACCES
  printf("  (EACCES %ld)\n", (long)(EACCES));
#else
  printf("  (EACCES absent)\n");
#endif
#ifdef EEXIST
  printf("  (EEXIST %ld)\n", (long)(EEXIST));
#else
  printf("  (EEXIST absent)\n");
#endif
#ifdef ENOTDIR
  printf("  (ENOTDIR %ld)\n", (long)(ENOTDIR));
#else
  printf("  (ENOTDIR absent)\n");
#endif
#ifdef EISDIR
  printf("  (EISDIR %ld)\n", (long)(EISDIR));
#else
  printf("  (EISDIR absent)\n");
#endif
#ifdef EINVAL
  printf("  (EINVAL %ld)\n", (long)(EINVAL));
#else
  printf("  (EINVAL absent)\n");
#endif
#ifdef EMFILE
  printf("  (EMFILE %ld)\n", (long)(EMFILE));
#else
  printf("  (EMFILE absent)\n");
#endif
#ifdef ENOTTY
  printf("  (ENOTTY %ld)\n", (long)(ENOTTY));
#else
  printf("  (ENOTTY absent)\n");
#endif
#ifdef ENOSPC
  printf("  (ENOSPC %ld)\n", (long)(ENOSPC));
#else
  printf("  (ENOSPC absent)\n");
#endif
#ifdef EROFS
  printf("  (EROFS %ld)\n", (long)(EROFS));
#else
  printf("  (EROFS absent)\n");
#endif
#ifdef EPIPE
  printf("  (EPIPE %ld)\n", (long)(EPIPE));
#else
  printf("  (EPIPE absent)\n");
#endif
#ifdef ENAMETOOLONG
  printf("  (ENAMETOOLONG %ld)\n", (long)(ENAMETOOLONG));
#else
  printf("  (ENAMETOOLONG absent)\n");
#endif
#ifdef ENOTEMPTY
  printf("  (ENOTEMPTY %ld)\n", (long)(ENOTEMPTY));
#else
  printf("  (ENOTEMPTY absent)\n");
#endif
#ifdef ELOOP
  printf("  (ELOOP %ld)\n", (long)(ELOOP));
#else
  printf("  (ELOOP absent)\n");
#endif
#ifdef EOVERFLOW
  printf("  (EOVERFLOW %ld)\n", (long)(EOVERFLOW));
#else
  printf("  (EOVERFLOW absent)\n");
#endif
#ifdef ETIMEDOUT
  printf("  (ETIMEDOUT %ld)\n", (long)(ETIMEDOUT));
#else
  printf("  (ETIMEDOUT absent)\n");
#endif
#ifdef ECONNREFUSED
  printf("  (ECONNREFUSED %ld)\n", (long)(ECONNREFUSED));
#else
  printf("  (ECONNREFUSED absent)\n");
#endif
#ifdef ENOTSOCK
  printf("  (ENOTSOCK %ld)\n", (long)(ENOTSOCK));
#else
  printf("  (ENOTSOCK absent)\n");
#endif
  printf("  )\n");
  return 0;
}
