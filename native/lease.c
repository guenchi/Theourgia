#include <node_api.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>

/* Stable lock names are never removed. Closing the descriptor releases the lock. */
static napi_value acquire(napi_env env, napi_callback_info info) {
  size_t argc=1,length=0;
  napi_value argv[1],answer;
  napi_get_cb_info(env,info,&argc,argv,NULL,NULL);
  if(argc!=1 || napi_get_value_string_utf8(env,argv[0],NULL,0,&length)!=napi_ok) {
    napi_throw_type_error(env,NULL,"Expected a lock path");return NULL;
  }
  char *name=malloc(length+1);
  if(!name){napi_throw_error(env,"RESOURCE_UNAVAILABLE","Cannot allocate lock path");return NULL;}
  napi_get_value_string_utf8(env,argv[0],name,length+1,&length);
  int fd=open(name,O_RDWR|O_CREAT|O_CLOEXEC|O_NOFOLLOW,0600);
  free(name);
  if(fd<0){napi_throw_error(env,"RESOURCE_UNAVAILABLE","Cannot open the stable lock file");return NULL;}
  struct stat status;
  if(fstat(fd,&status)<0 || !S_ISREG(status.st_mode) || status.st_nlink!=1){
    close(fd);napi_throw_error(env,"RESOURCE_UNAVAILABLE","Lock path is not a private regular file");return NULL;
  }
  if(flock(fd,LOCK_EX|LOCK_NB)<0){
    int reason=errno;close(fd);
    napi_throw_error(env,(reason==EWOULDBLOCK||reason==EAGAIN)?"RESOURCE_BUSY":"RESOURCE_UNAVAILABLE",
      "Another process holds this resource, or its lock cannot be verified");return NULL;
  }
  napi_create_int32(env,fd,&answer);return answer;
}
static napi_value release(napi_env env,napi_callback_info info){
  size_t argc=1;int32_t fd=-1;napi_value argv[1],answer;
  napi_get_cb_info(env,info,&argc,argv,NULL,NULL);
  if(argc!=1 || napi_get_value_int32(env,argv[0],&fd)!=napi_ok || fd<0){napi_throw_type_error(env,NULL,"Expected a lock descriptor");return NULL;}
  close(fd);napi_get_undefined(env,&answer);return answer;
}
static napi_value init(napi_env env,napi_value exports){
  napi_property_descriptor properties[]={{"acquire",NULL,acquire,NULL,NULL,NULL,napi_default,NULL},{"release",NULL,release,NULL,NULL,NULL,napi_default,NULL}};
  napi_define_properties(env,exports,2,properties);return exports;
}
NAPI_MODULE(NODE_GYP_MODULE_NAME,init)
