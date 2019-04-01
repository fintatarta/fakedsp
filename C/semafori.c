#include <sys/types.h>
#include <sys/wait.h>
#include <sys/ipc.h>
#include <sys/sem.h>

/*
 * This file is a little "layer" over the usual POSIX interface to
 * semaphores.  Its goal is to make a little simpler to use 
 * binary sempahores.
 */

#if defined(__GNU_LIBRARY__) && !defined(_SEM_SEMUN_UNDEFINED)
/* union semun is defined by including <sys/sem.h> */
#else
/* according to X/OPEN we have to define it ourselves */
union semun {
  int val;                  /* value for SETVAL */
  struct semid_ds *buf;     /* buffer for IPC_STAT, IPC_SET */
  unsigned short *array;    /* array for GETALL, SETALL */
  /* Linux specific part: */
  struct seminfo *__buf;    /* buffer for IPC_INFO */
};
#endif

#include "semafori.h"

int new_semaphore(int init_value)
{
  union semun semctl_arg;
  int id;

  id = semget(IPC_PRIVATE, 1, 0);

  semctl_arg.val=init_value;
  semctl(id, 0, SETVAL, semctl_arg);

  return id;
}


void semaphore_wait(int id)
{
  struct sembuf semop_arg;

  semop_arg.sem_num = 0;
  semop_arg.sem_op = -1;
  semop_arg.sem_flg = 0;
  semop(id, &semop_arg, 1);
}

void semaphore_signal(int id)
{
  struct sembuf semop_arg;

  semop_arg.sem_num = 0;
  semop_arg.sem_op =  1;
  semop_arg.sem_flg = 0;
  semop(id, &semop_arg, 1);
}


void delete_semaphore(int id)
{
  semctl(id, 0, IPC_RMID);
}



