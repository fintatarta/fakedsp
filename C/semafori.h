#ifndef SEMAFORI_H__
#define SEMAFORI_H__

/*
 * This file is a little "layer" over the usual POSIX interface to
 * semaphores.  Its goal is to make a little simpler to use 
 * binary sempahores.
 */

/** Create a new semaphore and return its ID.  Initialize it 
 *  with value init_value
 */
int new_semaphore(int init_value);
void semaphore_wait(int id);
void semaphore_signal(int id);
void delete_semaphore(int id);

#endif
