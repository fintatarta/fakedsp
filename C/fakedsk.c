#include <sys/types.h>
#include <sys/wait.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <time.h>
#include <math.h>

#define FAKEDSK_IN_FAKEDSK_C

#include "fakedsk.h"
#include "sampleio.h"
#include "semafori.h"

/*
 * Input and output buffers.  They need to be global variables since 
 * they must be accessed by the "main" program (via suitable macros
 * in fakedsk.h)
 */
short *fakedsk_input_buffer;
short *fakedsk_output_buffer;

static long int n_samples;   /* Number of input samples */
static long int buffer_pos;  /* Number of samples used so far */

/*
 * Address to the input/output callback functions defined by 
 * the user.
 */
static void (*inp_callback)(void);
static void (*out_callback)(void);

/*
 * Function fakedsk_interrupt_dispatch() is the handler of signal
 * SIGALRM.  It will called every time the alarm process send a SIGALRM
 * to the main process.  The duties of fakedsk_interrupt_dispatch() are
 * (i) make available the next input sample to the user program and 
 * (ii) call the user interrupt handlers.
 */
static void fakedsk_interrupt_dispatch(int junk)
{
  /*
   * Kill yourself if you run out of input samples. This will release
   * the parent process which is wait for your termination.
   */
  if (buffer_pos == n_samples) 
    { raise(SIGKILL); }


  /*
   * Call the user interrupt handlers
   */
  if (inp_callback) inp_callback();
  if (out_callback) out_callback();

  /* 
   * Increase the pointer to the input/output buffers
   */
  buffer_pos++;
  fakedsk_input_buffer++;
  fakedsk_output_buffer++;
}

/*
 * Sleep for sec seconds. (In case you are wondering, "dormi" 
 * means "sleep").
 */
static void dormi(double sec)
{
  struct timespec delta_t;

  delta_t.tv_sec = floor(sec);
  delta_t.tv_nsec = 1e9*(sec-delta_t.tv_sec);

  if (nanosleep(&delta_t, NULL) < 0)
    { 
      /* It should never happen.  Anyway... */
      fprintf(stderr, "Error in nanosleep: %m, %f\n", sec);  
      exit(1); 
    }
}

/*
 * Function alarm_task() is executed by the alarm process.
 */
static void alarm_task(int main_pid, double sampling_time, int main_ready)
{
  /*
   * Wait for the end of the initialization of the main process
   */
  semaphore_wait(main_ready);

  /*
   * Wake up every sampling_time seconds and send a SIGALRM to 
   * the main process.
   */

  while(1) {
#ifdef FAKEDSK_SLOW
    dormi(FAKEDSK_SLOW);
#else
    dormi(sampling_time);
#endif

    kill(main_pid, SIGALRM);
  }
}

/*
 * Function fakedsk_comm_intr() is called, via a macro in fakedsk.h,
 * when the user calls comm_intr().  Its main duties are
 *
 *  1) Read the input file
 *  2) Allocate the shared memory for the output buffer
 *  3) Create the main and the alarm processes 
 *  4) Wait for the end of the main process and save the
 *     the output buffer to the output file
 *  5) Clean up your own mess and exit.
 */ 
void fakedsk_comm_intr(char *input_name, 
		       char *output_name, 
		       void (*input_callback)(void),
		       void (*output_callback)(void),
		       double sampling_time)
{
  int shared_id; /* ID of the shared memory used as
		  * output buffer
		  */

  int main_ready_sem; /* ID of the semaphore used to
		       * synchronize the main process and the 
		       * alarm process
		       */

  int parent_pid; /* parent process PID  */
  int main_pid;   /* main   process PID  */
  int alarm_pid;  /* alarm  process PID  */

  /*
   * Open the input file and check its format.
   */

  if (read_data_file(input_name, &fakedsk_input_buffer, &n_samples) < 0)
    exit(1);

  buffer_pos = 0;

  inp_callback = input_callback;
  out_callback = output_callback;

  /*
   * Devo fare un giro un po' contorto...
   *
   * Voglio che il programma termini una volta che sono
   * stati letti tutti i campioni in ingresso ed inoltre non voglio
   * scrivere i campioni uno alla volta sul file di uscita
   * dall'interno di un gestore di segnali.  La soluzione e` salvare
   * i dati in vettore temporaneo che scriveremo una volta finiti i
   * campioni da elaborare.
   *
   * Purtroppo, il posto piu` "naturale" in cui fare il controllo e`
   * il gestore di segnali e non so quanto si possa fare I/O "pesanti"
   * dall'interno di un gestore.  Un'alternativa sarebbe fare una
   * exit() nel momento in cui finiamo i campioni e lasciare che sia
   * una funzione registrata con atexit() a fare il lavoro di
   * salvataggio.  Purtroppo non sono certo di come si comporti il
   * processo se chiamo exit() dall'interno di un gestore.
   *
   * La soluzione piu` sicura, anche se parecchio contorta, e` la
   * seguente: il processo si "spiezza in ddue" tramite una fork(), il
   * figlio esegue l'elaborazione, mentre il padre aspetta l'uscita
   * del figlio.  Il buffer di uscita sara` un segmento di
   * memoria condivisa che il padre, una volta che sia terminato il
   * figlio, provvedera` a salvare su disco.
   */

  /*
   * Allocate the shared memory segment to be used as output 
   * buffer.
   */
  shared_id = shmget(IPC_PRIVATE, sizeof(short)*n_samples, 0600);
  fakedsk_output_buffer = shmat(shared_id, NULL, 0);

  /*
   * Create the semaphore used to synchronize the main and the alarm
   * processes.
   */
  main_ready_sem = new_semaphore(0);

  /*
   * Get your own PID
   */
  parent_pid = getpid();

  /*
   * Now create the main and the alarm processes
   */
  if ((main_pid = fork()) == 0)
    {
      /* 
       * I am the main process. First, register the handler
       * for SIGALRM
       */
      struct sigaction action; 


      action.sa_handler = fakedsk_interrupt_dispatch;
      sigaction(SIGALRM, &action, NULL);
      
      /* 
       * Now signal to the alarm process that we are ready
       * and that it can begin its work.
       */

      semaphore_signal(main_ready_sem);

      /*
       * Back to the main() which will be interrupted every 
       * sampling_time seconds by the alarm process.
       */

      return;
    }
  else
    {
      /* If I am here, I am the parent process */

      if ((alarm_pid = fork())==0) 
	{
	  /* 
	   * If I am here, I am the alarm process. 
	   * Jump to your task function which will never 
	   * return.  The alarm process will be killed by the
	   * parent when the main process terminate.
	   */
	  alarm_task(main_pid, sampling_time, main_ready_sem); 
	} 
      else 
	{
	  /*
	   * If I am here, I am the parent process.
	   * Wait for the main process to stop, then save the
	   * results to the output file.
	   */
	  int status;

	  waitpid(main_pid, &status, 0); /* Wait for the main process */

	  /* Check why the main process terminated */
	  if (WIFSIGNALED(status) && WTERMSIG(status) != SIGKILL)
	    { fprintf(stderr, "main killed with %d\n", WTERMSIG(status)); }

	  /*
	   * Write the shared buffer to the output file
	   */
	  write_data_file(output_name, fakedsk_output_buffer, n_samples, 8000);

	  /*
	   * Delete the shared buffer
	   */
	  shmdt(fakedsk_output_buffer);
	  shmctl(shared_id, IPC_RMID, NULL);

	  /*
	   * Delete the semaphore
	   */
	  delete_semaphore(main_ready_sem);

	  /*
	   * Kill the alarm process
	   */
	  kill(alarm_pid, SIGKILL);

	  /* And, finally... exit */
	  exit(0); /* Bye! */
	} /* if (alarm_pid) */
    } /* if (main_pid) */
} /* fakedsk_comm_intr */
