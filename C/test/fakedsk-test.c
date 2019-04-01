#if 0
#  define FAKEDSK_INPUT  "input.txt"
#  define FAKEDSK_OUTPUT "output.txt"
#else
#  define FAKEDSK_INPUT  "$INPUT"
#  define FAKEDSK_OUTPUT "$OUTPUT"
#endif

#include "fakedsk.h"
#include <stdio.h>
#include <signal.h>

/*
 *  "Scheletro" per un tipico programma per la scheda DSP.
 *
 */


short  input, output;
double old_output=0.0;

interrupt void 
c_int12() // Chiamata quando e` pronto un campione
{
  input = input_sample();         // Read ADC 
  printf("in=%d\n", input);

  /* ==========> Elabora il campione appena letto... <========== */

  /* Semplice passa basso con H(z)=1/1-0.4*z^-1 */

  old_output = input+old_output*0.4;
  output = old_output;

  output = input*2;
  printf("in=%d, out=%d\n", input, output);
}

interrupt void  
c_int11() 
{
  /* Fai qualcosa... */
  printf("in=%d, out=%d\n", input, output);
  output_sample(output);     	    	   // send output sample to DAC
}

int main()
{
  // struct sigaction act;
  
  comm_intr(); // init DSK, codec, McBSP if DSK present

  //  printf("A, %f\n", FAKEDSK_SLOW);

  while(1) {
    // sigaction(SIGALRM, NULL, &act);    printf("main: %x\n", act.sa_handler);
  }   // Idle loop
}
