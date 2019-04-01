#ifndef PROVA_H__
#define PROVA_H__

#include<math.h>
/*
 * Name of the input file
 */
#ifndef FAKEDSK_INPUT
#  define FAKEDSK_INPUT "input.wav"
#endif

/*
 * Name of the output file
 */
#ifndef FAKEDSK_OUTPUT
#  define FAKEDSK_OUTPUT "output.wav"
#endif

/*
 * Name of the function called when a new input sample is ready
 * from the ADC
 */
#ifndef FAKEDSK_INPUT_CALLBACK
#  define FAKEDSK_INPUT_CALLBACK c_int12
#endif

/*
 * Name of the function called when the DAC is ready for
 * a new output sample
 */
#ifndef FAKEDSK_OUTPUT_CALLBACK
#  define FAKEDSK_OUTPUT_CALLBACK c_int11
#endif

#ifndef FAKEDSK_SAMPLING_FREQ
#  define FAKEDSK_SAMPLING_FREQ 8000 /* in Hz */
#endif 

void fakedsk_comm_intr(char *input_name, 
                       char *output_name, 
                       void (*input_callback)(void),
                       void (*output_callback)(void),
		       double sampling_time);

extern short *fakedsk_input_buffer;
extern short *fakedsk_output_buffer;

#define input_sample() (*fakedsk_input_buffer)
#define output_sample(x) (*fakedsk_output_buffer)=floor((x)+0.5)

#ifndef FAKEDSK_IN_FAKEDSK_C

/* Ignore the interrupt keyword */
#define interrupt 

#ifdef __GNUC__
#   define  mai__uti_lizata  __attribute__((__unused__))
#else
#   define  mai__uti_lizata 
#endif

/*
 * In case you are wondering why I declare the array "rumenta_massima" 
 * ("greatest junk"), I will tell you that without it the SIGALRM handler
 * used in fakedsk.c to interrupt the main program reverts to the
 * default action after the first interrupt. 
 *
 * Obviously, there must be a bug somewhere and I will try to find it;
 * meanwhile...
 */
#define comm_intr() do { \
     int rumenta_massima[100] mai__uti_lizata;\
     fakedsk_comm_intr(FAKEDSK_INPUT,\
                       FAKEDSK_OUTPUT, \
                       FAKEDSK_INPUT_CALLBACK, \
                       FAKEDSK_OUTPUT_CALLBACK,\
                       1.0/FAKEDSK_SAMPLING_FREQ); } while(0)

#endif





#endif
