#ifndef SAMPLEIO_H__
#define SAMPLEIO_H__
#include "waveio.h"

typedef struct {
  unsigned int is_wav;
  unsigned int is_pipe;
  WAV_FILE *wav_handler;
  FILE *handler;
} DATA_FILE;

int open_input_file(DATA_FILE *Input, char *filename);
int open_output_file(DATA_FILE *Output, char *output, int Fc);

int read_new_sample(DATA_FILE Input, short *sample);
void write_new_sample(DATA_FILE Output, short sample);

int read_data_file(char *filename, short **buffer, long int *n_samples);
int write_data_file(char *filename, short *buffer, long int n_samples, int Fc);

#endif
