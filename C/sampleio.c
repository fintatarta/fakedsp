#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "waveio.h"
#include "sampleio.h"


#define WAV_FORMAT 0
#define TXT_FORMAT 1
#define PIPE       2
#define NO_ENV     3


static unsigned int name_to_type(char **s)
{
  int len;
  char *buf = *s;

  /* If the name begins with "$", read the true filename from 
   * the environment
   */
  while (buf[0]=='$') 
    {
      if ((buf = *s = getenv(buf+1))==NULL)
	return NO_ENV;
    }

  /* If the name begins with "|", the input/output file is 
   * a pipe
   */
  if (buf[0]=='|') 
    { 
      *s = buf+1;
      return PIPE; 
    }

  /* Here the filename is "normal".  The file is supposed
   * to be in text format if the filename ends with ".txt" or
   * ".TXT", otherwise is supposed to be a WAV file.  Of course, 
   * if the filename is shorter than 4 char, it cannot have a 
   * 3-letter extension, so it must be a WAV file
   */
  len = strlen(buf);

  if (len < 4) return WAV_FORMAT;
  
  if ((strcmp(buf+len-4, ".txt")==0) || (strcmp(buf+len-4, ".TXT")==0))
    return TXT_FORMAT;
  else
    return WAV_FORMAT;
}

int open_input_file(DATA_FILE *Input, char *input)
{
  switch(name_to_type(&input))
    {
    case NO_ENV:
      return -1;

    case WAV_FORMAT:
      Input->wav_handler = open_input_wav(input);

      if (Input->wav_handler == NULL) { 
	waveio_perror("Could not open '%s' in input", input); 
	return -1;
      }
	

      if ((Input->wav_handler->fmt_info.channels != 1) ||
	  (Input->wav_handler->fmt_info.bit_per_sample != 16))
	{
	  fprintf(stderr, 
		  "File '%s': wrong format (I read 1-channel, 16 bit/samples WAV files\n", input);
	  return -1;
	}

      Input->is_wav = 1;
      Input->is_pipe = 0;
      break;

    case PIPE:
      Input->handler = popen(input, "r");
      if (Input->handler==NULL) 
	{ perror("Could not open input pipe"); return -1; }
      Input->is_wav = 0;
      Input->is_pipe = 1;
      break;

    case TXT_FORMAT:
      Input->handler = fopen(input, "r");
      if (Input->handler==NULL) 
	{ perror("Could not open input"); return -1; }
      Input->is_wav = 0;
      Input->is_pipe = 0;
      break;
    }

  return 0;
}

int open_output_file(DATA_FILE *Output, char *output, int Fc)
{
  switch(name_to_type(&output))
    {
    case NO_ENV:
      return -1;

    case WAV_FORMAT:
      Output->wav_handler = open_output_wav(output, 1, 16, Fc);

      if (Output == NULL) { 
	waveio_perror("Could not open '%s' in output", output); 
	return -1;
      }
      
      Output->is_wav = 1;
      Output->is_pipe = 0;
      break;

    case PIPE:
      Output->handler = popen(output, "w");
      if (Output->handler==NULL) 
	{ perror("Could not open output pipe"); return -1; }
      Output->is_wav = 0;
      Output->is_pipe = 1;
      break;

    case TXT_FORMAT:
      Output->handler = fopen(output, "w");
      if (Output->handler==NULL) 
	{ perror("Could not open output"); return -1; }
      Output->is_wav = 0;
      Output->is_pipe = 0;
      break;
    }

  return 0;
}

/*
 * Read a new sample from Input and store its value into *sample.
 * Return non-zero in case of error, zero otherwise.
 */
int read_new_sample(DATA_FILE Input, short *sample)
{
  if (Input.is_wav)
    { 
      return read_wav_samples(Input.wav_handler, sample, 1); 
    }
  else
    {
      if (fscanf(Input.handler, "%hd", sample) == 1)
	return 0;

      if (! feof(Input.handler))
	{ 
	  fprintf(stderr, "Wrong input file format.\nDo you know that the input values must be 16-bit signed integer?\n");
	}

      return 1;
    }
}

void write_new_sample(DATA_FILE Output, short sample)
{
  if (Output.is_wav)
    write_wav_samples(Output.wav_handler, &sample, 1);    
  else
    fprintf(Output.handler, "%d\n", (int) sample);
}

void close_data_file(DATA_FILE file)
{
  if (file.is_wav)
    close_wav_file(file.wav_handler);
  else 
    {
      if (file.is_pipe)
	pclose(file.handler);
      else
	fclose(file.handler);
    }
}


int read_data_file(char *filename, short **buffer, long int *n_samples)
{
  DATA_FILE Input;
  short sample;
  long int bufdim;
  int letti;
  short *local_buf;


  if (open_input_file(&Input, filename) < 0)
    return -1;

  letti = 0;
  bufdim = 0;
  local_buf = 0;
  while(read_new_sample(Input, &sample) == 0)
    {
      if (letti == bufdim)
	{
	  short *newbuf;

	  bufdim += 1024;

	  if (! (newbuf = realloc(local_buf, sizeof(short)*bufdim)))
	    {	
	      free(local_buf);
	      *buffer = NULL;
	      *n_samples = 0;
	      return -1 ;
	    }

	  local_buf = newbuf;
	}

      local_buf[letti++] = sample;
    }

  close_data_file(Input);

  *buffer = local_buf;
  *n_samples = letti;
  return 0;
}

int write_data_file(char *filename, short *buffer, long int n_samples, int Fc)
{
  long int n;

  DATA_FILE Output;

  if (open_output_file(&Output, filename, Fc) < 0)
    return -1;

  for(n=0; n<n_samples; n++) {
    write_new_sample(Output, buffer[n]);
  }

  close_data_file(Output);
  return 0;
}
