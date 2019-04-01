/*
 * Copyright 2004 Riccardo Bernardini
 *
 * This file is part of WaveIO.
 *
 * WaveIO is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>

#include "waveio.h"

#ifdef VARARG_PERROR
#  include <stdarg.h>
#endif


int waveio_errno=WAVEIO_OK; /**< Waveio error code */

char *waveio_errmsg[] = {
  "No Error",
  "End-of-file while reading",
  "Too many opened WAV files",
  "Invalid WAV file",
  "WAV file does not use PCM coding"};


void waveio_perror(char *fmt, ...)
{
  int maxerr = sizeof(waveio_errmsg)/sizeof(char *);


  if (-waveio_errno >= maxerr || -waveio_errno < 0)
    return;
  else
    {
#ifdef VARARG_PERROR
      va_list ap;

      /* First of all, print the user message */
      va_start(ap, fmt);
      vfprintf(stderr, fmt, ap);
      va_end(ap);
#else 
      fprintf(stderr, "%s", fmt);
#endif

      if (waveio_errno==0)
	fprintf(stderr, ": %s\n", strerror(errno));
      else
	fprintf(stderr, ": %s\n", waveio_errmsg[-waveio_errno]);
    }
}

typedef struct {
  char riff[4];           /* String "RIFF" (chunk type) */
  int32 len_minus_8;      /* Chunk size (i.e. file length) */
  char format[4];         /* String "WAVE" (RIFF type) */
} riff_header;

typedef struct {
  /* Begin DATA chunk */
  char data[4];           /* String "DATA" */
  int32 bytes_in_data;    /* Chunk length (in byte) */
} data_header;

// Package for file read/write

#if 0
static FILE *fopen_or_die(char *filename, char *mode)
{
  FILE *Stream;

  Stream=fopen(filename, mode);

  if (Stream != NULL)   
    return Stream;
  else
    {
      char buf[1024];
      char *fmt="Could not open %s in %s";

      sprintf(buf, fmt, filename, (mode[0]=='r') ? "input" : "output");
      perror(buf);
      exit(1);
    }
}
#endif

/*
 * Although closing every open file before exiting is a good habit, 
 * programmers are just human being and we would like to be able to 
 * close any still open WAV file when the program exits.  Note that we 
 * cannot rely on the behaviour of fopen() since, if we opened some
 * file in output, we need to update the size fields in the "data" 
 * and "riff" chunks.
 *
 * Because of this, we need to track every file we opened in order to 
 * be able to close it at the end.  The "cleaning" 
 * function (close_still_open()) will be called via atexit().
 */

/*
 * Table of still open files.  If still_open[n] != NULL, still_open[n]
 * is a WAV file which needs to be closed.  For the sake of simplicity,
 * I decided to use a "static", quite large, table.
 */
#define MAX_OPEN_WAV 128
WAV_FILE *still_open[MAX_OPEN_WAV];

int still_open_init = 0;  /* Has still_open[] been initialized? */

/*
 * Close any file which is still open.  Called via atexit() function.
 */
static void close_still_open()
{
  int n;
  for(n=0; n < MAX_OPEN_WAV; n++)
    {
      if (still_open[n] != NULL)
	{
	  close_wav_file(still_open[n]);
	}
    }
}

/*
 * Store the pointer to a newly opened file in the still_open[] table.
 * Return 0 if everything is ok, -1 if there is no more room in 
 * still_open[]
 */
static int store_in_still_open(WAV_FILE *f)
{
  int n;

  if (still_open_init == 0)
    {
      for (n=0; n<MAX_OPEN_WAV; n++)
	still_open[n]=NULL;

      still_open_init = 1;

      atexit(close_still_open);
    }

  for(n=0; n<MAX_OPEN_WAV; n++)
    {
      if (still_open[n]==NULL)
	{
	  f->idx = n;
	  still_open[n] = f;
	  return 0;
	}
    }

  return -1;
}

/*
 * --- PUBLIC SECTION ---
 */

/** Open a WAV file in input.  It returns a pointer to the newly opened 
 * file if everything is OK, otherwise returns NULL.
 */
WAV_FILE *open_input_wav(char *filename)
{
  riff_header junk;
  data_header data_h;
  WAV_FILE *ret=NULL;

  /*
   * Suppose everything is ok.
   */
  waveio_errno = 0;

  /*
   * Allocate the WAV_FILE structure.
   */
  ret = (WAV_FILE*) malloc(sizeof(WAV_FILE));
  if (ret == NULL)
    goto bad;

  ret->idx = -1;

  /*
   * Open the WAV file and check the RIFF header.
   */
  ret->stream = fopen(filename, "rb");
  if (ret->stream == NULL)
    goto bad;

  ret->mode = 'r';

  if (store_in_still_open(ret) < 0)
    {
      waveio_errno = WAVEIO_OUT_OF_ROOM;
      goto bad;
    }

  fread(&junk, sizeof(riff_header), 1, ret->stream);
  if (strncmp(junk.riff, "RIFF", 4) != 0 ||
      strncmp(junk.format, "WAVE", 4) != 0)
    {
      waveio_errno = WAVEIO_INV;
      goto bad;
    }

  /*
   * Get the next chunk and check if it is a "FMT" one
   */
  fread(&ret->fmt_info, sizeof(wav_fmt), 1, ret->stream);

  if (strncmp(ret->fmt_info.fmt, "fmt ", 4) != 0 ||
      ret->fmt_info.chunk_length <  16)  /* Chunk length */
    {
      waveio_errno = WAVEIO_INV;
      goto bad;
    }

  /*
   * Check for PCM format
   */
  if (ret->fmt_info.format_type  != 1)     /* PCM */
    {
      waveio_errno = WAVEIO_NO_PCM;
      goto bad;
    }

  /*
   * Skip to the end of this chunk.
   */
  if (ret->fmt_info.chunk_length > 16)
    fseek(ret->stream, ret->fmt_info.chunk_length-16, SEEK_CUR);

  fread(&data_h, sizeof(data_h), 1, ret->stream);

  if (strncmp(data_h.data, "data", 4) != 0)
    {
      waveio_errno = WAVEIO_INV;
      goto bad;
    }

  ret->nsamples = data_h.bytes_in_data/ret->fmt_info.byte_by_capture;

 good: 
#ifdef __GNUC__
__attribute__((unused))
#endif
  return ret;

 bad:
  if (ret != NULL)
    {
      if (ret->idx >= 0)
	still_open[ret->idx] = NULL;

      if (ret->stream != NULL)
	fclose(ret->stream);

      free(ret);
    }
  return NULL;
}

WAV_FILE *open_output_wav(char *filename, 
			  int nchan,         /* N. of channels */
			  int nbits,         /* bit/samples (8 or 16) */
			  int freq)          /* sampling frequency */
{
  riff_header riff_h;
  data_header data_h;

  WAV_FILE *ret=NULL;

  /*
   * Suppose everything is ok.
   */
  waveio_errno = 0;

  /*
   * Allocate the WAV_FILE structure.
   */
  ret = (WAV_FILE*) malloc(sizeof(WAV_FILE));
  if (ret == NULL)
    goto bad;

  ret->idx = -1;

  /*
   * Open the WAV file and write the RIFF header.
   */
  ret->stream = fopen(filename, "wb");
  if (ret->stream == NULL)
    goto bad;

  ret->mode = 'w';

  if (store_in_still_open(ret) < 0)
    {
      waveio_errno = WAVEIO_OUT_OF_ROOM;
      goto bad;
    }

  strncpy(riff_h.riff, "RIFF", 4);
  riff_h.len_minus_8 = 0;              /* Wrote at closing time */
  strncpy(riff_h.format, "WAVE", 4);

  /*
   *  Write the FMT chunk
   */
  fwrite(&riff_h, sizeof(riff_h), 1, ret->stream);

  strncpy(ret->fmt_info.fmt, "fmt ", 4);
  ret->fmt_info.chunk_length = 16;
  ret->fmt_info.format_type  = 1;
  ret->fmt_info.channels = nchan;
  ret->fmt_info.sampling_freq = freq;
  ret->fmt_info.bit_per_sample = nbits;

  ret->fmt_info.byte_by_capture = nbits*nchan/8;
  ret->fmt_info.byte_per_sec = ret->fmt_info.byte_by_capture*freq;

  fwrite(&ret->fmt_info, sizeof(wav_fmt), 1, ret->stream);

  /*
   *  Now write the beginning of the DATA chunk
   */ 

  strcpy(data_h.data, "data");
  data_h.bytes_in_data=0;               /* Wrote at closing time */

  fwrite(&data_h, sizeof(data_h), 1, ret->stream);

 ok:
#ifdef __GNUC__
__attribute__((unused))
#endif
  return ret;

 bad:
  if (ret != NULL)
    {
      if (ret->idx >= 0)
	still_open[ret->idx] = NULL;

      if (ret->stream != NULL)
	fclose(ret->stream);

      free(ret);
    }
  return NULL;
}

int read_wav_samples(WAV_FILE *Input, void *buf, int how_many)
{
  int n;

  waveio_errno = WAVEIO_OK;

  n=fread(buf, 
	  Input->fmt_info.bit_per_sample/8,
	  Input->fmt_info.channels*how_many, 
	  Input->stream);

  if (n == Input->fmt_info.channels*how_many)
    return 0;

  if (feof(Input->stream))
    waveio_errno = WAVEIO_EOF;

  return -1;
}

int write_wav_samples(WAV_FILE *Output, void *buf, int how_many)
{
  int n;

  waveio_errno = WAVEIO_OK;

  n=fwrite(buf, 
	   Output->fmt_info.bit_per_sample/8,
	   Output->fmt_info.channels*how_many, 
	   Output->stream);

  if (n == Output->fmt_info.channels*how_many)
    return 0;
  else
    return -1;
}

static int eight_to_16(unsigned char c)
{
  return ((int) c)-128;
}

wav_samples *slurp_wav_file(char *filename)
{
  WAV_FILE *Input;
  wav_samples *ret;
  int how_many;

  ret = (wav_samples*) malloc(sizeof(wav_samples));
  if (ret==NULL)
    return NULL;

  Input = open_input_wav(filename);
  if (Input == NULL)
    return NULL;


  how_many = ret->nsamples = Input->nsamples;
  ret->bit_per_sample = Input->fmt_info.bit_per_sample;
  ret->chan0 = ret->chan1 = NULL;
  ret->nchannels = Input->fmt_info.channels;
  ret->freq = Input->fmt_info.sampling_freq;

  if (Input->fmt_info.channels == 1)
    {
      /* Mono */
      ret->chan0 = (int16*) malloc(sizeof(int16)*how_many);
      if (ret->chan0 == NULL)
	goto bad;

      ret->chan1 = NULL;
      if (Input->fmt_info.bit_per_sample == 16)
	{
	  /* Mono 16 bit */
	  if (read_wav_samples(Input, ret->chan0, how_many) < 0)
	    goto bad;
	}
      else
	{
	  /* Mono 8 bit */

	  int n;
	  unsigned char buf;
	  for (n=0; n<how_many; n++)
	    {
	      if (read_wav_samples(Input, &buf, 1) < 0)
		goto bad;

	      ret->chan0[n]= eight_to_16(buf);

	      /*printf("%x\n", (unsigned int) buf);*/
	    }
	}
    }
  else
    {
      /* Stereo */
      ret->chan0 = (int16*) malloc(sizeof(int16)*how_many*2);
      if (ret->chan0 == NULL)
	goto bad;

      ret->chan1 = ret->chan0 + how_many;

      if (Input->fmt_info.bit_per_sample == 16)
	{
	  /* Stereo 16 bit */
	  int n;
	  int16 buf[2];
	  for (n=0; n<how_many; n++)
	    {
	      if (read_wav_samples(Input, buf, 1) < 0)
		goto bad;

	      ret->chan0[n]= buf[0];
	      ret->chan1[n]= buf[1];
	    }
	}
      else
	{
	  /* Stereo 8 bit */
	  int n;
	  unsigned char buf[2];
	  for (n=0; n<how_many; n++)
	    {
	      if (read_wav_samples(Input, buf, 1) < 0)
		goto bad;

	      ret->chan0[n]=eight_to_16(buf[0]);
	      ret->chan1[n]=eight_to_16(buf[1]);
	    }
	}

    }

 ok:
#ifdef __GNUC__
__attribute__((unused))
#endif
  return ret;

 bad:
  close_wav_file(Input);
  destroy_wav_samples(ret);
  return NULL;
}

void destroy_wav_samples(wav_samples *w)
{
  if (w != NULL)
    {
      if (w->chan0 != NULL)
	free(w->chan0);

      free(w);
    }
}


void close_wav_file(WAV_FILE *f)
{
  int32 size;
  int32 nsamples;

  if (f->mode == 'w')
    {
      /*
       * First of all, write all the pending data.
       */
      fflush(f->stream);
      
      /*
       * Now get the file size in bytes
       */
      fseek(f->stream, 0, SEEK_END);
      size = ftell(f->stream);
      
      /*
       * Now write the length fields.
       */
      size = size-8;
      fseek(f->stream, 4, SEEK_SET);
      fwrite(&size, sizeof(int32), 1, f->stream);

      nsamples = size-36;
      fseek(f->stream, 40, SEEK_SET);
      fwrite(&nsamples, sizeof(int32), 1, f->stream);
    }
  
  fclose(f->stream);
  still_open[f->idx]=NULL;
  free(f);

  return;
}
