
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


/** @mainpage WaveIO
 *
 * \section sec_intro Introduction
 *
 *   WaveIO is a simple C library which allows for reading/writing 
 *   WAV files.  The file must be PCM coded, 8 or 16 bits/sample and with 
 *   1 or 2 channels.
 *
 *   I wrote this library for my students, in order to allow them to do 
 *   experiments with audio signals.  I needed a simple interface and I did
 *   not want to rely on any external libraries => I decided to write
 *   this software by myself.
 *
 *   Although I tested and debugged this library, I would keep it, by now, 
 *   in a "beta" state.  Please, let me know if you 
 *   discover/correct any bugs.
 *
 * \section sec_contents What do I find in WaveIO?
 *
 *   WaveIO gives you
 *
 *     - Functions for opening WAV files in input/output
 *         - open_input_wav()
 *         - open_output_wav()
 *
 *     - Functions for reading/writing samples from/to WAV files
 *         - read_wav_samples()
 *         - write_wav_samples()
 *
 *     - You can also "slurp" a whole WAV file in memory with
 *         - slurp_wav_file()
 *
 * \section sec_compile How do I use it?
 *
 *   -# Put waveio.c and waveio.h in a directory searched by your 
 *      C compiler
 *   -# Make waveio.o from waveio.c (e.g. with <tt>cc -c waveio.c</tt>)
 *   -# Link waveio.o with your program (e.g. with 
 *      <tt>cc -o myprogram myprogram.o waveio.o</tt>)
 *
 * \section sec_tipico  Typical usage
 * \code
 *
 * #include <errno.h>
 * #include "waveio.h"
 *
 * // In case of error, print a suitable message and exit.
 * void handle_error(char *msg)
 * {
 *   waveio_perror(msg);
 *   exit(1);
 * }
 *
 *
 * int main(int argc, char **argv)
 * {
 *   WAV_FILE *Output;
 * 
 *   wav_samples *buf;
 *   int16 data[N];
 *
 *    // Read the samples of pippo.wav
 *   buf = slurp_wav_file("/tmp/pippo.wav");
 *   if (buf==NULL)  
 *      handle_error("Could not read pippo.wav");
 *
 *    // Process the first channel of pippo.wav to obtain some other
 *    // audio data (which goes in data[])
 *   process_audio(data, buf->chan0);
 *
 *    // Write the result to pluto.wav
 *   Output = open_output_wav("/tmp/pluto.wav", // Output file
 *                            1,                // mono
 *                  // use the same bit/sample and sampling frequency
 *                  // of the input file
 *                            buf->bit_per_samples,             
 *                            buf->freq);
 *   if (Output==NULL) 
 *      handle_error("Could not open pluto.wav");
 * 
 *   if (write_wav_samples(Output, data, buf->nsamples) < buf->nsamples)
 *      handle_error("Error while writing to pluto.wav");
 * 
 *   close_wav_file(Output);
 * 
 *   return 0;
 * }
 * \endcode

 *
 */

/** @file waveio.h
 * \brief Simple functions for I/O on WAV file
 *
 * This modules contains functions for reading and writing simple WAV 
 * files (at most 2 channels, PCM coding, 8 or 16 bits/sample).
 *
 * <h2>Error reporting</h2>
 *
 * There are two different classes of errors: 
 *
 *   - errors found by the library (e.g. a WAV file with a wrong format)
 *
 *   - errors returned by system functions (e.g. fopen returns NULL
 *     because it cannot open the file)
 *
 * In order to distinguish between the two classes we will use a global
 * variable waveio_errno as follows
 *
 *   - If the error was found by the library we put an error code in 
 *     waveio_errno and return
 *
 *   - If the error was reported by a system function such as fopen(),
 *     then we put waveio_errno=WAVEIO_OK and return.  The code of 
 *     returned by the system function will be in errno.
 *
 */
#ifndef __WAVEIO_H__
#define __WAVEIO_H__

#include <stdio.h>

extern int   waveio_errno;     /**< Waveio error code */
extern char *waveio_errmsg[];  /**< Waveio error messages */

/** No error or system error (see discussion about error reporting). 
 */
#define WAVEIO_OK   0  
#define WAVEIO_EOF -1          /**< End-of-file while reading. */

#define WAVEIO_OUT_OF_ROOM -2  /**< Too many open files. */
#define WAVEIO_INV         -3  /**< Bad WAV file. */
#define WAVEIO_NO_PCM      -4  /**< WAV file does not use PCM. */

typedef short int16;
typedef int   int32;

/** Format descriptor.
 * This struct is the copy of the "fmt " chunk in the WAV file.  It
 * contains several informations such as number of channels, sampling 
 * frequency and so on...
 */
typedef struct {
  /* Begin of FMT chunk */
  char fmt[4];            /* String "fmt ". */
  int32 chunk_length;     /* Length of FMT chunk >= 16 byte. */
  int16 format_type;      /* Format tag (1=PCM). */
  int16 channels;         /**< # of channels. */
  int32 sampling_freq;    /**< Sampling freq in Hz. */
  int32 byte_per_sec;     /**< # of byte/sec = sampling*byte_by_capture. */
  int16 byte_by_capture;  /**< channels * bit_per_sample/8. */
  int16 bit_per_sample;   /**< 8 or 16. */
} wav_fmt;

/** WAV file descriptor, it plays the role of FILE in stdio.
 */
typedef struct {
  FILE *stream;   
  wav_fmt fmt_info; /**< Format information. */
  int nsamples;     /**< # of samples in the WAV file */
  int  idx;
  char mode;   /* 'r' => open for reading, 'w' => writing */
} WAV_FILE;

/** Returned by slurp_wav_file, contains the samples in the WAV file.
 *
 * This struct is filled by slurp_wav_file() with the samples contained
 * in the WAV file.  Note that slurp_wav_file() always returns 16 bit samples, 
 * even if the file is an 8 bit one.
 */
typedef struct {
  int bit_per_sample;  /**< # of bit/samples (8 or 16). */
  int nsamples;        /**< # of samples per channel.   */
  int nchannels;       /**< # of channels (1 or 2).     */
  int freq;            /**< Sampling frequency (in Hz). */
  int16 *chan0;        /**< Array of the samples in the first channel. */
  int16 *chan1;        /**< Array of the samples in the second channel.
                            If the file is mono (and nchannels=1), then
			    chan1=NULL, otherwise chan1=chan0+nsamples. 
		       */
} wav_samples;



/*! Open a WAV file in input.  It returns a pointer to the newly opened 
 * file if everything is OK, otherwise returns NULL.
 */
WAV_FILE *open_input_wav(char *filename);

/*! Open a WAV file in output.  It returns a pointer to the newly opened 
 * file if everything is OK, otherwise returns NULL.
 */
WAV_FILE *open_output_wav(char *filename,
			  int nchan,      /**< # of channels (1 or 2). */
			  int nbits,      /**< # of  bit/samples (8 or 16). */
			  int freq        /**< Sampling frequency. */
			  );

/** Close a WAV file opened with open_output_wav() or open_input_wav(). */
void close_wav_file(WAV_FILE *f);

/** Write some samples to a WAV file.
 * Parameter buf points to a char array or to an int array, depending
 * on the WAV file type (with 8 or 16 bits/sample).  If succesful, it 
 * returns 0, otherwise it returns -1 and variables errno/waveio_errno
 * are setted.
 */
int write_wav_samples(WAV_FILE *Output, void *buf, int how_many);

/** Read some samples from a WAV file.
 * Parameter buf points to a char array or to an int array, depending
 * on the WAV file type (with 8 or 16 bits/sample). If succesful, it 
 * returns 0, otherwise it returns -1 and variables errno/waveio_errno
 * are setted.
 */
int read_wav_samples(WAV_FILE *Input, void *buf, int how_many);

/** Read a whole WAV file.
 * The samples and other informations (such as # of channels, sampling 
 * frequency and so on) are returned in a wav_samples structure.
 * It returns NULL and sets  variables errno/waveio_errno if some 
 * error happens. 
 */
wav_samples *slurp_wav_file(char *filename);

/** Free the memory used by a wav_samples struct. */
void destroy_wav_samples(wav_samples *w);

/** Print an error message "a la" perror().
 *  Differently from perror(), it handles both system error messages
 *  (e.g. from fopen()) and WaveIO error messages.  If the library has
 *  been compiled with symbol VARARG_PERROR defined (the default), it 
 *  accepts a variable number of arguments as printf() (i.e., the first
 *  argument is a format printf-like and the following arguments are
 *  used in formatting the error message).
 */
void waveio_perror(char *fmt, ...);

#define VARARG_PERROR /* Undef this if you do not have vfprintf() */

#endif
