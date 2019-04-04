# What is this?
This repository contains two small libraries (in C and Ada) that I wrote for my DSP course.

The goal of this library is to allow the students to write software with the same "style" that they would use to write software running on a DSP, but working on a PC.  This started as a tool to be used to do debugging in a more forgiving environment (a PC) than a DSP running in real time.  It can be used to introduce students to writing code in DSP-style, that is, processing samples as they arrive, without having the whole signal stored in a vector (like, for example, in Matlab/octave).

# How do I use this?

Both the C and Ada version share the same model, the details about how they are used differ a bit and are documented separately  The library simulates a DSP card with an ADC, a DAC and a processor that it receives an interrupt every time a new sample is ready. The virtual ADC reads its data from an external source represented by a file (WAV and text-based files are currently accepted), the virtual DAC writes on a destination (again a file).

The user writes the "virtual interrupt handler" and starts the virtual card.  The virtual interrupt handler can read the ADC and write to the DAC using provided functions.  The samples are represented as 16 bit signed integers.

The main program can wait for the card to stop (this happens when the virtual ADC read all the data from the source) or it can do other stuff, checking every now and then if the card stopped.

A tentative is done to call the virtual handler with the chosen sampling frequency.  Unfortunately, in common OS the resolution of timing in process switching is limited by the frequency the kernel is interrupted (100 Hz is a common value) and this makes almost impossible to have a realistici, say, 8 kHz sampling.


