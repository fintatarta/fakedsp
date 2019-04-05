# What is this?
This repository contains two small libraries (in C and Ada) that I wrote for my DSP course.

The goal of this library is to allow the students to write software with the same "style" that they would use to write software running on a DSP, but working on a PC.  This started as a tool to be used to do debugging in a more forgiving environment (a PC) than a DSP running in real time.  It can be used to introduce students to writing code in DSP-style, that is, processing samples as they arrive, without having the whole signal stored in a vector (like, for example, in Matlab/octave).

# How do I install the software?

As said above, this package includes two libraries: one in C, the other in Ada.  

* If you want to install the **C** library
    * Just copy the content of directory *C/* where your compiler will find it. 
    * Include "fakedsk.h" in your code
    * Link your code together  with all the C files that are in the directory
    * If you want a slightly more efficient approach, you can create a library to be linked with your code. See the documentation in the *C/* directory for details
* If you want to install the **Ada** library, 
  * Use one of the following two possible installation procedures
    1. Copy the content of *Ada/src/fakelib* where your compiler can find it, or, 
    1. If you use `gprbuild` (distributed with the Adacore suite) then copy the **whole** *Ada/* directory somewhere and `with` the file `fakedsp.gpr` in your project file
  * In both cases you'll use in your code the resources provided by packages `fakedsp`, `fakedsp.card` and `fakedsp.data_streams.files`
  * See also the documentation in *Ada/doc/html*

# How do I use this?

Both the C and Ada version share the same model, the details about how they are used differ a bit and are documented separately  The library simulates a DSP card with an ADC, a DAC and a processor that it receives an interrupt every time a new sample is ready. The virtual ADC reads its data from an external source represented by a file (WAV and text-based files are currently accepted), the virtual DAC writes on a destination (again a file).

The user writes the "virtual interrupt handler" and starts the virtual card.  The virtual interrupt handler can read the ADC and write to the DAC using provided functions.  The samples are represented as 16 bit signed integers.

The main program can wait for the card to stop (this happens when the virtual ADC read all the data from the source) or it can do other stuff, checking every now and then if the card stopped.

A tentative is done to call the virtual handler with the chosen sampling frequency.  Unfortunately, in common OS the resolution of timing in process switching is limited by the frequency the kernel is interrupted (100 Hz is a common value) and this makes almost impossible to have a realistici, say, 8 kHz sampling.

## Where do I find a more detailed explanation of the two libraries?

* Under the C directory you'll find the file **fakedsk-doc.pdf**
* Under the Ada directory **Ada/doc/html/** start from the file **rootdoc_docRoot2FFakedsp.html**.  If the file or the directory is missing, run robodoc in Ada
