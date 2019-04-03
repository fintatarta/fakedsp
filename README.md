# What is this?
This repository contains two small libraries (in C and Ada) that I wrote for my DSP course.  

The goal of this library is to allow the students to write software with the same "style" that they would use to write software running on a DSP, but working on a PC.  This started as a tool to be used to do debugging in a more forgiving environment (a PC) than a DSP running in real time.  It can be used to introduce students to writing code in DSP-style, that is, processing samples as they arrive, without having the whole signal stored in a vector (like, for example, in Matlab/octave).

# How do I use this?

Both the C and Ada version share the same model, the details about how they are used differ a bit and are documented separately  The library simulates a DSP card with an ADC, a DAC and a processor that it receives an interrupt every time a new sample is ready. The virtual ADC reads its data from an external source represented by a file (WAV and text-based files are currently accepted), the virtual DAC writes on a destination (again a file).

The user writes the "virtual interrupt handler" and starts the virtual card.  The virtual interrupt handler can read the ADC and write to the DAC using provided functions.  The samples are represented as 16 bit signed integers.

The main program can wait for the card to stop (this happens when the virtual ADC read all the data from the source) or it can do other stuff, checking every now and then if the card stopped.

A tentative is done to call the virtual handler with the chosen sampling frequency.  Unfortunately, in common OS the resolution of timing in process switching is limited by the frequency the kernel is interrupted (100 Hz is a common value) and this makes almost impossible to have a realistici, say, 8 kHz sampling.  

# Why Ada?  I do not know it!

Well, you can learn... :-) 

I think that Ada is a very nice and modern language that helps you writing very robust software easily and in short time. Debugging times are typically 10% of the debugging time of C. (**Note**: 10% *of..* **not** 10% *smaller...*) I abandoned C since I learned Ada. A main drawback is that is it is not widely known and there is not much material to learn it.  Nevertheless, there are few *very good* resources

* [Ada 95 tutorial]:https://dwheeler.com/lovelace/lovelace.htm [Ada 95 tutorial] 
This is an old tutorial (with excercises) about Ada 95.  I learned Ada with this.  Ada 95 is a fairly old version of Ada (now we are at Ada 2012) and many new features are missing, but it is a good introduction to the language.  If you are new to the language, breaking the ground with this can be a good idea.
* [Adacore]:https://learn.adacore.com/ [Adacore]
Adacore is the producer of GNAT, the gcc frontend for Ada. In this site you can find an introduction to Ada and an introduction to SPARK (a subset of Ada suitable for formal checking, nothing to do with Apache...).  I did not use them, but they look nice (and relative to more modern Ada versions)
* [Adaic]:https://www.adaic.org/learn/materials/ [Adaic] This is a meta-resource, it is a site with lots of links to learning material, libraries, tools, and so on...  
* [RM]:https://www.adaic.org/resources/add_content/standards/12rm/html/RM-TOC.html [RM] It is not possible not to mention the (in)famous and fundamental **Reference Manual**. This is the standard ISO describing the language, it is written in "computer science legalese" and if it was a bit more obscure it would seem encrypted. :-)  Seriously, this is the ultimate resource to know about Ada, although you need to get used to it.  The annotated version and the corresponding "Rationale" (that you can find on the Adaic site) often can help since they do not need to be as formal as the reference text. 
* There is also the newsgroup (yes! they are still alive!) **comp.lang.ada** quite active, with a good signal to noise ratio and many people wiling to help beginners (although you must be nice too... this goes without saying...).  There is also a LinkedIn group, but it is less active and more for announcements etc. I know that there is also an IRC channel, but I am not an IRC user.

You can download open source IDE, compilers, and so on... from the [Adacore]:https://www.adacore.com/download [Adacore] site. 

Oh, yes, if you come definitively to the dark side and want to meet with other Adaists, keep an eye on the [FOSDEM]:https://fosdem.org/ the Free and Open Source Developers European Meeting, hold yearly in Bruxells.  Often in the program there is the *Ada dev room* with interesting talks and introductions for beginners.  Some past editions

* [2019]: https://fosdem.org/2019/schedule/track/ada/ [2019]
* [2018]: https://archive.fosdem.org/2018/schedule/track/ada/ [2018]
* [2016]: https://archive.fosdem.org/2016/schedule/track/ada/ [2016]

