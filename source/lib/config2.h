/* Copyright (c) 2010 Wildfire Games
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * compile-time configuration for isolated spots
 */

#ifndef INCLUDED_CONFIG2
#define INCLUDED_CONFIG2

// rationale: a centralized header makes it much easier to see what all
// can be changed. it is assumed that only a few modules will need
// configuration choices, so rebuilding them all is acceptable.
// use config.h when settings must apply to the entire project.

// allow use of RDTSC for raw tick counts (otherwise, the slower but
// more reliable on MP systems wall-clock will be used).
#ifndef CONFIG2_TIMER_ALLOW_RDTSC
# define CONFIG2_TIMER_ALLOW_RDTSC 1
#endif

// this enables/disables the actual checking done by OverrunProtector
// (quite slow, entailing mprotect() before/after each access).
// define to 1 here or in the relevant module if you suspect mem corruption.
// we provide this option because OverrunProtector requires some changes to
// the object being wrapped, and we want to leave those intact but not
// significantly slow things down except when needed.
#ifndef CONFIG2_ALLOCATORS_OVERRUN_PROTECTION
# define CONFIG2_ALLOCATORS_OVERRUN_PROTECTION 0
#endif

// zero-copy IO means all clients share the cached buffer; changing their
// contents is forbidden. this flag causes the buffers to be marked as
// read-only via MMU (writes would cause an exception), which takes a
// bit of extra time.
#ifndef CONFIG2_CACHE_READ_ONLY
#define CONFIG2_CACHE_READ_ONLY 1
#endif

// enable the wsdl emulator in Windows builds.
//
// NOTE: the official SDL distribution has two problems on Windows:
// - it specifies "/defaultlib:msvcrt.lib". this is troublesome because
//   multiple heaps are active; errors result when allocated blocks are
//   (for reasons unknown) passed to a different heap to be freed.
//   one workaround is to add "/nodefaultlib:msvcrt.lib" to the linker
//   command line in debug configurations.
// - it doesn't support color hardware mouse cursors and clashes with
//   cursor.cpp's efforts by resetting the mouse cursor after movement.
#ifndef CONFIG2_WSDL
# define CONFIG2_WSDL 1
#endif

#ifndef CONFIG2_FILE_ENABLE_AIO
// work around a bug introduced in Linux 2.6.38
// (http://www.wildfiregames.com/forum/index.php?showtopic=14561&view=findpost&p=217710)
# if OS_LINUX
#  define CONFIG2_FILE_ENABLE_AIO 0
# else
#  define CONFIG2_FILE_ENABLE_AIO 1
# endif
#endif

// allow an attempt to start the Aken driver (i.e. service) at runtime.
// enable at your own risk on WinXP systems to allow access to
// better timers than Windows provides. on newer Windows versions,
// attempts to start the service from code fail unless the process
// is elevated, and definitely fail due to lack of cross-signing unless
// test-signing mode is active.
// if the user has taken explicit action to install and start the
// service via aken_install.bat, mahaf.cpp will be able to access it
// even if this is defined to 0.
#ifndef CONFIG2_MAHAF_ATTEMPT_DRIVER_START
# define CONFIG2_MAHAF_ATTEMPT_DRIVER_START 0
#endif

#endif	// #ifndef INCLUDED_CONFIG2
