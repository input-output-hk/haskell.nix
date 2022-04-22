// Just enoough windows.h stuff to make rts headers usable when building GHC cross compiler.
// See compiler/ghc/default.nix for where this is used.
#ifndef _WINDOWS_
#define _WINDOWS_

#define __stdcall

typedef unsigned long DWORD;
typedef void * PVOID;

typedef struct _RTL_CONDITION_VARIABLE { PVOID Ptr; } RTL_CONDITION_VARIABLE;
typedef RTL_CONDITION_VARIABLE CONDITION_VARIABLE;

typedef struct _RTL_SRWLOCK { PVOID Ptr; } RTL_SRWLOCK;
typedef RTL_SRWLOCK SRWLOCK;

#endif

