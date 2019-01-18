#ifndef _BSWAP_H
#define _BSWAP_H

#include <stdint.h>

#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
#include <sys/endian.h>
#endif

#if defined(__linux__) && defined(__GNUC__)
#include <byteswap.h>
#define bswap16 __bswap_16
#define bswap32 __bswap_32
#define bswap64 __bswap_64
#endif

#ifdef _MSC_VER
#include <stdlib.h>
#define bswap16 _byteswap_ushort
#define bswap32 _byteswap_ulong
#define bswap64 _byteswap_uint64
#endif

#ifdef __APPLE__
#include <libkern/_OSByteOrder.h>
#define bswap16 _OSSwapInt16
#define bswap32 _OSSwapInt32
#define bswap64 _OSSwapInt64
#endif

#ifndef bswap16

uint16_t BSwap16(uint16_t x);
uint32_t BSwap32(uint32_t x);
uint64_t BSwap64(uint64_t x);

#define bswap16 BSwap16
#define bswap32 BSwap32
#define bswap64 BSwap64

#endif

#endif
