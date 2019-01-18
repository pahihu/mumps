#include <stdint.h>
#include <string.h>

#include "bswap.h"

uint16_t BSwap16(uint16_t n)
{
    return ((0x00FFU & n) << 8) + ((0xFF00U & n) >> 8);
}

uint32_t BSwap32(uint32_t n)
{
  unsigned char d[4];

  memcpy(d,&n,4);
  return ((uint32_t)(d[0])<<24)+((uint32_t)(d[1])<<16)+
    ((uint32_t)(d[2])<< 8)+ (uint32_t)(d[3]);
}

uint64_t BSwap64(uint64_t n)
{
  unsigned char d[8];

  memcpy(d,&n,8);
  return ((uint64_t)(d[0])<<56)+((uint64_t)(d[1])<<48)+
    ((uint64_t)(d[2])<<40)+((uint64_t)(d[3])<<32)+
    ((uint64_t)(d[4])<<24)+((uint64_t)(d[5])<<16)+
    ((uint64_t)(d[6])<< 8)+ (uint64_t)(d[7]);
}
