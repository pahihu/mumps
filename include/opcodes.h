// File: mumps/include/opcodes.h
//
// module MUMPS header file - internal op codes (and only real opcodes)

/*      Copyright (c) 1999 - 2014
 *      Raymond Douglas Newman.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of Raymond Douglas Newman nor the names of the
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 *
 */


#ifndef _MUMPS_OPCODES_H_                       // only do this once
#define _MUMPS_OPCODES_H_

#define ENDLIN	0				// End of line
#define OPHALT  1                               // Halt instruction
#define OPERROR 2                               // short -(ERR) follows
#define OPNOT   3                               // boolean (int) NOT
#define OPENDC  4                               // end of command
#define JMP0	5				// jump if false
#define OPIFN	6				// no arg if
#define OPIFA	7				// if (check stack)
#define OPIFI	8				// if indirect
#define OPELSE	9				// else


#define OPADD   10                              // add top two on the stack
#define OPSUB   11                              // (sp-2) - (sp-1)
#define OPMUL   12                              // mult top two on the stack
#define OPDIV   13                              // (sp-2) / (sp-1)
#define OPINT   14                              // integer divide (MUMPS style)
#define OPMOD   15                              // modulus (MUMPS style)
#define OPPOW   16                              // x to the power y
#define OPCAT   17                              // a concatenated with b
#define OPPLUS	18				// unary plus
#define OPMINUS	19				// unary minus

#define OPEQL   20                              // a = b
#define OPLES   21                              // a < b
#define OPGTR   22                              // a > b
#define OPAND   23                              // a & b
#define OPIOR   24                              // a ! b (inclusive or)
#define OPCON   25                              // a contains b
#define OPFOL   26                              // a follows b
#define OPSAF   27                              // a sorts after b
#define OPPAT   28                              // a pattern matches b
#define OPHANG	29				// hang

#define OPNEQL  30                              // not a = b
#define OPNLES  31                              // not a < b
#define OPNGTR  32                              // not a > b
#define OPNAND  33                              // not a & b
#define OPNIOR  34                              // not a ! b (inclusive or)
#define OPNCON  35                              // not a contains b
#define OPNFOL  36                              // not a follows b
#define OPNSAF  37                              // not a sorts after b
#define OPNPAT  38                              // not a pattern matches b
//spare		39

//spare		40
#define CMSET	41				// set
#define CMSETE	42				// set $E()
#define CMSETP	43				// set $P()
#define OPNAKED	44				// set NAKED from mvar on astk
#define CMFLUSH	45				// flush inputs
#define CMREADS 46				// read star
#define CMREADST 47				// read star with timeout
#define CMREAD	48				// read variable
#define CMREADT	49				// read variable t/o

#define CMREADC	50				// read variable count
#define CMREADCT 51				// read variable count, t/o
#define CMWRTST	52				// write star
#define CMWRTNL	53				// write !
#define CMWRTFF	54				// write #
#define CMWRTAB	55				// write ?expr
#define CMWRTEX	56				// write expression
#define CMUSE	57				// use (args) ch, a1, a2, ...
#define CMOPEN	58				// open chan, p1, p2, timeout
#define CMCLOSE	59				// close channel

#define OPSTR   60                              // string follows in line
#define OPVAR   61                              // eval var name follows
#define OPMVAR  62                              // build mvar, name follows
#define OPMVARN 63                              // build mvar, (null OK)
#define OPMVARF 64                              // bld mvar, no null, full size
#define INDEVAL	65				// eval name indirection
#define INDMVAR	66				// mvar name indirection
#define INDMVARN 67				// mvar name ind (null ok)
#define INDMVARF 68				// mvar name ind, full size
//spare		69

#define OPBRK0	70				// argless break
#define OPBRKN	71				// break with arguments
#define OPDUPASP 72				// duplicate top of astk
//spare		73 -> 79

#define VARD	80				// $D[EVICE]
#define VAREC	81				// $EC[ODE]
#define VARES	82				// $ES[TACK],
#define VARET	83				// $ET[RAP]
#define VARH	84				// $H[OROLOG]
#define VARI	85				// $I[O]
#define VARJ	86				// $J[OB]
#define VARK	87				// $K[EY]
#define VARP	88				// $P[RINCIPAL]
#define VARQ	89				// $Q[UIT]

#define VARR	90				// $R[EFERENCE]
#define VARS	91				// $S[TORAGE]
#define VARST	92				// $ST[ACK]
#define VARSY	93				// $SY[STEM]
#define VART	94				// $T[EST]
#define VARX	95				// $X
#define VARY	96				// $Y
//spare		97 -> 99

#define FUNA1	100				// $A[SCII] 1 arg
#define FUNA2	101				// $A[SCII] 2 arg
#define FUNC	102				// $C[HARACTER]
#define FUND	103				// $D[ATA]
#define FUNE1	104				// $E[XTRACT] 1 arg
#define FUNE2	105				// $E[XTRACT] 2 arg
#define FUNE3	106				// $E[XTRACT] 3 arg
#define FUNF2	107				// $F[IND] 2 arg
#define FUNF3	108				// $F[IND] 3 arg
#define FUNFN2	109				// $FN[UMBER] 2 arg

#define FUNFN3	110				// $FN[UMBER] 2 arg
#define FUNG1	111				// $G[ET] 1 arg
#define FUNG2	112				// $G[ET] 2 arg
#define FUNJ2	113				// $J[USTIFY] 2 arg
#define FUNJ3	114				// $J[USTIFY] 3 arg
#define FUNL1	115				// $L[ENGTH] 1 arg
#define FUNL2	116				// $L[ENGTH] 2 arg
#define FUNNA1	117				// $NA[ME] 1 arg
#define FUNNA2	118				// $NA[ME] 1 arg
#define FUNO1	119				// $O[RDER] 1 arg

#define FUNO2	120				// $O[RDER] 1 arg
#define FUNP2	121				// $P[IECE] 2 arg
#define FUNP3	122				// $P[IECE] 3 arg
#define FUNP4	123				// $P[IECE] 4 arg
#define FUNQL	124				// $QL[ENGTH]
#define FUNQS	125				// $QS[UBSCRIPT]
#define FUNQ1	126				// $Q[UERY] 1 arg
#define FUNQ2	127				// $Q[UERY] 2 arg
#define FUNR	128				// $R[ANDOM]
#define FUNRE	129				// $RE[VERSE]

#define FUNST1	130				// $ST[ACK]
#define FUNST2	131				// $ST[ACK] 2 arg
#define FUNT	132				// $T[EXT]
#define FUNTR2	133				// $TR[ANSLATE] 2 arg
#define FUNTR3	134				// $TR[ANSLATE] 3 arg
#define FUNV2	135				// $V[IEW] - 2 arg
#define FUNV3	136				// $V[IEW] - 3 arg
#define FUNV4	137				// $V[IEW] - 4 arg
#define CMVIEW	138				// VIEW command - 4 args
#define CMMERGE	139				// merge 1 variable from nxt

#define CMDOWRT	140				// DO from WRITE /xxx[(param)]
#define CMDOTAG	141				// DO tag in this rou [args]
#define CMDOROU	142				// DO routine (no tag) [args]
#define CMDORT	143				// DO routine, tag [args]
#define CMDORTO	144				// DO routine, tag, off [args]
#define CMDON	145				// DO - no arguments
#define CMJOBTAG 146				// JOB tag in this rou [args]
#define CMJOBROU 147				// JOB routine (no tag) [args]
#define CMJOBRT	148				// JOB routine, tag [args]
#define CMJOBRTO 149				// JOB routine, tag, off [args]

#define CMGOTAG	150				// GOTO tag in this rou
#define CMGOROU	151				// GOTO routine (no tag)
#define CMGORT	152				// GOTO routine, tag
#define CMGORTO	153				// GOTO routine, tag, off
#define CMXECUT	154				// XECUTE
#define CMXECI	155				// XECUTE indirect
#define CHKDOTS	156				// check current level
#define CMQUIT	157				// QUIT - no arg (not FOR)
#define CMQUITA	158				// QUIT with argument
//spare		159

#define	CMLCKU	160				// un LOCK all
#define CMLCK	161				// LOCK #args()
#define CMLCKP	162				// LOCK + #args()
#define CMLCKM	163				// LOCK - #args()
#define CMNEW	164				// NEW
#define CMNEWB	165				// NEW #args() - new except
#define CMKILL	166				// kill 1 variable
#define CMKILLB	167				// kill but() args
#define NEWBREF	168				// push null for NEW by ref
#define VARUNDF 169				// point at VAR_UNDEFINED

#define LINENUM	170				// set rou line number
#define LOADARG 171				// load args (illegal in line)
#define JMP	172				// unconditional jump
#define CMFOR0	173				// argless FOR
#define CMFOR1	174				// FOR with 1 argument
#define CMFOR2	175				// FOR with 2 arguments
#define CMFOR3	176				// FOR with 3 arguments
#define CMFORSET 177				// setup FOR
#define CMFOREND 178				// Jump to end of line
#define OPNOP	179				// NOP

#define INDREST	180				// restore isp & mumpspc
#define INDCLOS 181				// CLOSE arg indir
#define INDDO	182				// DO arg indir
#define INDGO	183				// GOTO arg indir
#define INDHANG 184				// HANG arg indir
#define INDIF	185				// IF arg indir
#define INDJOB	186				// JOB arg indir
#define INDKILL	187				// KILL arg indir
#define INDLOCK	188				// LOCK arg indir
#define INDMERG	189				// MERGE arg indir

#define INDNEW	190				// NEW arg indir
#define INDOPEN	191				// OPEN arg indir
#define INDREAD	192				// READ arg indir
#define INDSET	193				// SET arg indir
#define INDUSE	194				// USE arg indir
#define INDWRIT	195				// WRITE arg indir
#define INDXEC	196				// XECUTE arg indir
//spare		197 -> 199

//spare		200 -> 229

//spare		230 -> 233
#define XCWAIT  234                             // Xcall $&%WAIT()
#define XCCOMP	235				// Xcall $&%COMPRESS()
#define XCSIG   236				// Xcall $&%SIGNAL()
#define XCHOST	237				// Xcall $&%HOST()
#define XCFILE	238				// Xcall $&%FILE()
#define XCDEBUG 239				// Xcall $&DEBUG()

#define XCDIR	240				// Xcall $&%DIRECTORY()
#define XCERR	241				// Xcall $&%ERRMSG()
#define XCOPC	242				// Xcall $&%OPCOM()
#define XCSPA	243				// Xcall $&%SPAWN()
#define XCVER	244				// Xcall $&%VERSION()
#define XCZWR	245				// Xcall $&%ZWRITE()
#define XCE	246				// Xcall $&E()
#define XCPAS	247				// Xcall $&PASCHK()
#define XCV	248				// Xcall $&V()
#define XCX	249				// Xcall $&X()

#define XCXRSM	250				// Xcall $&XRSM()
#define XCSETENV 251				// Xcall $&%SETENV()
#define XCGETENV 252				// Xcall $&%GETENV()
#define XCROUCHK 253				// Xcall $&%ROUCHK()
#define XCFORK	254				// Xcall $&%FORK()
#define XCIC	255				// Xcall $&%IC()

#endif                                          // _MUMPS_OPCODES_H_
