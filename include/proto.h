// File: mumps/include/proto.h
//
// module MUMPS header file - prototypes

/*      Copyright (c) 1999 - 2013
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


#ifndef _MUMPS_PROTO_H_                         // only do this once
#define _MUMPS_PROTO_H_


//****************************************************************************
// Identifier
//
chr_x *_X_Clear(chr_x *a);
int   _X_EQ(chr_x *a, chr_x *b);
int   _X_NE(chr_x *a, chr_x *b);
int   _X_Empty(chr_x *a);
void  X_set(const void *src, void *dst, size_t n);
int   X_put(const chr_x *a, u_char *b);
int   X_take(u_char *a, chr_x *b);

#define X_Clear(x)    _X_Clear(&(x))
#define X_EQ(x,y)     _X_EQ(&(x),&(y))
#define X_NE(x,y)     _X_NE(&(x),&(y))
#define X_Empty(x)    _X_Empty(&(x))

//****************************************************************************
// Database
//

// Database prototypes
short DB_Get(mvar *var, u_char *buf);            // get global data
short DB_Set(mvar *var, cstring *data);          // set global data
short DB_Data(mvar *var, u_char *buf);           // get $DATA()
short DB_Kill(mvar *var);                        // remove sub-tree
short DB_Mount( char *file, int volnum, int gmb); // mount dataset
short DB_Order(mvar *var, u_char *buf, int dir); // get next subscript
short DB_Query(mvar *var, u_char *buf, int dir); // get next key
short DB_QueryD(mvar *var, u_char *buf);	     // get next key and data
short DB_GetLen( mvar *var, int lock, u_char *buf); // return length of global
short DB_Compress(mvar *var, int flags);	     // on line compressor
int DB_Free(int vol);                            // return total free blocks
short DB_UCISet(int vol, int uci, var_u name);   // set uci name
short DB_UCIKill(int vol, int uci);              // kill uci entry
short DB_Expand(int vol, u_int vsiz);		     // expand db
int DB_Dismount(int vol);			             // dismount a volume
void ClearJournal();				             // clear journal
void DB_StopJournal(int vol, u_char action);	 // Stop journal
int DB_GetFlags(mvar *var);                    	 // Get flags
int DB_SetFlags(mvar *var, int flags);         	 // Set flags
int DB_ic(int vol, int block);			         // integrity checker
struct GBD *DB_ViewGet(int vol, int block);      // return gbd address of
                                                 // specified block, null on err
void DB_ViewPut(int vol, struct GBD *ptr);       // que block for write
void DB_ViewRel(int vol, struct GBD *ptr);       // release block, gbd -> free


//****************************************************************************
// Sequential IO
//
short SQ_Init();				// init chan 0 etc
short SQ_Open(int chan,                         // open on this channel
              cstring *object,                  // this file/device
              cstring *op,                      // in this mode
              int tout);                        // timeout (-1=unlimited)
short SQ_Use(int chan,                          // set chan as current $IO
             cstring *interm,                   // input terminators or NULL
             cstring *outerm,                   // output terminators or NULL
             int par);                           // parameters see mumps.h
short SQ_Close(int chan);                       // close channel
short SQ_Write(cstring *buf);                   // write to current $IO
short SQ_WriteStar(u_char c);                   // output one character
short SQ_WriteFormat(int count);                // write format chars
short SQ_Read(u_char *buf,                      // read from current $IO to buf
              int tout,                         // timeout (-1=unlimited)
              int maxbyt);                      // maximum bytes (-1=unlimited)
short SQ_ReadStar(int *result,			// read one character
		  int timeout );		// timeout (-1=unlimited)
short SQ_Flush();                               // flush input on $IO
short SQ_Device(u_char *buf);                   // return attributes
short SQ_Force(cstring *device,
               cstring *msg);                   // force data to a device

//****************************************************************************
// Compiler
//
short Compile_Routine(mvar *rou, mvar *src, u_char *stack); // whole routine
void eval();					// compiler
void parse();					// ditto
void dodollar();				// parse var, funct etc
short routine(int runtime);			// parse routine ref

//****************************************************************************
// Runtime
//
// Runtime Utilities
int cstringtoi(cstring *str);                   // convert cstring to int
int cstringtob(cstring *str);                   // convert cstring to boolean
short itocstring(u_char *buf, int n);		// convert int to string
short uitocstring(u_char *buf, u_int n);	// convert u_int to string
int mumps_version(u_char *ret_buffer);          // return version string
short Set_Error(int err, cstring *user, cstring *space); // Set $ECODE

short run(int asp, int ssp);			// run compiled code
short buildmvar(mvar *var, int nul_ok, int asp); // build an mvar
short patmat(cstring *str, cstring *pattern);	// pattern match
short attention();				// process attention
int ForkIt(int cft);				// Fork (copy file table)
void SchedYield();				// do a sched_yield()
void DoInfo();					// for control t

// Runtime math (decimal ex FreeMUMPS)
short runtime_add(char *a, char *b);		// add b to a
short runtime_mul(char *a, char *b);		// mul a by b
short runtime_div(char *uu, char *v, short typ); // divide string arithmetic
short runtime_power(char *a, char *b); 		// raise a to the b-th power
short runtime_comp (char *s, char *t);		// compare

// Runtime Functions
short Dascii1(u_char *ret_buffer, cstring *expr);
short Dascii2(u_char *ret_buffer, cstring *expr, int posn);
short Dchar(u_char *ret_buffer, int i);
short Ddata(u_char *ret_buffer, mvar *var);
short Dextract(u_char *ret_buffer, cstring *expr, int start, int stop);
short Dfind2(u_char *ret_buffer, cstring *expr1, cstring *expr2);
short Dfind3(u_char *ret_buffer, cstring *expr1, cstring *expr2, int start);
int   Dfind3x(cstring *expr1, cstring *expr2, int start);
short Dfnumber2(u_char *ret_buffer, cstring *numexp, cstring *code);
short Dfnumber3(u_char *ret_buffer, cstring *numexp, cstring *code, int rnd);
short Dget1(u_char *ret_buffer, mvar *var);
short Dget2(u_char *ret_buffer, mvar *var, cstring *expr);
short Djustify2(u_char *ret_buffer, cstring *expr, int size);
short Djustify3(u_char *ret_buffer, cstring *expr, int size, int round);
short Dlength1(u_char *ret_buffer, cstring *expr);
short Dlength2(u_char *ret_buffer, cstring *expr, cstring *delim);
short Dname1(u_char *ret_buffer, mvar *var);
short Dname2(u_char *ret_buffer, mvar *var, int sub);
short Dorder1(u_char *ret_buffer, mvar *var);
short Dorder2(u_char *ret_buffer, mvar *var, int dir);
short Dpiece2(u_char *ret_buffer, cstring *expr, cstring *delim);
short Dpiece3(u_char *ret_buffer, cstring *expr, cstring *delim, int i1);
short Dpiece4(u_char *ret_buffer, cstring *expr,
	cstring *delim, int i1, int i2);
short Dquery1(u_char *ret_buffer, mvar *var);
short Dquery2(u_char *ret_buffer, mvar *var, int dir);
short Drandom(u_char *ret_buffer, int seed);
short Dreverse(u_char *ret_buffer, cstring *expr);
short Dstack1(u_char *ret_buffer, int level);
short Dstack1x(u_char *ret_buffer, int level, int job);
short Dstack2(u_char *ret_buffer, int level, cstring *code);
short Dstack2x(u_char *ret_buffer, int level, cstring *code, int job);
short Dtext(u_char *ret_buffer, cstring *str);
short Dtranslate2(u_char *ret_buffer, cstring *expr1, cstring *expr2);
short Dtranslate3(u_char *ret_buffer, cstring *expr1,
                 cstring *expr2, cstring *expr3);
short Dview(u_char *ret_buffer, int chan, int loc,
            int size, cstring *value);			// $VIEW()

short DSetpiece(u_char *tmp, cstring *cptr, mvar *var,
		cstring *dptr, int i1, int i2);		// Set $PIECE()
short DSetextract(u_char *tmp, cstring *cptr, mvar *var,
		  int i1, int i2);			// Set $EXTRACT()


// Runtime Variables
short Vecode(u_char *ret_buffer);
short Vetrap(u_char *ret_buffer);
short Vhorolog(u_char *ret_buffer);
short Vkey(u_char *ret_buffer);
short Vreference(u_char *ret_buffer);
short Vsystem(u_char *ret_buffer);
short Vx(u_char *ret_buffer);
short Vy(u_char *ret_buffer);
short Vset(mvar *var, cstring *cptr);		// set a special variable

// Symbol Table prototypes
short ST_Get(mvar *var, u_char *buf);           // get local data
short ST_GetAdd(mvar *var, cstring **add); 	// get local data address
short ST_Set(mvar *var, cstring *data);         // set local data
short ST_Data(mvar *var, u_char *buf);          // get $DATA()

short ST_Kill(mvar *var);                       // remove sub-tree
short ST_KillAll(int count, var_u *keep);	// kill all except spec in keep
short ST_Order(mvar *var, u_char *buf, int dir); // get next subscript
short ST_Query(mvar *var, u_char *buf, int dir); // get next key
short ST_QueryD(mvar *var, u_char *buf);	// get next key and data
short ST_Dump();				// dump the symbol table
short ST_DumpV(mvar *global);			// dump symtab vars as subs

short ST_SymAtt(chr_x *var);			// attach to variable
void  ST_SymDet(int count, short *list);	// detach from variables
short ST_SymGet(short syment, u_char *buf);	// get using syment
short ST_SymSet(short syment, cstring *data);	// set using syment
short ST_SymKill(short syment);			// kill var using syment

short ST_New(int count, var_u *list);		// new a list of vars
short ST_NewAll(int count, var_u *list);	// new all other than listed
short ST_ConData(mvar *var, u_char *data);	// connect reference to data

// SSVN prototypes
short SS_Norm(mvar *var);                       // "normalize" ssvn
short SS_Get(mvar *var, u_char *buf);           // get ssvn data
short SS_Set(mvar *var, cstring *data);         // set ssvn data
short SS_Data(mvar *var, u_char *buf);          // get $DATA()
short SS_Kill(mvar *var);                       // remove sub-tree
short SS_Order(mvar *var, u_char *buf, int dir); // get next subscript

// Key Utility prototypes
short UTIL_Key_Build( cstring *src, u_char *dest); // locn of source string
short UTIL_Key_Extract( u_char *key,
	u_char *str, int *cnt); 		// extract subscript
short UTIL_String_Key( u_char *key,
	u_char *str, int max_subs); 		// extr all keys
short UTIL_String_Mvar( mvar *var,
	u_char *str, int max_subs); 		// mvar -> string
int UTIL_Key_Last( mvar *var);			// point at last subs in mvar
short UTIL_MvarFromCStr( cstring *src, mvar *var); // cvt cstring to mvar
int UTIL_Key_KeyCmp(u_char *key1, u_char *key2, int kleng1, int kleng2);
int UTIL_Key_Chars_In_Subs( char *Key, int keylen,
	int maxsubs, int *subs, char *KeyBuffer );

// General utility prototypes
short UTIL_strerror(int err, u_char *buf);      // return string error msg
short mcopy(u_char *src, u_char *dst, int bytes); // bcopy with checking etc
short ncopy(u_char **src, u_char *dst);         // copy as number
void CleanJob(int job);				// tidy up a job
void panic(char *msg); 				// die on error
struct RBD *Routine_Attach(chr_x routine);	// attach to routine
void Routine_Detach(struct RBD *pointer);	// Detach from routine
void Routine_Delete(chr_x routine, int uci);	// mark mapped routine deleted
void Dump_rbd();				// dump descriptors
void Dump_lt();					// dump used/free lockspace

short UTIL_String_Lock( locktab *var,         	// address of lock entry
                        u_char *str);           // locn of dest string
short UTIL_mvartolock( mvar *var, u_char *buf);	// convert mvar to string

// Share and semaphore stuff
int UTIL_Share(char *dbf);			// attach share and semaphores
short SemOp(int sem_num, int numb);             // Add/Remove semaphore
short LCK_Order(cstring *ent, u_char *buf, int dir);
short LCK_Get(cstring *ent, u_char *buf);
short LCK_Kill(cstring *ent);
void LCK_Remove(int job);
short LCK_Old(int count, cstring *list, int to);
short LCK_Add(int count, cstring *list, int to);
short LCK_Sub(int count, cstring *list);

// Xcalls
//
short Xcall_host ( char *ret_buffer, cstring *name, cstring *dum2 );
short Xcall_file ( char *ret_buffer, cstring *file, cstring *attr );
short Xcall_debug(char *ret_buffer, cstring *arg1, cstring *arg2);
short Xcall_wait(char *ret_buffer, cstring *arg1, cstring *arg2);
short Xcall_directory(char *ret_buffer, cstring *file, cstring *dummy);
short Xcall_errmsg(char *ret_buffer, cstring *err, cstring *dummy);
short Xcall_opcom(char *ret_buffer, cstring *msg, cstring *device);
short Xcall_signal(char *ret_buffer, cstring *pid, cstring *sig);
short Xcall_spawn(char *ret_buffer, cstring *cmd, cstring *dummy);
short Xcall_version(char *ret_buffer, cstring *name, cstring *dummy);
short Xcall_zwrite(char *ret_buffer, cstring *tmp, cstring *dummy);
short Xcall_e(char *ret_buffer, cstring *istr, cstring *STR_mask);
short Xcall_paschk(char *ret_buffer, cstring *user, cstring *pwd);
short Xcall_v(char *ret_buffer, cstring *lin, cstring *col);
short Xcall_x(char *ret_buffer, cstring *str, cstring *dummy);
short Xcall_xrsm(char *ret_buffer, cstring *str, cstring *dummy);
short Xcall_getenv(char *ret_buffer, cstring *env, cstring *dummy);
short Xcall_setenv(char *ret_buffer, cstring *env, cstring *value);
short Xcall_fork(char *ret_buffer, cstring *dum1, cstring *dum2);

#endif                                          // !_MUMPS_PROTO_H_
